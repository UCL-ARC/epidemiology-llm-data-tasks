"""
Script to rebuild experiment data directories in tmp/ by copying raw input data
and re-running R scripts (rtruth.R and rpred.R) for each task.

This recreates the data/input/ and data/output/ directories that are excluded
from version control via .gitignore.
"""

import argparse
import json
import re
import subprocess
from pathlib import Path

from logger.logger import Live, Spinner, console, logger


def get_arg_parser() -> argparse.ArgumentParser:
    """Create and return the argument parser."""
    parser = argparse.ArgumentParser(
        description="Rebuild experiment data directories in tmp/",
    )
    parser.add_argument(
        "-i",
        "--input_dir",
        type=Path,
        default=Path("data/input"),
        help="Path to the directory containing the raw .tab data files. "
        "(default: data/input)",
    )
    parser.add_argument(
        "-t",
        "--tmp_dir",
        type=Path,
        default=Path("tmp"),
        help="Path to the tmp directory containing experiment outputs. "
        "(default: tmp)",
    )
    parser.add_argument(
        "-m",
        "--model",
        type=str,
        default=None,
        help="Filter to model-run directories containing this substring "
        "(e.g. 'qwen3.5:9b' or 'qwen3.5:9b_2' to target a specific run).",
    )
    parser.add_argument(
        "-s",
        "--task",
        type=int,
        default=None,
        help="Filter to a specific task number (e.g. 4 for task4_*).",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Enable verbose output from R scripts.",
    )
    return parser


def load_metadata(metadata_path: Path) -> dict:
    """Load metadata from JSON file."""
    try:
        with Path.open(metadata_path) as f:
            metadata = json.load(f)
    except FileNotFoundError:
        logger.error(f"Metadata file not found: {metadata_path}")
        return {}
    except json.JSONDecodeError as e:
        logger.error(f"Invalid JSON in metadata {metadata_path}: {e}")
        return {}
    except Exception as e:  # noqa: BLE001
        logger.exception(f"Failed to load metadata: {e}")
        return {}
    return metadata


def copy_raw_data(input_dir: Path, task_input_dir: Path, metadata: dict) -> list:
    """
    Copy raw data files based on metadata keys.

    Returns a list of filenames that could not be copied.
    """
    failed_files = []
    Path.mkdir(task_input_dir, parents=True, exist_ok=True)
    for file_name in metadata:
        src_file = input_dir / file_name
        dest_file = task_input_dir / file_name
        if not src_file.exists():
            logger.error(
                f"Missing input file: [cyan]{file_name}[/cyan] "
                f"(not found in {input_dir})",
            )
            failed_files.append(file_name)
            continue
        try:
            with (
                Path.open(src_file, "rb") as src,
                Path.open(dest_file, "wb") as dest,
            ):
                dest.write(src.read())
        except Exception as e:  # noqa: BLE001
            logger.error(f"Error copying {file_name}: {e}")
            failed_files.append(file_name)
    return failed_files


def run_r_script(r_script_path: Path, *, verbose: bool) -> bool:
    """Run R script via subprocess."""
    if not r_script_path.exists():
        logger.warning(f"R script not found: {r_script_path}")
        return False
    try:
        result = subprocess.run(  # noqa: S603
            ["Rscript", r_script_path.name],  # noqa: S607
            cwd=r_script_path.parent,
            capture_output=True,
            text=True,
            check=False,
        )
        if verbose and result.stdout:
            logger.debug(f"R script output:\n{result.stdout}")
        if result.stderr:
            logger.warning(f"R script errors:\n{result.stderr}")
        if result.returncode == 0:
            return True
        logger.error(f"R script failed with exit code {result.returncode}")
    except Exception as e:  # noqa: BLE001
        logger.exception(f"Error running R script {r_script_path}: {e}")
    return False


def get_model_run_dirs(tmp_dir: Path, model_filter: str | None) -> list[Path]:
    """Discover model-run directories under tmp/, optionally filtered."""
    model_dirs = sorted(
        d
        for d in tmp_dir.iterdir()
        if d.is_dir() and d.name.startswith("smolagent_context_")
    )
    if model_filter:
        model_dirs = [d for d in model_dirs if model_filter in d.name]
    return model_dirs


def get_task_dirs(model_run_dir: Path, task_filter: int | None) -> list[Path]:
    """Get and sort task directories within a model-run directory."""
    task_dirs = [
        d
        for d in model_run_dir.iterdir()
        if d.is_dir() and re.match(r"task\d+", d.name)
    ]

    def extract_task_number(dir_path: Path) -> int:
        match = re.search(r"task(\d+)", dir_path.name)
        return int(match.group(1)) if match else 0

    task_dirs.sort(key=extract_task_number)

    if task_filter is not None:
        task_dirs = [d for d in task_dirs if extract_task_number(d) == task_filter]

    return task_dirs


def process_task(
    task_dir: Path,
    input_dir: Path,
    *,
    verbose: bool,
) -> bool:
    """
    Process a single task directory: copy inputs, run R scripts.

    Returns True if all steps succeeded.
    """
    # --- Load metadata ---
    with Live(
        Spinner("dots", text="Loading metadata..."),
        console=console,
        refresh_per_second=4,
    ):
        metadata = load_metadata(task_dir / "metadata.json")

    if not metadata:
        logger.error(f"Failed to load metadata for {task_dir.name}")
        return False

    # --- Copy raw input data ---
    with Live(
        Spinner("dots", text="Copying raw data files..."),
        console=console,
        refresh_per_second=4,
    ):
        task_input_dir = task_dir / "data" / "input"
        failed_files = copy_raw_data(input_dir, task_input_dir, metadata)

    if failed_files:
        logger.error(
            f"Missing {len(failed_files)} input file(s) for "
            f"{task_dir.name}: {', '.join(failed_files)}",
        )
        return False

    # --- Ensure output directory exists ---
    output_dir = task_dir / "data" / "output"
    Path.mkdir(output_dir, parents=True, exist_ok=True)

    # --- Run rtruth.R → cleaned_data.csv ---
    rtruth_path = task_dir / "rtruth.R"
    with Live(
        Spinner("dots", text="Running rtruth.R..."),
        console=console,
        refresh_per_second=4,
    ):
        rtruth_success = run_r_script(rtruth_path, verbose=verbose)

    if not rtruth_success:
        logger.error(f"rtruth.R failed for {task_dir.name}")
        return False

    # --- Run rpred.R → output.csv ---
    rpred_path = task_dir / "rpred.R"
    with Live(
        Spinner("dots", text="Running rpred.R..."),
        console=console,
        refresh_per_second=4,
    ):
        rpred_success = run_r_script(rpred_path, verbose=verbose)

    if not rpred_success:
        logger.warning(f"rpred.R failed for {task_dir.name} (LLM-generated script)")

    return True


def main() -> None:
    """Rebuild experiment data for each task in tmp/."""
    parser = get_arg_parser()
    args = parser.parse_args()

    input_dir: Path = args.input_dir
    tmp_dir: Path = args.tmp_dir
    model_filter: str | None = args.model
    task_filter: int | None = args.task
    verbose: bool = args.verbose

    logger.info(f"Rebuilding experiment data from: [cyan]{input_dir}[/cyan]")
    logger.info(f"Scanning experiments in: [cyan]{tmp_dir}[/cyan]")
    if model_filter:
        logger.info(f"Model filter: [yellow]{model_filter}[/yellow]")
    if task_filter is not None:
        logger.info(f"Task filter: [yellow]task{task_filter}[/yellow]")

    # --- Discover model-run directories ---
    model_dirs = get_model_run_dirs(tmp_dir, model_filter)
    if not model_dirs:
        logger.error("No model-run directories found.")
        return

    logger.info(f"Found [green]{len(model_dirs)}[/green] model-run directory(ies)")

    succeeded = []
    failed = []

    for model_dir in model_dirs:
        logger.info(f"\n{'='*60}")
        logger.info(f"Model run: [green]{model_dir.name}[/green]")
        logger.info(f"{'='*60}")

        task_dirs = get_task_dirs(model_dir, task_filter)
        if not task_dirs:
            logger.warning(f"No matching task directories in {model_dir.name}")
            continue

        for task_dir in task_dirs:
            logger.info(f"  Processing: [cyan]{task_dir.name}[/cyan]")
            success = process_task(task_dir, input_dir, verbose=verbose)
            if success:
                logger.success(f"  ✓ {task_dir.name}")
                succeeded.append(f"{model_dir.name}/{task_dir.name}")
            else:
                logger.error(f"  ✗ {task_dir.name}")
                failed.append(f"{model_dir.name}/{task_dir.name}")

    # --- Summary ---
    total = len(succeeded) + len(failed)
    logger.info(f"\n{'='*60}")
    logger.info("REBUILD SUMMARY")
    logger.info(f"{'='*60}")
    logger.info(f"Total tasks processed: {total}")
    logger.success(f"Succeeded: {len(succeeded)}")

    if failed:
        logger.error(f"Failed: {len(failed)}")
        for task in failed:
            logger.error(f"  - {task}")
    else:
        logger.success("\nAll tasks rebuilt successfully!")


if __name__ == "__main__":
    main()
