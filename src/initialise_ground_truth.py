"""Script to initialise ground truth data for samples by copying raw data files and running R scripts."""

import argparse
import json
import re
import subprocess
from pathlib import Path

from logger.logger import Live, Spinner, console, logger


def get_arg_parser() -> argparse.ArgumentParser:
    """Create and return the argument parser."""
    parser = argparse.ArgumentParser(description="Ground Truth initialisation script")
    parser.add_argument(
        "-i",
        "--input_dir",
        type=Path,
        required=True,
        help="Path to the directory containing the raw data used for ground truth generation.",
    )
    parser.add_argument(
        "-g",
        "--ground_truth_dir",
        type=Path,
        help="Path to the directory where the ground truth data will be stored.",
        default=Path("ground_truth"),
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Enable verbose output",
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


def copy_raw_data(input_dir: Path, sample_input_dir: Path, metadata: dict) -> list:
    """Copy raw data files based on metadata."""
    failed_files = []
    Path.mkdir(sample_input_dir, exist_ok=True)
    for file_name in metadata:
        src_file = input_dir / file_name
        dest_file = sample_input_dir / file_name

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


def get_and_sort_sample_dirs(ground_truth_dir: Path) -> list:
    """Get and sort sample directories in the ground truth directory."""
    sample_dirs = [
        d for d in Path.iterdir(ground_truth_dir) if re.match(r"sample\d+", d.name)
    ]

    def extract_sample_number(dir_name: Path) -> int:
        match = re.search(r"sample(\d+)", dir_name.name)
        return int(match.group(1)) if match else 0

    sample_dirs.sort(key=extract_sample_number)
    return sample_dirs


def main() -> None:
    """Initialise ground truth data for each sample."""
    parser = get_arg_parser()
    args = parser.parse_args()

    input_dir: Path = args.input_dir
    ground_truth_dir: Path = args.ground_truth_dir
    verbose: bool = args.verbose
    failed_samples = []

    logger.info(f"Initialising ground truth using data from: [cyan]{input_dir}[/cyan]")
    logger.info(f"Ground truth data will be stored in: [cyan]{ground_truth_dir}[/cyan]")

    sample_dirs = get_and_sort_sample_dirs(ground_truth_dir)

    for ground_truth_sample in sample_dirs:
        logger.info(f"Processing: [green]{ground_truth_sample}[/green]")

        # --- Load metadata ---
        with Live(
            Spinner("dots", text="Loading metadata..."),
            console=console,
            refresh_per_second=4,
        ):
            metadata_path = ground_truth_sample / "metadata.json"
            metadata = load_metadata(metadata_path)

        if not metadata:
            logger.error(f"Failed to load metadata for {ground_truth_sample}")
            failed_samples.append(ground_truth_sample)
            continue

        # --- Copy raw data ---
        with Live(
            Spinner("dots", text="Copying raw data files..."),
            console=console,
            refresh_per_second=4,
        ):
            sample_input_dir = ground_truth_sample / "data" / "input"
            failed_files = copy_raw_data(input_dir, sample_input_dir, metadata)

        if failed_files:
            logger.error(f"Failed to copy files for {ground_truth_sample}")
            failed_samples.append(ground_truth_sample)
            continue

        # --- Run R script ---
        with Live(
            Spinner("dots", text="Running R script..."),
            console=console,
            refresh_per_second=4,
        ):
            r_script_path = ground_truth_sample / "rtruth.R"
            r_success = run_r_script(r_script_path, verbose=verbose)

        if not r_success:
            logger.error(f"R script failed for {ground_truth_sample}")
            failed_samples.append(ground_truth_sample)
            continue

        logger.success(f"✓ Successfully processed {ground_truth_sample}")

    # --- Summary ---
    if failed_samples:
        logger.error("\nThe following samples failed during processing:")
        for sample in failed_samples:
            logger.error(f" - {sample}")
    else:
        logger.success("\nAll samples processed successfully!")


if __name__ == "__main__":
    main()
