"""Script to initialise ground truth data for samples by copying raw data files and running R scripts."""

import argparse
import json
import pathlib
import re
import subprocess

from rich.console import Console
from rich.live import Live
from rich.spinner import Spinner

console = Console()


def get_arg_parser() -> argparse.ArgumentParser:
    """Create and return the argument parser."""
    parser = argparse.ArgumentParser(description="Ground Truth initialisation script")
    parser.add_argument(
        "-i",
        "--input_dir",
        type=pathlib.Path,
        required=True,
        help=(
            "Path to the directory containing the raw data."
            "This data will be used to generate the ground truth."
        ),
    )
    parser.add_argument(
        "-g",
        "--ground_truth_dir",
        type=pathlib.Path,
        help=("Path to the directory where the ground truth data will be stored."),
        default=pathlib.Path("ground_truth"),
    )

    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Enable verbose output",
    )
    return parser


def load_metadata(metadata_path: pathlib.Path) -> dict:
    """
    Load metadata from a JSON file.

    Args:
        metadata_path (pathlib.Path): Path to the metadata JSON file.
    returns:
        dict: Metadata dictionary if successful, or empty dict otherwise to ensure type consistency.

    note that if the metadata file is successfully loaded but empty, the subsequente processing will fail.

    """
    try:
        with pathlib.Path.open(metadata_path) as f:
            metadata = json.load(f)
    except FileNotFoundError:
        console.print("[bold red]Metadata file not found[/bold red]")
        return {}
    except json.JSONDecodeError as e:
        console.print(f"[bold red]Invalid JSON in metadata {e}[/bold red]")
        return {}
    except Exception as e:  # noqa: BLE001
        console.print(f"[bold red]Failed to load metadata for {e}[/bold red]")
        return {}

    return metadata


def copy_raw_data(
    input_dir: pathlib.Path, sample_input_dir: pathlib.Path, metadata: dict
) -> list:
    """
    Copy raw data files from input directory to sample input directory based on metadata.

    Args:
        input_dir (pathlib.Path): Path to the directory containing raw data files.
        sample_input_dir (pathlib.Path): Path to the sample's input directory where files will be copied.
        metadata (dict): Metadata dictionary containing file names to copy.
    returns:
        list: List of file names that failed to copy.

    """
    failed_files = []
    for file_name in metadata:
        src_file = input_dir / file_name
        dest_file = sample_input_dir / file_name

        try:
            pathlib.Path.mkdir(sample_input_dir, exist_ok=True)
            with (
                pathlib.Path.open(src_file, "rb") as src,
                pathlib.Path.open(dest_file, "wb") as dest,
            ):
                dest.write(src.read())
        except Exception as e:  # noqa: BLE001
            console.print(f"[bold red]Error copying {file_name}: {e}[/bold red]")
            failed_files.append(file_name)
    return failed_files


def run_r_script(r_script_path: pathlib.Path, *, verbose: bool) -> bool:
    """
    Run R script from its directory using subprocess.

    Args:
        r_script_path (pathlib.Path): Path to the R script to run.
        verbose (bool): Whether to print verbose output.
    returns:
        bool: True if successful, False if failed

    """
    try:
        # Get the directory containing the R script
        script_dir = r_script_path.parent

        # Run the R script with the script directory as working directory
        result = subprocess.run(  # noqa: S603
            ["Rscript", r_script_path.name],  # noqa: S607
            cwd=script_dir,  # Set working directory to script's directory
            capture_output=True,  # Capture stdout and stderr
            text=True,  # Return output as strings rather than bytes
            check=False,  # Don't raise exception on non-zero exit
        )

        # Print output for debugging
        if verbose and result.stdout:
            console.print("R script output:")
            console.print(result.stdout)

        if result.stderr:
            console.print("[bold red]R script errors:[/bold red]")
            console.print(result.stderr)

        # Check if R script succeeded
        if result.returncode == 0:
            return True

        console.print(
            f"[bold red]R script failed with exit code: {result.returncode}[/bold red]"
        )

    except Exception as e:  # noqa: BLE001
        console.print(
            f"[bold red]Error running R script {r_script_path}: {e}[/bold red]"
        )

    return False


def get_and_sort_sample_dirs(ground_truth_dir: pathlib.Path) -> list:
    """Get and sort sample directories in the ground truth directory."""
    sample_dirs = [
        d
        for d in pathlib.Path.iterdir(ground_truth_dir)
        if re.match(r"sample\d+", d.name)
    ]

    def extract_sample_number(dir_name: pathlib.Path) -> int:
        match = re.search(r"sample(\d+)", dir_name.name)
        return int(match.group(1)) if match else 0

    sample_dirs.sort(key=extract_sample_number)
    return sample_dirs


def main() -> None:
    """Initialise ground truth data for each sample."""
    parser = get_arg_parser()
    args = parser.parse_args()

    input_dir: pathlib.Path = args.input_dir
    ground_truth_dir: pathlib.Path = args.ground_truth_dir
    verbose: bool = args.verbose
    failed_samples = []

    console.print(
        f"[bold blue]Initialising ground truth using data from:[/bold blue] {input_dir}"
    )
    console.print(
        f"[bold blue]Ground truth data will be stored in:[/bold blue] {ground_truth_dir}"
    )

    # Get all sample directories and sort them
    sample_dirs = get_and_sort_sample_dirs(ground_truth_dir)

    for ground_truth_sample in sample_dirs:
        console.print(f"[bold green]Processing:[/bold green] {ground_truth_sample}")

        # Load metadata with spinner
        with Live(
            Spinner("dots", text="Loading metadata..."),
            console=console,
            refresh_per_second=4,
        ):
            metadata_path = ground_truth_dir / ground_truth_sample / "metadata.json"
            metadata = load_metadata(metadata_path)

        if not metadata:
            console.print(f"[bold red]Failed for {ground_truth_sample}[/bold red]")
            failed_samples.append(ground_truth_sample)
            continue

        # Copy over raw data files to sample input directory
        with Live(
            Spinner("dots", text="Copying raw data files..."),
            console=console,
            refresh_per_second=4,
        ):
            sample_input_dir = ground_truth_dir / ground_truth_sample / "data" / "input"
            failed_files = copy_raw_data(input_dir, sample_input_dir, metadata)

        if len(failed_files) > 0:
            console.print(f"[bold red]Failed for {ground_truth_sample}[/bold red]")
            failed_samples.append(ground_truth_sample)
            continue

        # Run R script from inside the sample directory
        with Live(
            Spinner("dots", text="Running R script..."),
            console=console,
            refresh_per_second=4,
        ):
            r_script_path = ground_truth_dir / ground_truth_sample / "rtruth.R"
            r_success = run_r_script(r_script_path, verbose=verbose)

        if not r_success:
            console.print(
                f"[bold red]R script failed for {ground_truth_sample}[/bold red]"
            )
            failed_samples.append(ground_truth_sample)
            continue

        console.print(
            f"[bold green]✓ Successfully processed {ground_truth_sample}[/bold green]"
        )

    # Final summary
    if len(failed_samples) > 0:
        console.print(
            "\n[bold red]The following samples failed during processing:[/bold red]"
        )
        for sample in failed_samples:
            console.print(f"[red]- {sample}[/red]")
    else:
        console.print("\n[bold green]All samples processed successfully![/bold green]")


if __name__ == "__main__":
    main()
