"""Utility to clean up incomplete smolagent outputs and report missing samples."""

import shutil
import sys
from pathlib import Path

from loguru import logger


def main(*, delete_data: bool = False) -> None:  # noqa: PLR0912
    """Identify incomplete smolagent outputs and optionally delete them."""
    context_root = Path("./tmp/smolagent_context")
    all_sample_ids = list(range(1, 21))

    if not context_root.exists():
        logger.warning(f"Context root does not exist: {context_root}")
        logger.info(f"samples_to_run = {all_sample_ids}")
        return

    # Map sample_id -> list of output dirs (there may be multiple from retries)
    sample_dirs: dict[int, list[Path]] = {sid: [] for sid in all_sample_ids}

    for p in context_root.iterdir():
        if not p.is_dir():
            continue
        name = p.name
        # Dirs look like "sample5_abc123"
        if not name.startswith("sample"):
            continue
        # Extract sample id between "sample" and "_"
        underscore_idx = name.find("_", 6)
        id_part = name[6:underscore_idx] if underscore_idx != -1 else name[6:]
        if id_part.isdigit():
            sample_dirs[int(id_part)].append(p)

    incomplete: list[int] = []
    complete: list[int] = []

    for sid in all_sample_ids:
        dirs = sorted(sample_dirs[sid], key=lambda p: p.stat().st_mtime)

        if not dirs:
            incomplete.append(sid)
            continue

        # Check if any dir has cleaned_data.csv in data/output/
        valid_dirs = [
            d for d in dirs if (d / "data" / "output" / "cleaned_data.csv").exists()
        ]

        if valid_dirs:
            if delete_data:
                # Keep the latest valid one, delete everything else
                keep = valid_dirs[-1]
                for d in dirs:
                    if d != keep:
                        logger.info(f"Deleting duplicate/stale dir: {d}")
                        shutil.rmtree(d)
            complete.append(sid)
        else:
            if delete_data:
                # No valid output — delete all dirs for this sample
                for d in dirs:
                    logger.info(f"Deleting incomplete dir (no cleaned_data.csv): {d}")
                    shutil.rmtree(d)
            else:
                for d in dirs:
                    logger.info(f"Would delete incomplete dir: {d}")
            incomplete.append(sid)

    logger.info(f"\nComplete samples ({len(complete)}): {complete}")
    logger.info(f"Incomplete/missing samples ({len(incomplete)}): {incomplete}")

    if not delete_data and incomplete:
        logger.info("Run with --delete to remove incomplete outputs.")

    logger.info(f"\nsamples_to_run = {incomplete}")


if __name__ == "__main__":
    delete = "--delete" in sys.argv
    main(delete_data=delete)
