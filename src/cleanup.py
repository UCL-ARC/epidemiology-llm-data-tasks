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

    incomplete_dirs: list[Path] = []
    complete_dirs: list[Path] = []
    missing_samples: list[int] = []

    for sid in all_sample_ids:
        dirs = sorted(sample_dirs[sid], key=lambda p: p.stat().st_mtime)

        if not dirs:
            missing_samples.append(sid)
            continue

        for d in dirs:
            if (d / "data" / "output" / "cleaned_data.csv").exists():
                complete_dirs.append(d)
            else:
                incomplete_dirs.append(d)

    # Report
    logger.info(f"\nComplete dirs ({len(complete_dirs)}):")
    for d in complete_dirs:
        logger.info(f"  ✓ {d}")

    if incomplete_dirs:
        logger.info(
            f"\nIncomplete dirs ({len(incomplete_dirs)}) — " f"no cleaned_data.csv:"
        )
        for d in incomplete_dirs:
            logger.info(f"  ✗ {d}")

    if missing_samples:
        logger.info(f"\nMissing samples (no dirs at all): {missing_samples}")

    # Delete incomplete dirs
    if incomplete_dirs:
        if delete_data:
            for d in incomplete_dirs:
                logger.info(f"Deleting incomplete dir: {d}")
                shutil.rmtree(d)
            logger.info(f"Deleted {len(incomplete_dirs)} incomplete dir(s).")
        else:
            logger.info(
                f"\nRun with --delete to remove {len(incomplete_dirs)} "
                f"incomplete dir(s).",
            )

    # Report which samples still need running (missing or have no complete dirs)
    complete_sample_ids = {int(d.name[6 : d.name.find("_", 6)]) for d in complete_dirs}
    samples_to_run = sorted(set(all_sample_ids) - complete_sample_ids)
    logger.info(f"\nsamples_to_run = {samples_to_run}")


if __name__ == "__main__":
    delete = "--delete" in sys.argv
    main(delete_data=delete)
