"""Utility to clean up incomplete smolagent outputs and report missing tasks."""

import shutil
import sys
from pathlib import Path

from loguru import logger


def main(*, delete_data: bool = False) -> None:  # noqa: PLR0912, C901
    """Identify incomplete smolagent outputs and optionally delete them."""
    context_root = Path("./tmp/smolagent_context")
    all_task_ids = list(range(1, 21))

    if not context_root.exists():
        logger.warning(f"Context root does not exist: {context_root}")
        logger.info(f"tasks_to_run = {all_task_ids}")
        return

    # Map task_id -> list of output dirs (there may be multiple from retries)
    task_dirs: dict[int, list[Path]] = {sid: [] for sid in all_task_ids}

    for p in context_root.iterdir():
        if not p.is_dir():
            continue
        name = p.name
        # Dirs look like "task5_abc123"
        if not name.startswith("task"):
            continue
        # Extract task id between "task" and "_"
        underscore_idx = name.find("_", 4)
        id_part = name[4:underscore_idx] if underscore_idx != -1 else name[4:]
        if id_part.isdigit():
            task_dirs[int(id_part)].append(p)

    incomplete_dirs: list[Path] = []
    complete_dirs: list[Path] = []
    missing_tasks: list[int] = []

    for sid in all_task_ids:
        dirs = sorted(task_dirs[sid], key=lambda p: p.stat().st_mtime)

        if not dirs:
            missing_tasks.append(sid)
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

    if missing_tasks:
        logger.info(f"\nMissing tasks (no dirs at all): {missing_tasks}")

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

    # Report which tasks still need running (missing or have no complete dirs)
    complete_task_ids = set()
    for d in complete_dirs:
        underscore_idx = d.name.find("_", 4)
        id_part = d.name[4:underscore_idx] if underscore_idx != -1 else d.name[4:]
        if id_part.isdigit():
            complete_task_ids.add(int(id_part))
    tasks_to_run = sorted(set(all_task_ids) - complete_task_ids)
    logger.info(f"\ntasks_to_run = {tasks_to_run}")


if __name__ == "__main__":
    delete = "--delete" in sys.argv
    main(delete_data=delete)
