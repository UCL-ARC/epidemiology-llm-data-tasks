"""Script to run dataset comparison on experiment output directories."""

import argparse
import json
import re
import sys
from pathlib import Path

import pandas as pd
from loguru import logger

from src.config import (
    CATEGORICAL_DATA_MATCH_THRESHOLD,
    CATEGORICAL_MATCH_THRESHOLD,
    CATEGORICAL_THRESHOLD,
    COLUMN_DATA_MATCH_THRESHOLD,
    GT_FILENAME,
    MATCH_THRESHOLD,
    NUMERICAL_DATA_MATCH_THRESHOLD,
    PRED_FILENAME,
    SMOLAGENT_CONTEXT_PREFIX,
    TMP_DIR,
)
from src.tabmatch import (
    DataComparator,
    aggregate_comparison_results,
    build_category_mapping_table,
    build_column_mapping_table,
    print_comparison_report,
)


def get_arg_parser() -> argparse.ArgumentParser:
    """Create and return the argument parser."""
    parser = argparse.ArgumentParser(
        description="Run dataset comparison on task data directories.",
    )
    parser.add_argument(
        "model",
        nargs="?",
        type=str,
        default=None,
        help="Model name suffix for the context directory "
        "(e.g. '_qwen3.5:9b_3' → tmp/smolagent_context_qwen3.5:9b_3).",
    )
    parser.add_argument(
        "--all",
        action="store_true",
        dest="run_all",
        help="Run comparison for all smolagent_context_* directories in the base dir.",
    )
    parser.add_argument(
        "-v",
        "--verbose",
        action="store_true",
        help="Enable debug logging.",
    )
    parser.add_argument(
        "--base-dir",
        type=Path,
        default=TMP_DIR,
        help=f"Base directory containing experiment outputs (default: {TMP_DIR}).",
    )
    parser.add_argument(
        "--categorical-threshold",
        type=int,
        default=CATEGORICAL_THRESHOLD,
        help=f"Max unique values to treat a column as categorical "
        f"(default: {CATEGORICAL_THRESHOLD}).",
    )
    parser.add_argument(
        "--match-threshold",
        type=float,
        default=MATCH_THRESHOLD,
        help="Column name match threshold (default: 0.8).",
    )
    parser.add_argument(
        "--column-data-match-threshold",
        type=float,
        default=COLUMN_DATA_MATCH_THRESHOLD,
        help=f"Column data match threshold "
        f"(default: {COLUMN_DATA_MATCH_THRESHOLD}).",
    )
    parser.add_argument(
        "--categorical-data-match-threshold",
        type=float,
        default=CATEGORICAL_DATA_MATCH_THRESHOLD,
        help=f"Categorical data match threshold "
        f"(default: {CATEGORICAL_DATA_MATCH_THRESHOLD}).",
    )
    parser.add_argument(
        "--numerical-data-match-threshold",
        type=float,
        default=NUMERICAL_DATA_MATCH_THRESHOLD,
        help=f"Numerical data match threshold "
        f"(default: {NUMERICAL_DATA_MATCH_THRESHOLD}).",
    )
    parser.add_argument(
        "--categorical-match-threshold",
        type=float,
        default=CATEGORICAL_MATCH_THRESHOLD,
        help=f"Categorical column match threshold "
        f"(default: {CATEGORICAL_MATCH_THRESHOLD}).",
    )
    parser.add_argument(
        "--gt-filename",
        type=str,
        default=GT_FILENAME,
        help=f"Ground truth filename (default: {GT_FILENAME}).",
    )
    parser.add_argument(
        "--pred-filename",
        type=str,
        default=PRED_FILENAME,
        help=f"Predicted output filename (default: {PRED_FILENAME}).",
    )
    parser.add_argument(
        "--gt-null-list",
        type=Path,
        default=Path("exploration/data/all_null_value_summary.csv"),
        help=(
            "CSV file containing a 'null_value' column used to mask ground truth nulls "
            "during per-column comparison. When a GT value "
            "matches an entry in this list, "
            "both the GT and paired prediction values are set to NaN for that row. "
            "(default: exploration/data/all_null_value_summary.csv)."
        ),
    )
    return parser


def normalize_null_token(value: object) -> str:
    """Normalize tokens so data values can be matched to null-list entries."""
    if pd.isna(value):
        return "<na>"

    text = str(value).strip().replace("\u2019", "'")
    text = re.sub(r"\s+", " ", text)

    # Normalize simple float-int formatting differences (e.g. -1.0 -> -1).
    if re.fullmatch(r"-?\d+\.0", text):
        text = text[:-2]

    return text.lower()


def load_gt_null_values(null_list_path: Path) -> set[str]:
    """Load normalized null values from the generated null-summary CSV."""
    if not null_list_path.exists():
        logger.warning(f"GT null list not found: {null_list_path}")
        return set()

    null_df = pd.read_csv(null_list_path)
    if "null_value" not in null_df.columns:
        logger.warning(
            f"GT null list missing 'null_value' column: {null_list_path}",
        )
        return set()

    null_values = {
        normalize_null_token(v) for v in null_df["null_value"].dropna().tolist()
    }
    logger.info(f"Loaded {len(null_values)} GT null values from {null_list_path}")
    return null_values


def main(argv: list[str] | None = None) -> None:  # noqa: PLR0915
    """Run comparison on task data directories."""
    logger.remove()

    parser = get_arg_parser()
    args = parser.parse_args(argv)

    log_level = "DEBUG" if args.verbose else "INFO"
    logger.add(sys.stderr, level=log_level)

    gt_null_values = load_gt_null_values(args.gt_null_list)

    if args.run_all:
        context_dirs = sorted(
            p
            for p in args.base_dir.iterdir()
            if p.is_dir() and p.name.startswith(SMOLAGENT_CONTEXT_PREFIX)
        )
        if not context_dirs:
            logger.error(f"No smolagent_context_* directories found in {args.base_dir}")
            sys.exit(1)
    elif args.model is not None:
        context_dirs = [args.base_dir / f"smolagent_context{args.model}"]
    else:
        context_dirs = [args.base_dir / "smolagent_context"]

    for context_dir in context_dirs:
        if not context_dir.exists():
            logger.warning(f"Skipping missing directory: {context_dir}")
            continue

        logger.info(f"\n{'='*60}")
        logger.info(f"Processing context: {context_dir.name}")
        logger.info(f"{'='*60}")

        results = []

        for output_dir in context_dir.glob("task*/data/output"):
            gt_file = output_dir / args.gt_filename
            pred_file = output_dir / args.pred_filename

            if not gt_file.exists() or not pred_file.exists():
                logger.warning(f"Skipping {output_dir}: missing files")
                continue

            logger.info(f"\n\nProcessing: {output_dir}")

            gt_df = pd.read_csv(gt_file, index_col=0)
            pred_df = pd.read_csv(pred_file, index_col=0)

            comparator = DataComparator(
                categorical_threshold=args.categorical_threshold,
                match_threshold=args.match_threshold,
                column_data_match_threshold=args.column_data_match_threshold,
                categorical_data_match_threshold=args.categorical_data_match_threshold,
                numerical_data_match_threshold=args.numerical_data_match_threshold,
                categorical_match_threshold=args.categorical_match_threshold,
                gt_null_values=gt_null_values,
            )

            result, output_df = comparator.compare(gt_df, pred_df)
            print_comparison_report(result)

            # save individual output
            output_df.to_csv(output_dir / "comparison_output.csv")

            # save column and category mapping tables
            col_map_df = build_column_mapping_table(result)
            col_map_df.to_csv(output_dir / "column_mapping.csv", index=False)

            cat_map_df = build_category_mapping_table(result)
            if len(cat_map_df) != 0:
                cat_map_df.to_csv(output_dir / "category_mapping.csv", index=False)

            # Load runtime metadata (tokens, steps, time)
            runtime_file = output_dir.parent.parent / "runtime_data.json"
            runtime_data = None
            if runtime_file.exists():
                with Path.open(runtime_file) as f:
                    runtime_data = json.load(f)
            else:
                logger.warning(f"No runtime_data.json found in {runtime_file.parent}")

            results.append((output_dir.parent.parent.name, result, runtime_data))

        # Generate and save aggregate summary
        if results:
            summary_df = aggregate_comparison_results(results)

            output_path = context_dir / "comparison_summary.csv"
            summary_df.to_csv(output_path, index=False)
            logger.info(f"\nSummary saved to: {output_path}")


if __name__ == "__main__":
    main()
