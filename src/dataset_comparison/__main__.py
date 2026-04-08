"""CLI entry point for dataset comparison."""

import argparse
import json
import sys
from pathlib import Path

import pandas as pd
from loguru import logger

from . import DataComparator, aggregate_comparison_results, print_comparison_report


def get_arg_parser() -> argparse.ArgumentParser:
    """Create and return the argument parser."""
    parser = argparse.ArgumentParser(
        description="Run dataset comparison on sample data directories.",
    )
    parser.add_argument(
        "model",
        type=str,
        help="Model name suffix for the context directory "
        "(e.g. '_qwen3.5:9b_3' → tmp/smolagent_context_qwen3.5:9b_3).",
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
        default=Path("tmp"),
        help="Base directory containing experiment outputs (default: tmp).",
    )
    parser.add_argument(
        "--categorical-threshold",
        type=int,
        default=20,
        help="Max unique values to treat a column as categorical (default: 20).",
    )
    parser.add_argument(
        "--match-threshold",
        type=float,
        default=0.8,
        help="Column name match threshold (default: 0.8).",
    )
    parser.add_argument(
        "--data-match-threshold",
        type=float,
        default=0.7,
        help="Data match threshold (default: 0.7).",
    )
    parser.add_argument(
        "--categorical-data-match-threshold",
        type=float,
        default=0.95,
        help="Categorical data match threshold (default: 0.95).",
    )
    parser.add_argument(
        "--numerical-data-match-threshold",
        type=float,
        default=0.0,
        help="Numerical data match threshold (default: 0.0).",
    )
    parser.add_argument(
        "--categorical-match-threshold",
        type=float,
        default=0.8,
        help="Categorical column match threshold (default: 0.8).",
    )
    parser.add_argument(
        "--gt-filename",
        type=str,
        default="output.csv",
        help="Ground truth filename (default: output.csv).",
    )
    parser.add_argument(
        "--pred-filename",
        type=str,
        default="cleaned_data.csv",
        help="Predicted output filename (default: cleaned_data.csv).",
    )
    return parser


def main(argv: list[str] | None = None) -> None:
    """Run comparison on sample data directories."""
    logger.remove()

    parser = get_arg_parser()
    args = parser.parse_args(argv)

    log_level = "DEBUG" if args.verbose else "INFO"
    logger.add(sys.stderr, level=log_level)

    context_dir = args.base_dir / f"smolagent_context{args.model}"

    results = []

    for output_dir in context_dir.glob("sample*/data/output"):
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
            data_match_threshold=args.data_match_threshold,
            categorical_data_match_threshold=args.categorical_data_match_threshold,
            numerical_data_match_threshold=args.numerical_data_match_threshold,
            categorical_match_threshold=args.categorical_match_threshold,
        )

        result, output_df = comparator.compare(gt_df, pred_df)
        print_comparison_report(result)

        # save individual output
        output_df.to_csv(output_dir / "comparison_output.csv")

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
