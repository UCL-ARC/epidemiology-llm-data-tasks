"""CLI entry point for dataset comparison."""

import sys
from pathlib import Path

import pandas as pd
from loguru import logger

from . import DataComparator, aggregate_comparison_results, print_comparison_report


def main() -> None:
    """Run comparison on sample data directories."""
    logger.remove()
    logger.add(sys.stderr, level="INFO")

    results = []

    # TO DO: remove the hardcoding here
    for output_dir in Path("tmp/smolagent_context_devstral-small-2:24b-cloud").glob(
        "sample*/data/output"
    ):
        gt_file = output_dir / "output.csv"
        pred_file = output_dir / "cleaned_data.csv"

        if not gt_file.exists() or not pred_file.exists():
            logger.warning(f"Skipping {output_dir}: missing files")
            continue

        logger.info(f"\n\nProcessing: {output_dir}")

        gt_df = pd.read_csv(gt_file, index_col=0)
        pred_df = pd.read_csv(pred_file, index_col=0)

        comparator = DataComparator(
            categorical_threshold=20,
            match_threshold=0.8,
            data_match_threshold=0.7,
            categorical_data_match_threshold=0.98,
            numerical_data_match_threshold=0.0,
            categorical_match_threshold=0.8,
        )

        result, output_df = comparator.compare(gt_df, pred_df)
        print_comparison_report(result)

        # save individual output
        output_df.to_csv(output_dir / "comparison_output.csv")
        results.append((output_dir.parent.parent.name, result))

    # Generate and save aggregate summary
    if results:
        summary_df = aggregate_comparison_results(results)

        output_path = Path("tmp/smolagent_context/comparison_summary.csv")
        summary_df.to_csv(output_path, index=False)
        logger.info(f"\nSummary saved to: {output_path}")


if __name__ == "__main__":
    main()
