"""CLI entry point for dataset comparison."""

from pathlib import Path

import pandas as pd
from loguru import logger

from . import DataComparator, print_comparison_report


def main() -> None:
    """Run comparison on sample data directories."""
    for output_dir in Path("tmp/smolagent_context").glob("sample*/data/output"):
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
            match_threshold=0.5,
        )

        result = comparator.compare(gt_df, pred_df)
        print_comparison_report(result)


if __name__ == "__main__":
    main()
