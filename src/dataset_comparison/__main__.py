"""CLI entry point for dataset comparison."""

import json
import sys
from pathlib import Path

import pandas as pd
from loguru import logger

from . import DataComparator, aggregate_comparison_results, print_comparison_report


def main() -> None:
    """Run comparison on sample data directories."""
    logger.remove()
    logger.add(sys.stderr, level="INFO")
    model_name = "_qwen3.5:9b_3"  # TO DO: remove hardcoding here

    results = []

    # TO DO: remove the hardcoding here
    for output_dir in Path(f"tmp/smolagent_context{model_name}").glob(
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
            categorical_data_match_threshold=0.95,
            numerical_data_match_threshold=0.0,
            categorical_match_threshold=0.8,
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

        output_path = Path(f"tmp/smolagent_context{model_name}/comparison_summary.csv")
        summary_df.to_csv(output_path, index=False)
        logger.info(f"\nSummary saved to: {output_path}")


if __name__ == "__main__":
    main()
