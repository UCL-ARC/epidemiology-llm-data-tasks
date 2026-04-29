"""Tests for dataset_comparison __main__ module."""

from pathlib import Path
from unittest.mock import MagicMock, patch

import pandas as pd

from src.tabmatch.__main__ import main
from src.tabmatch.models import (
    ColumnComparison,
    ColumnMatch,
    ColumnType,
    DataComparisonResult,
    JoinCompleteness,
    MatchMethod,
    NumericComparison,
)

_MODEL_ARG = "_test_model_1"


def _make_result() -> DataComparisonResult:
    """Create a minimal DataComparisonResult for testing."""
    return DataComparisonResult(
        primary_key="id | id",
        join_completeness=JoinCompleteness(
            gt_row_count=10,
            pred_row_count=10,
            joined_row_count=10,
            missing_in_pred=0,
            extra_in_pred=0,
            join_completeness_score=1.0,
            gt_duplicate_keys=0,
            pred_duplicate_keys=0,
        ),
        column_matches=[ColumnMatch("a", "a", 1.0, MatchMethod.LEVENSHTEIN)],
        column_comparisons=[
            ColumnComparison(
                gt_column="a",
                pred_column="a",
                column_type=ColumnType.NUMERIC,
                numeric_comparison=NumericComparison(
                    rmse=0.0,
                    mae=0.0,
                    nrmse=0.0,
                    nmae=0.0,
                    correlation=1.0,
                    gt_mean=5.0,
                    pred_mean=5.0,
                    gt_std=2.0,
                    pred_std=2.0,
                    data_match=True,
                ),
            ),
        ],
        task_completion_percentage=100.0,
    )


class TestMain:
    """Tests for the main() CLI entry point."""

    @patch("src.dataset_comparison.__main__.json.load")
    @patch("src.dataset_comparison.__main__.print_comparison_report")
    @patch("src.dataset_comparison.__main__.aggregate_comparison_results")
    @patch("src.dataset_comparison.__main__.DataComparator")
    @patch("src.dataset_comparison.__main__.pd.read_csv")
    def test_main_processes_tasks(
        self,
        mock_read_csv: MagicMock,
        mock_comparator_cls: MagicMock,
        mock_aggregate: MagicMock,
        mock_print_report: MagicMock,
        mock_json_load: MagicMock,
        tmp_path: Path,
    ) -> None:
        """Test main processes found task directories."""
        # Create real directory structure
        output_dir = (
            tmp_path / f"smolagent_context{_MODEL_ARG}" / "task1" / "data" / "output"
        )
        output_dir.mkdir(parents=True)
        (output_dir / "output.csv").write_text("id,a\n0,1\n1,2\n")
        (output_dir / "cleaned_data.csv").write_text("id,a\n0,1\n1,2\n")

        # Create runtime_data.json
        runtime_dir = output_dir.parent.parent
        (runtime_dir / "runtime_data.json").write_text(
            '{"token_usage": 100, "steps": 3, "time_taken": 1.5}'
        )

        # Mock CSV reading
        task_df = pd.DataFrame({"a": [1, 2]}, index=pd.Index([0, 1], name="id"))
        mock_read_csv.return_value = task_df

        # Mock comparator
        mock_comparator = MagicMock()
        mock_comparator_cls.return_value = mock_comparator
        result = _make_result()
        output_df = MagicMock(spec=pd.DataFrame)
        mock_comparator.compare.return_value = (result, output_df)

        # Mock aggregate
        mock_aggregate.return_value = MagicMock(spec=pd.DataFrame)

        main([_MODEL_ARG, "--base-dir", str(tmp_path)])

        mock_comparator.compare.assert_called_once()
        mock_print_report.assert_called_once_with(result)

    @patch("src.dataset_comparison.__main__.DataComparator")
    def test_main_skips_missing_files(
        self,
        mock_comparator_cls: MagicMock,
        tmp_path: Path,
    ) -> None:
        """Test main skips directories with missing files."""
        output_dir = (
            tmp_path / f"smolagent_context{_MODEL_ARG}" / "task1" / "data" / "output"
        )
        output_dir.mkdir(parents=True)
        # Only create gt file, not pred file
        (output_dir / "output.csv").write_text("id,a\n0,1\n")

        main([_MODEL_ARG, "--base-dir", str(tmp_path)])

        mock_comparator_cls.return_value.compare.assert_not_called()

    @patch("src.dataset_comparison.__main__.DataComparator")
    def test_main_no_tasks_found(
        self,
        mock_comparator_cls: MagicMock,
        tmp_path: Path,
    ) -> None:
        """Test main when no task directories are found."""
        context_dir = tmp_path / f"smolagent_context{_MODEL_ARG}"
        context_dir.mkdir(parents=True)

        main([_MODEL_ARG, "--base-dir", str(tmp_path)])

        mock_comparator_cls.return_value.compare.assert_not_called()
