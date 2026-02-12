"""Tests for dataset_comparison __main__ module."""

from pathlib import Path
from unittest.mock import MagicMock, patch

import pandas as pd

from src.dataset_comparison.__main__ import main
from src.dataset_comparison.models import (
    ColumnComparison,
    ColumnMatch,
    ColumnType,
    DataComparisonResult,
    JoinCompleteness,
    MatchMethod,
    NumericComparison,
)


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
            completeness_score=1.0,
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

    @patch("src.dataset_comparison.__main__.print_comparison_report")
    @patch("src.dataset_comparison.__main__.aggregate_comparison_results")
    @patch("src.dataset_comparison.__main__.DataComparator")
    @patch("src.dataset_comparison.__main__.pd.read_csv")
    @patch("src.dataset_comparison.__main__.Path")
    def test_main_processes_samples(
        self,
        mock_path_cls: MagicMock,
        mock_read_csv: MagicMock,
        mock_comparator_cls: MagicMock,
        mock_aggregate: MagicMock,
        mock_print_report: MagicMock,
        tmp_path: Path,
    ) -> None:
        """Test main processes found sample directories."""
        # Set up output_dir mock
        gt_file = MagicMock(spec=Path)
        pred_file = MagicMock(spec=Path)
        comparison_output_file = MagicMock(spec=Path)
        gt_file.exists.return_value = True
        pred_file.exists.return_value = True

        output_dir = MagicMock(spec=Path)
        output_dir.__truediv__ = MagicMock(
            side_effect=[gt_file, pred_file, comparison_output_file]
        )
        output_dir.parent.parent.name = "sample1"

        # Configure Path("tmp/...").glob() to return our output_dir
        # and Path("tmp/.../comparison_summary.csv") for aggregate output
        mock_base_path = MagicMock()
        mock_base_path.glob.return_value = [output_dir]
        summary_path = MagicMock(spec=Path)
        mock_path_cls.side_effect = [mock_base_path, summary_path]

        # Mock CSV reading
        sample_df = pd.DataFrame({"a": [1, 2]}, index=pd.Index([0, 1], name="id"))
        mock_read_csv.return_value = sample_df

        # Mock comparator
        mock_comparator = MagicMock()
        mock_comparator_cls.return_value = mock_comparator
        result = _make_result()
        output_df = MagicMock(spec=pd.DataFrame)
        mock_comparator.compare.return_value = (result, output_df)

        # Mock aggregate
        mock_aggregate.return_value = MagicMock(spec=pd.DataFrame)

        main()

        mock_comparator.compare.assert_called_once()
        mock_print_report.assert_called_once_with(result)

    @patch("src.dataset_comparison.__main__.DataComparator")
    @patch("src.dataset_comparison.__main__.Path")
    def test_main_skips_missing_files(
        self,
        mock_path_cls: MagicMock,
        mock_comparator_cls: MagicMock,
    ) -> None:
        """Test main skips directories with missing files."""
        output_dir = MagicMock(spec=Path)
        gt_file = MagicMock(spec=Path)
        pred_file = MagicMock(spec=Path)
        gt_file.exists.return_value = True
        pred_file.exists.return_value = False  # Missing predicted file
        output_dir.__truediv__ = MagicMock(side_effect=[gt_file, pred_file])

        mock_base_path = MagicMock()
        mock_base_path.glob.return_value = [output_dir]
        mock_path_cls.return_value = mock_base_path

        main()

        mock_comparator_cls.return_value.compare.assert_not_called()

    @patch("src.dataset_comparison.__main__.DataComparator")
    @patch("src.dataset_comparison.__main__.Path")
    def test_main_no_samples_found(
        self,
        mock_path_cls: MagicMock,
        mock_comparator_cls: MagicMock,
    ) -> None:
        """Test main when no sample directories are found."""
        mock_base_path = MagicMock()
        mock_base_path.glob.return_value = []
        mock_path_cls.return_value = mock_base_path

        main()

        mock_comparator_cls.return_value.compare.assert_not_called()
