"""Tests for dataset_comparison report module."""

from dataclasses import replace

import pandas as pd
import pytest

from src.dataset_comparison.models import (
    CategoricalComparison,
    ColumnComparison,
    ColumnMatch,
    ColumnType,
    DataComparisonResult,
    JoinCompleteness,
    MatchMethod,
    NumericComparison,
)
from src.dataset_comparison.report import (
    aggregate_comparison_results,
    build_category_mapping_table,
    build_column_mapping_table,
    print_comparison_report,
)

_DEFAULT_JOIN_COMPLETENESS = JoinCompleteness(
    gt_row_count=100,
    pred_row_count=100,
    joined_row_count=100,
    missing_in_pred=0,
    extra_in_pred=0,
    join_completeness_score=1.0,
    gt_duplicate_keys=0,
    pred_duplicate_keys=0,
)

_DEFAULT_NUMERIC_COMPARISON = NumericComparison(
    rmse=0.0,
    mae=0.0,
    nrmse=0.0,
    nmae=0.0,
    correlation=1.0,
    gt_mean=50.0,
    pred_mean=50.0,
    gt_std=10.0,
    pred_std=10.0,
    data_match=True,
)

_DEFAULT_CATEGORICAL_COMPARISON = CategoricalComparison(
    exact_match_rate=1.0,
    gt_categories={"a", "b", "c"},
    pred_categories={"a", "b", "c"},
    missing_categories=set(),
    extra_categories=set(),
    category_overlap_score=1.0,
    distribution_similarity=1.0,
    data_match=True,
)

_DEFAULT_RESULT = DataComparisonResult(
    primary_key="id | id",
    join_completeness=_DEFAULT_JOIN_COMPLETENESS,
    column_matches=[
        ColumnMatch("age", "age", 1.0, MatchMethod.LEVENSHTEIN),
    ],
    column_comparisons=[
        ColumnComparison(
            gt_column="age",
            pred_column="age",
            column_type=ColumnType.NUMERIC,
            numeric_comparison=_DEFAULT_NUMERIC_COMPARISON,
        ),
    ],
    unmatched_gt_columns=[],
    unmatched_pred_columns=[],
    task_completion_percentage=100.0,
)


class TestPrintComparisonReport:
    """Tests for print_comparison_report function."""

    def test_prints_without_error_all_matched_numeric(
        self, capsys: pytest.CaptureFixture
    ) -> None:
        """Test report prints for all-matched numeric columns."""
        print_comparison_report(_DEFAULT_RESULT)
        captured = capsys.readouterr()
        assert "DATA COMPARISON REPORT" in captured.out
        assert "id | id" in captured.out

    def test_prints_with_categorical_columns(
        self, capsys: pytest.CaptureFixture
    ) -> None:
        """Test report with categorical comparisons."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("name", "name", 0.9, MatchMethod.SEMANTIC),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="name",
                    pred_column="name",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=_DEFAULT_CATEGORICAL_COMPARISON,
                ),
            ],
        )
        print_comparison_report(result)
        captured = capsys.readouterr()
        assert "DATA COMPARISON REPORT" in captured.out

    def test_prints_with_unmatched_columns(self, capsys: pytest.CaptureFixture) -> None:
        """Test report with unmatched columns."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("age", "age", 1.0, MatchMethod.LEVENSHTEIN),
                ColumnMatch("extra_col", None, 0.1, None),
            ],
            unmatched_gt_columns=["extra_col"],
            unmatched_pred_columns=["unknown_col"],
        )
        print_comparison_report(result)
        captured = capsys.readouterr()
        assert "Unmatched GT Columns" in captured.out
        assert "Unmatched Pred Columns" in captured.out

    def test_prints_with_duplicates(self, capsys: pytest.CaptureFixture) -> None:
        """Test report with duplicate keys flagged."""
        result = replace(
            _DEFAULT_RESULT,
            join_completeness=replace(
                _DEFAULT_JOIN_COMPLETENESS,
                gt_duplicate_keys=2,
                pred_duplicate_keys=1,
            ),
        )
        print_comparison_report(result)
        captured = capsys.readouterr()
        assert "Duplicate keys detected" in captured.out

    def test_prints_with_missing_in_pred(self, capsys: pytest.CaptureFixture) -> None:
        """Test report with missing rows in pred."""
        result = replace(
            _DEFAULT_RESULT,
            join_completeness=replace(
                _DEFAULT_JOIN_COMPLETENESS,
                missing_in_pred=10,
                extra_in_pred=5,
                join_completeness_score=0.9,
            ),
        )
        print_comparison_report(result)
        captured = capsys.readouterr()
        assert "DATA COMPARISON REPORT" in captured.out

    def test_prints_with_low_match_score(self, capsys: pytest.CaptureFixture) -> None:
        """Test report with low column match scores."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("age", "years", 0.3, MatchMethod.LEVENSHTEIN),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="age",
                    pred_column="years",
                    column_type=ColumnType.NUMERIC,
                    numeric_comparison=replace(
                        _DEFAULT_NUMERIC_COMPARISON,
                        data_match=False,
                        correlation=None,
                    ),
                ),
            ],
        )
        print_comparison_report(result)
        captured = capsys.readouterr()
        assert "DATA COMPARISON REPORT" in captured.out

    def test_prints_with_medium_match_score(
        self, capsys: pytest.CaptureFixture
    ) -> None:
        """Test report with medium column match scores."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("age", "years", 0.6, MatchMethod.SEMANTIC),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="age",
                    pred_column="years",
                    column_type=ColumnType.NUMERIC,
                    numeric_comparison=replace(
                        _DEFAULT_NUMERIC_COMPARISON,
                        data_match=None,
                    ),
                ),
            ],
        )
        print_comparison_report(result)
        captured = capsys.readouterr()
        assert "DATA COMPARISON REPORT" in captured.out

    def test_prints_categorical_low_match(self, capsys: pytest.CaptureFixture) -> None:
        """Test report with low categorical exact match rate."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("name", "name", 1.0, MatchMethod.LEVENSHTEIN),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="name",
                    pred_column="name",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=replace(
                        _DEFAULT_CATEGORICAL_COMPARISON,
                        exact_match_rate=0.5,
                        data_match=False,
                    ),
                ),
            ],
        )
        print_comparison_report(result)
        captured = capsys.readouterr()
        assert "DATA COMPARISON REPORT" in captured.out

    def test_prints_categorical_medium_match(
        self, capsys: pytest.CaptureFixture
    ) -> None:
        """Test report with medium categorical exact match rate."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("name", "name", 1.0, MatchMethod.LEVENSHTEIN),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="name",
                    pred_column="name",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=replace(
                        _DEFAULT_CATEGORICAL_COMPARISON,
                        exact_match_rate=0.75,
                        data_match=None,
                    ),
                ),
            ],
        )
        print_comparison_report(result)
        captured = capsys.readouterr()
        assert "DATA COMPARISON REPORT" in captured.out

    def test_prints_end_report(self, capsys: pytest.CaptureFixture) -> None:
        """Test that END REPORT marker is printed."""
        print_comparison_report(_DEFAULT_RESULT)
        captured = capsys.readouterr()
        assert "END REPORT" in captured.out


class TestAggregateComparisonResults:
    """Tests for aggregate_comparison_results function."""

    def test_empty_results(self) -> None:
        """Test aggregation with empty results list."""
        summary_df = aggregate_comparison_results([])
        assert isinstance(summary_df, pd.DataFrame)
        assert len(summary_df) == 0

    def test_single_result_numeric(self) -> None:
        """Test aggregation with a single numeric-only result."""
        summary_df = aggregate_comparison_results([("sample1", _DEFAULT_RESULT, None)])
        assert len(summary_df) == 2  # 1 sample + 1 aggregate row
        assert summary_df.iloc[0]["sample"] == "sample1"
        assert summary_df.iloc[1]["sample"] == "AGGREGATE"
        assert summary_df.iloc[0]["join_completeness"] == 1.0
        assert summary_df.iloc[0]["matched_cols"] == 1
        assert summary_df.iloc[0]["unmatched_gt_cols"] == 0

    def test_single_result_categorical(self) -> None:
        """Test aggregation with a categorical-only result."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("name", "name", 1.0, MatchMethod.LEVENSHTEIN),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="name",
                    pred_column="name",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=_DEFAULT_CATEGORICAL_COMPARISON,
                ),
            ],
        )
        summary_df = aggregate_comparison_results([("sample1", result, None)])
        assert len(summary_df) == 2
        assert summary_df.iloc[0]["matched_categorical_cols"] == 1
        assert summary_df.iloc[0]["matched_numeric_cols"] == 0

    def test_multiple_results(self) -> None:
        """Test aggregation with multiple results."""
        result2 = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("name", "name", 0.9, MatchMethod.SEMANTIC),
                ColumnMatch("missing_col", None, 0.1, None),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="name",
                    pred_column="name",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=replace(
                        _DEFAULT_CATEGORICAL_COMPARISON,
                        data_match=False,
                    ),
                ),
            ],
            unmatched_gt_columns=["missing_col"],
            task_completion_percentage=50.0,
        )
        summary_df = aggregate_comparison_results(
            [("sample1", _DEFAULT_RESULT, None), ("sample2", result2, None)]
        )
        assert len(summary_df) == 3  # 2 samples + aggregate
        agg = summary_df[summary_df["sample"] == "AGGREGATE"].iloc[0]
        assert agg["matched_cols"] == 2
        assert agg["unmatched_gt_cols"] == 1

    def test_correctness_completeness_yield_computed(self) -> None:
        """Test that correctness, completeness, output_yield are computed correctly."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("age", "age", 1.0, MatchMethod.LEVENSHTEIN),
                ColumnMatch("missing", None, 0.1, None),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="age",
                    pred_column="age",
                    column_type=ColumnType.NUMERIC,
                    numeric_comparison=replace(
                        _DEFAULT_NUMERIC_COMPARISON,
                        data_match=True,
                    ),
                ),
            ],
            unmatched_gt_columns=["missing"],
        )
        summary_df = aggregate_comparison_results([("sample1", result, None)])
        row = summary_df.iloc[0]
        assert row["true_positives"] == 1
        assert row["false_positives"] == 0
        assert row["false_negatives"] == 1
        assert row["correctness"] == pytest.approx(1.0)
        assert row["completeness"] == pytest.approx(0.5)

    def test_correctness_with_false_positives(self) -> None:
        """Test correctness when there are false positives."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("age", "years", 0.8, MatchMethod.LEVENSHTEIN),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="age",
                    pred_column="years",
                    column_type=ColumnType.NUMERIC,
                    numeric_comparison=replace(
                        _DEFAULT_NUMERIC_COMPARISON,
                        data_match=False,
                    ),
                ),
            ],
        )
        summary_df = aggregate_comparison_results([("sample1", result, None)])
        row = summary_df.iloc[0]
        assert row["true_positives"] == 0
        assert row["false_positives"] == 1
        assert row["correctness"] == pytest.approx(0.0)

    def test_aggregate_correctness_completeness_micro(self) -> None:
        """Test aggregate row uses micro-averaged correctness/completeness."""
        result2 = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("val", "val", 1.0, MatchMethod.LEVENSHTEIN),
                ColumnMatch("extra", None, 0.1, None),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="val",
                    pred_column="val",
                    column_type=ColumnType.NUMERIC,
                    numeric_comparison=replace(
                        _DEFAULT_NUMERIC_COMPARISON,
                        data_match=True,
                    ),
                ),
            ],
            unmatched_gt_columns=["extra"],
        )
        summary_df = aggregate_comparison_results(
            [("s1", _DEFAULT_RESULT, None), ("s2", result2, None)]
        )
        agg = summary_df[summary_df["sample"] == "AGGREGATE"].iloc[0]
        # Total TP=2, FP=0, FN=1
        assert agg["true_positives"] == 2
        assert agg["false_negatives"] == 1
        assert agg["correctness"] == pytest.approx(1.0)
        assert agg["completeness"] == pytest.approx(2.0 / 3.0)

    def test_no_matches_yields_none_metrics(self) -> None:
        """Test that no TP+FP yields None correctness."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("col", None, 0.1, None),
            ],
            column_comparisons=[],
            unmatched_gt_columns=["col"],
        )
        summary_df = aggregate_comparison_results([("sample1", result, None)])
        row = summary_df.iloc[0]
        assert row["correctness"] is None
        assert row["completeness"] == pytest.approx(0.0)
        assert row["output_yield"] is None

    def test_aggregate_with_none_numeric_metrics(self) -> None:
        """Test aggregate handles samples with no numeric columns."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("name", "name", 1.0, MatchMethod.LEVENSHTEIN),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="name",
                    pred_column="name",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=_DEFAULT_CATEGORICAL_COMPARISON,
                ),
            ],
        )
        summary_df = aggregate_comparison_results([("sample1", result, None)])
        row = summary_df.iloc[0]
        assert row["avg_numeric_rmse"] is None
        assert row["avg_numeric_corr"] is None
        assert row["avg_categorical_exact_match"] == pytest.approx(1.0)

    def test_task_completion_fields(self) -> None:
        """Test task completion percentage and task_complete flag."""
        summary_df = aggregate_comparison_results([("sample1", _DEFAULT_RESULT, None)])
        row = summary_df.iloc[0]
        assert row["task_completion_percentage"] == 100.0
        assert row["task_complete"] is True

    def test_task_not_complete(self) -> None:
        """Test task_complete is False when not 100%."""
        result = replace(
            _DEFAULT_RESULT,
            task_completion_percentage=75.0,
        )
        summary_df = aggregate_comparison_results([("sample1", result, None)])
        row = summary_df.iloc[0]
        assert row["task_complete"] is False

    def test_aggregate_row_task_metrics(self) -> None:
        """Test aggregate row has avg task completion and count."""
        result2 = replace(
            _DEFAULT_RESULT,
            task_completion_percentage=50.0,
        )
        summary_df = aggregate_comparison_results(
            [("s1", _DEFAULT_RESULT, None), ("s2", result2, None)]
        )
        agg = summary_df[summary_df["sample"] == "AGGREGATE"].iloc[0]
        assert agg["avg_task_completion_percentage"] == pytest.approx(75.0)
        assert agg["number_task_complete"] == 1

    def test_categorical_data_match_true_positive(self) -> None:
        """Test that categorical data_match=True counts as TP."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("name", "name", 1.0, MatchMethod.LEVENSHTEIN),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="name",
                    pred_column="name",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=replace(
                        _DEFAULT_CATEGORICAL_COMPARISON,
                        data_match=True,
                    ),
                ),
            ],
        )
        summary_df = aggregate_comparison_results([("sample1", result, None)])
        row = summary_df.iloc[0]
        assert row["true_positives"] == 1

    def test_categorical_data_match_false_positive(self) -> None:
        """Test that categorical data_match=False counts as FP."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("name", "name", 1.0, MatchMethod.LEVENSHTEIN),
            ],
            column_comparisons=[
                ColumnComparison(
                    gt_column="name",
                    pred_column="name",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=replace(
                        _DEFAULT_CATEGORICAL_COMPARISON,
                        data_match=False,
                    ),
                ),
            ],
        )
        summary_df = aggregate_comparison_results([("sample1", result, None)])
        row = summary_df.iloc[0]
        assert row["false_positives"] == 1


class TestBuildColumnMappingTable:
    """Tests for build_column_mapping_table function."""

    def test_matched_columns_appear(self) -> None:
        """Matched columns have status='matched' with gt and pred names."""
        table = build_column_mapping_table(_DEFAULT_RESULT)
        matched = table[table["status"] == "matched"]
        assert len(matched) == 1
        assert matched.iloc[0]["gt_column"] == "age"
        assert matched.iloc[0]["pred_column"] == "age"
        assert matched.iloc[0]["match_score"] == 1.0

    def test_unmatched_gt_columns(self) -> None:
        """Unmatched GT columns appear with empty pred side."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[
                ColumnMatch("age", "age", 1.0, MatchMethod.LEVENSHTEIN),
                ColumnMatch("height", None, 0.3, None),
            ],
            unmatched_gt_columns=["height"],
        )
        table = build_column_mapping_table(result)
        unmatched_gt = table[table["status"] == "unmatched_gt"]
        assert len(unmatched_gt) == 1
        assert unmatched_gt.iloc[0]["gt_column"] == "height"
        assert unmatched_gt.iloc[0]["pred_column"] == ""

    def test_unmatched_pred_columns(self) -> None:
        """Unmatched pred columns appear with empty gt side."""
        result = replace(
            _DEFAULT_RESULT,
            unmatched_pred_columns=["extra_col"],
        )
        table = build_column_mapping_table(result)
        unmatched_pred = table[table["status"] == "unmatched_pred"]
        assert len(unmatched_pred) == 1
        assert unmatched_pred.iloc[0]["pred_column"] == "extra_col"
        assert unmatched_pred.iloc[0]["gt_column"] == ""

    def test_data_match_included(self) -> None:
        """data_match from the comparison is included in matched rows."""
        result = replace(
            _DEFAULT_RESULT,
            column_comparisons=[
                ColumnComparison(
                    gt_column="age",
                    pred_column="age",
                    column_type=ColumnType.NUMERIC,
                    numeric_comparison=_DEFAULT_NUMERIC_COMPARISON,
                    data_match=True,
                ),
            ],
        )
        table = build_column_mapping_table(result)
        matched = table[table["status"] == "matched"]
        assert matched.iloc[0]["data_match"] == True  # noqa: E712

    def test_empty_result(self) -> None:
        """Empty matches produce empty table."""
        result = replace(
            _DEFAULT_RESULT,
            column_matches=[],
            column_comparisons=[],
            unmatched_gt_columns=[],
            unmatched_pred_columns=[],
        )
        table = build_column_mapping_table(result)
        assert len(table) == 0


class TestBuildCategoryMappingTable:
    """Tests for build_category_mapping_table function."""

    def test_identity_mapping(self) -> None:
        """Categories with same name in gt and pred are status='identity'."""
        result = replace(
            _DEFAULT_RESULT,
            column_comparisons=[
                ColumnComparison(
                    gt_column="color",
                    pred_column="color",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=CategoricalComparison(
                        exact_match_rate=1.0,
                        gt_categories={"red", "blue"},
                        pred_categories={"red", "blue"},
                        missing_categories=set(),
                        extra_categories=set(),
                        category_overlap_score=1.0,
                        distribution_similarity=1.0,
                        data_match=True,
                        category_mapping={"red": "red", "blue": "blue"},
                    ),
                ),
            ],
        )
        table = build_category_mapping_table(result)
        assert all(table["status"] == "identity")

    def test_remapped_categories(self) -> None:
        """Categories with different names are status='remapped'."""
        result = replace(
            _DEFAULT_RESULT,
            column_comparisons=[
                ColumnComparison(
                    gt_column="color",
                    pred_column="colour",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=CategoricalComparison(
                        exact_match_rate=0.9,
                        gt_categories={"red", "blue"},
                        pred_categories={"Red", "Blue"},
                        missing_categories=set(),
                        extra_categories=set(),
                        category_overlap_score=1.0,
                        distribution_similarity=0.95,
                        data_match=True,
                        category_mapping={"Red": "red", "Blue": "blue"},
                    ),
                ),
            ],
        )
        table = build_category_mapping_table(result)
        remapped = table[table["status"] == "remapped"]
        assert len(remapped) == 2

    def test_unmatched_gt_categories(self) -> None:
        """GT categories not in the mapping appear as unmatched_gt."""
        result = replace(
            _DEFAULT_RESULT,
            column_comparisons=[
                ColumnComparison(
                    gt_column="color",
                    pred_column="color",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=CategoricalComparison(
                        exact_match_rate=0.7,
                        gt_categories={"red", "blue", "green"},
                        pred_categories={"red", "blue"},
                        missing_categories={"green"},
                        extra_categories=set(),
                        category_overlap_score=0.67,
                        distribution_similarity=0.8,
                        data_match=False,
                        category_mapping={"red": "red", "blue": "blue"},
                    ),
                ),
            ],
        )
        table = build_category_mapping_table(result)
        unmatched_gt = table[table["status"] == "unmatched_gt"]
        assert len(unmatched_gt) == 1
        assert unmatched_gt.iloc[0]["gt_category"] == "green"
        assert unmatched_gt.iloc[0]["pred_category"] == ""

    def test_unmatched_pred_categories(self) -> None:
        """Pred categories not in the mapping appear as unmatched_pred."""
        result = replace(
            _DEFAULT_RESULT,
            column_comparisons=[
                ColumnComparison(
                    gt_column="color",
                    pred_column="color",
                    column_type=ColumnType.CATEGORICAL,
                    categorical_comparison=CategoricalComparison(
                        exact_match_rate=0.7,
                        gt_categories={"red", "blue"},
                        pred_categories={"red", "blue", "yellow"},
                        missing_categories=set(),
                        extra_categories={"yellow"},
                        category_overlap_score=0.67,
                        distribution_similarity=0.8,
                        data_match=False,
                        category_mapping={"red": "red", "blue": "blue"},
                    ),
                ),
            ],
        )
        table = build_category_mapping_table(result)
        unmatched_pred = table[table["status"] == "unmatched_pred"]
        assert len(unmatched_pred) == 1
        assert unmatched_pred.iloc[0]["pred_category"] == "yellow"
        assert unmatched_pred.iloc[0]["gt_category"] == ""

    def test_numeric_columns_skipped(self) -> None:
        """Numeric-only comparisons produce no category mapping rows."""
        table = build_category_mapping_table(_DEFAULT_RESULT)
        assert len(table) == 0

    def test_no_comparisons(self) -> None:
        """Empty comparisons produce empty table."""
        result = replace(_DEFAULT_RESULT, column_comparisons=[])
        table = build_category_mapping_table(result)
        assert len(table) == 0
