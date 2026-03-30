"""Tests for data_comparator module."""

from unittest.mock import MagicMock, patch

import numpy as np
import pandas as pd
import pytest

from src.dataset_comparison.data_comparator import DataComparator
from src.dataset_comparison.models import (
    ColumnComparison,
    ColumnMatch,
    ColumnType,
    DataComparisonResult,
    JoinCompleteness,
    MatchMethod,
)


class TestDataComparatorInit:
    """Tests for DataComparator initialization."""

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_init_default_params(self, mock_column_matcher: MagicMock) -> None:
        """Test initialization with default parameters."""
        comparator = DataComparator()

        assert comparator.categorical_threshold == 20
        assert comparator.categorical_match_threshold == 0.8
        assert comparator.data_match_threshold == 0.7

        mock_column_matcher.assert_called_once_with(
            cross_encoder_model_name="cross-encoder/stsb-roberta-base",
            match_threshold=0.5,
        )

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_init_custom_params(self, mock_column_matcher: MagicMock) -> None:
        """Test initialization with custom parameters."""
        comparator = DataComparator(
            categorical_threshold=50,
            cross_encoder_model_name="custom-model",
            match_threshold=0.7,
            data_match_threshold=0.8,
            categorical_match_threshold=0.9,
        )

        assert comparator.categorical_threshold == 50
        assert comparator.categorical_match_threshold == 0.9
        assert comparator.data_match_threshold == 0.8

        mock_column_matcher.assert_called_once_with(
            cross_encoder_model_name="custom-model",
            match_threshold=0.7,
        )


class TestCheckJoinCompleteness:
    """Tests for _check_join_completeness method."""

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_identical_indices(self, mock_column_matcher: MagicMock) -> None:
        """Test join completeness with identical indices."""
        comparator = DataComparator()

        gt_df = pd.DataFrame({"a": [1, 2, 3]}, index=pd.Index([1, 2, 3], name="id"))
        pred_df = pd.DataFrame({"b": [4, 5, 6]}, index=pd.Index([1, 2, 3], name="id"))

        completeness, joined_df = comparator._check_join_completeness(gt_df, pred_df)

        assert completeness.gt_row_count == 3
        assert completeness.pred_row_count == 3
        assert completeness.joined_row_count == 3
        assert completeness.missing_in_pred == 0
        assert completeness.extra_in_pred == 0
        assert completeness.join_completeness_score == 1.0
        assert completeness.gt_duplicate_keys == 0
        assert completeness.pred_duplicate_keys == 0
        assert len(joined_df) == 3

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_missing_keys_in_pred(self, mock_column_matcher: MagicMock) -> None:
        """Test when pred is missing some keys from GT."""
        comparator = DataComparator()

        gt_df = pd.DataFrame(
            {"a": [1, 2, 3, 4]}, index=pd.Index([1, 2, 3, 4], name="id")
        )
        pred_df = pd.DataFrame({"b": [5, 6]}, index=pd.Index([1, 2], name="id"))

        completeness, joined_df = comparator._check_join_completeness(gt_df, pred_df)

        assert completeness.gt_row_count == 4
        assert completeness.pred_row_count == 2
        assert completeness.joined_row_count == 2
        assert completeness.missing_in_pred == 2
        assert completeness.extra_in_pred == 0
        assert completeness.join_completeness_score == 0.5

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_extra_keys_in_pred(self, mock_column_matcher: MagicMock) -> None:
        """Test when pred has extra keys not in GT."""
        comparator = DataComparator()

        gt_df = pd.DataFrame({"a": [1, 2]}, index=pd.Index([1, 2], name="id"))
        pred_df = pd.DataFrame(
            {"b": [3, 4, 5, 6]}, index=pd.Index([1, 2, 3, 4], name="id")
        )

        completeness, joined_df = comparator._check_join_completeness(gt_df, pred_df)

        assert completeness.gt_row_count == 2
        assert completeness.pred_row_count == 4
        assert completeness.joined_row_count == 2
        assert completeness.missing_in_pred == 0
        assert completeness.extra_in_pred == 2
        assert completeness.join_completeness_score == 1.0  # All GT keys found

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_no_overlapping_keys(self, mock_column_matcher: MagicMock) -> None:
        """Test when there are no overlapping keys."""
        comparator = DataComparator()

        gt_df = pd.DataFrame({"a": [1, 2]}, index=pd.Index([1, 2], name="id"))
        pred_df = pd.DataFrame({"b": [3, 4]}, index=pd.Index([3, 4], name="id"))

        completeness, joined_df = comparator._check_join_completeness(gt_df, pred_df)

        assert completeness.joined_row_count == 0
        assert completeness.missing_in_pred == 2
        assert completeness.extra_in_pred == 2
        assert completeness.join_completeness_score == 0.0

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_duplicate_keys_in_gt(self, mock_column_matcher: MagicMock) -> None:
        """Test detection of duplicate keys in GT."""
        comparator = DataComparator()

        gt_df = pd.DataFrame({"a": [1, 2, 3]}, index=pd.Index([1, 1, 2], name="id"))
        pred_df = pd.DataFrame({"b": [4, 5]}, index=pd.Index([1, 2], name="id"))

        completeness, _ = comparator._check_join_completeness(gt_df, pred_df)

        assert completeness.gt_duplicate_keys == 1
        assert completeness.has_duplicates is True

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_duplicate_keys_in_pred(self, mock_column_matcher: MagicMock) -> None:
        """Test detection of duplicate keys in pred."""
        comparator = DataComparator()

        gt_df = pd.DataFrame({"a": [1, 2]}, index=pd.Index([1, 2], name="id"))
        pred_df = pd.DataFrame({"b": [3, 4, 5]}, index=pd.Index([1, 1, 2], name="id"))

        completeness, _ = comparator._check_join_completeness(gt_df, pred_df)

        assert completeness.pred_duplicate_keys == 1
        assert completeness.has_duplicates is True

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_empty_gt_dataframe(self, mock_column_matcher: MagicMock) -> None:
        """Test with empty GT dataframe."""
        comparator = DataComparator()

        gt_df = pd.DataFrame({"a": []}, index=pd.Index([], name="id"))
        pred_df = pd.DataFrame({"b": [1, 2]}, index=pd.Index([1, 2], name="id"))

        completeness, joined_df = comparator._check_join_completeness(gt_df, pred_df)

        assert completeness.gt_row_count == 0
        assert completeness.joined_row_count == 0
        assert completeness.join_completeness_score == 0.0

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_joined_df_has_suffixes(self, mock_column_matcher: MagicMock) -> None:
        """Test that joined dataframe has correct column suffixes."""
        comparator = DataComparator()

        gt_df = pd.DataFrame({"col": [1, 2]}, index=pd.Index([1, 2], name="id"))
        pred_df = pd.DataFrame({"col": [3, 4]}, index=pd.Index([1, 2], name="id"))

        _, joined_df = comparator._check_join_completeness(gt_df, pred_df)

        assert "col_gt" in joined_df.columns
        assert "col_pred" in joined_df.columns


class TestCompareColumnPair:
    """Tests for _compare_column_pair method."""

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_numeric_column_comparison(self, mock_column_matcher: MagicMock) -> None:
        """Test comparison of numeric columns."""
        comparator = DataComparator(categorical_threshold=5)

        joined_df = pd.DataFrame(
            {
                "value_gt": [1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0, 9.0, 10.0],
                "value_pred": [1.1, 2.1, 3.1, 4.1, 5.1, 6.1, 7.1, 8.1, 9.1, 10.1],
            }
        )

        result = comparator._compare_column_pair(joined_df, "value", "value")

        assert result.gt_column == "value"
        assert result.pred_column == "value"
        assert result.column_type == ColumnType.NUMERIC
        assert result.numeric_comparison is not None
        assert result.categorical_comparison is None
        assert result.numeric_comparison.rmse == pytest.approx(0.1, abs=0.01)

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_categorical_column_comparison(
        self, mock_column_matcher: MagicMock
    ) -> None:
        """Test comparison of categorical columns."""
        comparator = DataComparator(categorical_threshold=20)

        joined_df = pd.DataFrame(
            {
                "category_gt": ["a", "b", "c", "a", "b"],
                "category_pred": ["a", "b", "c", "a", "b"],
            }
        )

        result = comparator._compare_column_pair(joined_df, "category", "category")

        assert result.gt_column == "category"
        assert result.pred_column == "category"
        assert result.column_type == ColumnType.CATEGORICAL
        assert result.categorical_comparison is not None
        assert result.numeric_comparison is None
        assert result.categorical_comparison.exact_match_rate == 1.0

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_different_column_names(self, mock_column_matcher: MagicMock) -> None:
        """Test comparison when GT and pred have different column names."""
        comparator = DataComparator(categorical_threshold=5)

        joined_df = pd.DataFrame(
            {
                "age_gt": [25, 30, 35, 40, 45, 50, 55, 60, 65, 70],
                "years_pred": [25, 30, 35, 40, 45, 50, 55, 60, 65, 70],
            }
        )

        result = comparator._compare_column_pair(joined_df, "age", "years")

        assert result.gt_column == "age"
        assert result.pred_column == "years"

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_columns_without_suffix(self, mock_column_matcher: MagicMock) -> None:
        """Test when columns don't have _gt/_pred suffix."""
        comparator = DataComparator(categorical_threshold=5)

        # When column names are different, no suffix is added
        joined_df = pd.DataFrame(
            {
                "age": [25, 30, 35, 40, 45, 50, 55, 60, 65, 70],
                "years": [25, 30, 35, 40, 45, 50, 55, 60, 65, 70],
            }
        )

        result = comparator._compare_column_pair(joined_df, "age", "years")

        assert result.gt_column == "age"
        assert result.pred_column == "years"


class TestDataMatchColumns:
    """Tests for _data_match_columns method."""

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_empty_unmatched_lists(self, mock_column_matcher: MagicMock) -> None:
        """Test with empty unmatched column lists."""
        comparator = DataComparator()

        joined_df = pd.DataFrame({"a": [1, 2, 3]})

        result = comparator._data_match_columns(joined_df, [], ["b"])
        assert result == []

        result = comparator._data_match_columns(joined_df, ["a"], [])
        assert result == []

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_numeric_data_matching(self, mock_column_matcher: MagicMock) -> None:
        """Test data matching for numeric columns."""
        comparator = DataComparator(
            categorical_threshold=5,
            data_match_threshold=0.5,
        )

        # Create columns with identical data but different names
        joined_df = pd.DataFrame(
            {
                "gt_col": list(range(100)),
                "pred_col": list(range(100)),
            }
        )

        result = comparator._data_match_columns(joined_df, ["gt_col"], ["pred_col"])

        assert len(result) == 1
        assert result[0].gt_column == "gt_col"
        assert result[0].pred_column == "pred_col"
        assert result[0].method == MatchMethod.DATA_NUMERIC
        assert result[0].score == pytest.approx(1.0, abs=0.01)

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_categorical_data_matching(self, mock_column_matcher: MagicMock) -> None:
        """Test data matching for categorical columns."""
        comparator = DataComparator(
            categorical_threshold=20,
            data_match_threshold=0.5,
        )

        joined_df = pd.DataFrame(
            {
                "gt_col": ["a", "b", "c", "a", "b"] * 10,
                "pred_col": ["a", "b", "c", "a", "b"] * 10,
            }
        )

        result = comparator._data_match_columns(joined_df, ["gt_col"], ["pred_col"])

        assert len(result) == 1
        assert result[0].gt_column == "gt_col"
        assert result[0].pred_column == "pred_col"
        assert result[0].method == MatchMethod.DATA_CATEGORICAL

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_incompatible_types_no_match(self, mock_column_matcher: MagicMock) -> None:
        """Test that incompatible types don't match."""
        comparator = DataComparator(
            categorical_threshold=20,
            data_match_threshold=0.5,
        )

        joined_df = pd.DataFrame(
            {
                "numeric_col": list(range(100)),
                "categorical_col": ["a", "b", "c"] * 33 + ["a"],
            }
        )

        result = comparator._data_match_columns(
            joined_df, ["numeric_col"], ["categorical_col"]
        )

        assert len(result) == 1
        assert result[0].pred_column is None
        assert result[0].method is None

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_below_threshold_no_match(self, mock_column_matcher: MagicMock) -> None:
        """Test that low similarity scores don't produce matches."""
        comparator = DataComparator(
            categorical_threshold=5,
            data_match_threshold=0.9,  # High threshold
        )

        # Very different data
        rng = np.random.default_rng()
        x = rng.standard_normal(100)
        joined_df = pd.DataFrame(
            {
                "gt_col": x,
                "pred_col": x * 100,
            }
        )

        result = comparator._data_match_columns(joined_df, ["gt_col"], ["pred_col"])

        assert len(result) == 1
        assert result[0].pred_column is None

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_greedy_matching_best_first(self, mock_column_matcher: MagicMock) -> None:
        """Test that greedy matching assigns best matches first."""
        comparator = DataComparator(
            categorical_threshold=5,
            data_match_threshold=0.5,
        )

        # gt_a matches pred_a perfectly, gt_b also matches pred_a well
        joined_df = pd.DataFrame(
            {
                "gt_a": list(range(100)),
                "gt_b": list(range(100)),
                "pred_a": list(range(100)),
                "pred_b": list(range(50, 150)),  # Shifted
            }
        )

        result = comparator._data_match_columns(
            joined_df, ["gt_a", "gt_b"], ["pred_a", "pred_b"]
        )

        # gt_a should get pred_a (best match)
        gt_a_match = next(m for m in result if m.gt_column == "gt_a")
        assert gt_a_match.pred_column == "pred_a"


class TestCompare:
    """Tests for compare method."""

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_requires_named_index_gt(self, mock_column_matcher: MagicMock) -> None:
        """Test that GT dataframe must have named index."""
        comparator = DataComparator()

        gt_df = pd.DataFrame({"a": [1, 2, 3]})  # No named index
        pred_df = pd.DataFrame({"a": [1, 2, 3]}, index=pd.Index([0, 1, 2], name="id"))

        with pytest.raises(
            ValueError, match="Ground truth dataframe must have an index"
        ):
            comparator.compare(gt_df, pred_df)

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_requires_named_index_pred(self, mock_column_matcher: MagicMock) -> None:
        """Test that pred dataframe must have named index."""
        comparator = DataComparator()

        gt_df = pd.DataFrame({"a": [1, 2, 3]}, index=pd.Index([0, 1, 2], name="id"))
        pred_df = pd.DataFrame({"a": [1, 2, 3]})  # No named index

        with pytest.raises(ValueError, match="Predicted dataframe must have an index"):
            comparator.compare(gt_df, pred_df)

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_full_comparison_identical_data(
        self, mock_column_matcher: MagicMock
    ) -> None:
        """Test full comparison with identical dataframes."""
        mock_matcher_instance = MagicMock()
        mock_column_matcher.return_value = mock_matcher_instance

        # Mock column matching to return perfect matches
        mock_matcher_instance.match_columns.return_value = [
            ColumnMatch("col_a", "col_a", 1.0, MatchMethod.LEVENSHTEIN),
            ColumnMatch("col_b", "col_b", 1.0, MatchMethod.LEVENSHTEIN),
        ]

        comparator = DataComparator(categorical_threshold=5)

        gt_df = pd.DataFrame(
            {
                "col_a": list(range(50)),
                "col_b": list(range(50, 100)),
            },
            index=pd.Index(range(50), name="id"),
        )
        pred_df = pd.DataFrame(
            {
                "col_a": list(range(50)),
                "col_b": list(range(50, 100)),
            },
            index=pd.Index(range(50), name="id"),
        )

        result, _ = comparator.compare(gt_df, pred_df)

        assert isinstance(result, DataComparisonResult)
        assert result.primary_key == "id | id"
        assert result.join_completeness.join_completeness_score == 1.0
        assert len(result.column_matches) == 2
        assert len(result.column_comparisons) == 2
        assert result.unmatched_gt_columns == []
        assert result.unmatched_pred_columns == []

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_comparison_with_unmatched_columns(
        self, mock_column_matcher: MagicMock
    ) -> None:
        """Test comparison when some columns don't match."""
        mock_matcher_instance = MagicMock()
        mock_column_matcher.return_value = mock_matcher_instance

        # Only col_a matches, col_b doesn't
        mock_matcher_instance.match_columns.return_value = [
            ColumnMatch("col_a", "col_a", 1.0, MatchMethod.LEVENSHTEIN),
            ColumnMatch("col_b", None, 0.2, None),
        ]

        comparator = DataComparator(categorical_threshold=5, data_match_threshold=0.9)

        gt_df = pd.DataFrame(
            {
                "col_a": list(range(50)),
                "col_b": list(range(50)),
            },
            index=pd.Index(range(50), name="id"),
        )
        pred_df = pd.DataFrame(
            {
                "col_a": list(range(50)),
                "col_x": ["x"] * 50,  # Different column
            },
            index=pd.Index(range(50), name="id"),
        )

        result, _ = comparator.compare(gt_df, pred_df, use_data_matching=False)

        assert len(result.unmatched_gt_columns) == 1
        assert "col_b" in result.unmatched_gt_columns
        assert len(result.unmatched_pred_columns) == 1
        assert "col_x" in result.unmatched_pred_columns

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_data_matching_enabled(self, mock_column_matcher: MagicMock) -> None:
        """Test that data matching is used when enabled."""
        mock_matcher_instance = MagicMock()
        mock_column_matcher.return_value = mock_matcher_instance

        # Name matching fails
        mock_matcher_instance.match_columns.return_value = [
            ColumnMatch("values", None, 0.1, None),
        ]

        comparator = DataComparator(
            categorical_threshold=5,
            data_match_threshold=0.5,
        )

        gt_df = pd.DataFrame(
            {"values": list(range(100))},
            index=pd.Index(range(100), name="id"),
        )
        pred_df = pd.DataFrame(
            {"numbers": list(range(100))},  # Same data, different name
            index=pd.Index(range(100), name="id"),
        )

        result, _ = comparator.compare(gt_df, pred_df, use_data_matching=True)

        # Data matching should find the match
        matched_cols = [m for m in result.column_matches if m.pred_column is not None]
        assert len(matched_cols) == 1
        assert matched_cols[0].method == MatchMethod.DATA_NUMERIC

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_data_matching_disabled(self, mock_column_matcher: MagicMock) -> None:
        """Test that data matching is not used when disabled."""
        mock_matcher_instance = MagicMock()
        mock_column_matcher.return_value = mock_matcher_instance

        mock_matcher_instance.match_columns.return_value = [
            ColumnMatch("values", None, 0.1, None),
        ]

        comparator = DataComparator(
            categorical_threshold=5,
            data_match_threshold=0.5,
        )

        gt_df = pd.DataFrame(
            {"values": list(range(100))},
            index=pd.Index(range(100), name="id"),
        )
        pred_df = pd.DataFrame(
            {"numbers": list(range(100))},
            index=pd.Index(range(100), name="id"),
        )

        result, _ = comparator.compare(gt_df, pred_df, use_data_matching=False)

        # No data matching, column stays unmatched
        assert "values" in result.unmatched_gt_columns


class TestIntegration:
    """Integration tests for DataComparator."""

    @patch("src.dataset_comparison.data_comparator.ColumnMatcher")
    def test_end_to_end_comparison(self, mock_column_matcher: MagicMock) -> None:
        """Test end-to-end comparison workflow."""
        mock_matcher_instance = MagicMock()
        mock_column_matcher.return_value = mock_matcher_instance

        mock_matcher_instance.match_columns.return_value = [
            ColumnMatch("age", "age", 1.0, MatchMethod.LEVENSHTEIN),
            ColumnMatch("name", "name", 1.0, MatchMethod.LEVENSHTEIN),
        ]

        comparator = DataComparator(categorical_threshold=5)

        gt_df = pd.DataFrame(
            {
                "age": [25, 30, 35, 40, 45, 50, 55, 60, 65, 70] * 5,
                "name": ["Alice", "Bob", "Charlie"] * 16 + ["Alice", "Bob"],
            },
            index=pd.Index(range(50), name="id"),
        )
        pred_df = pd.DataFrame(
            {
                "age": [25, 30, 35, 40, 45, 50, 55, 60, 65, 70] * 5,
                "name": ["Alice", "Bob", "Charlie"] * 16 + ["Alice", "Bob"],
            },
            index=pd.Index(range(50), name="id"),
        )

        result, _ = comparator.compare(gt_df, pred_df)

        # Verify structure
        assert isinstance(result, DataComparisonResult)
        assert isinstance(result.join_completeness, JoinCompleteness)
        assert all(isinstance(m, ColumnMatch) for m in result.column_matches)
        assert all(isinstance(c, ColumnComparison) for c in result.column_comparisons)

        # Verify completeness
        assert result.join_completeness.join_completeness_score == 1.0

        # Verify comparisons
        assert len(result.column_comparisons) == 2

        age_comp = next(c for c in result.column_comparisons if c.gt_column == "age")
        assert age_comp.column_type == ColumnType.NUMERIC
        assert age_comp.numeric_comparison is not None
        assert age_comp.numeric_comparison.rmse == 0.0

        name_comp = next(c for c in result.column_comparisons if c.gt_column == "name")
        assert name_comp.column_type == ColumnType.CATEGORICAL
        assert name_comp.categorical_comparison is not None
        assert name_comp.categorical_comparison.exact_match_rate == 1.0
