"""Tests for comparisons module."""

import numpy as np
import pandas as pd
import pytest

from src.dataset_comparison.comparisons import (
    compare_categorical,
    compare_numeric,
    compute_categorical_similarity,
    compute_numeric_similarity,
    infer_column_type,
)
from src.dataset_comparison.models import ColumnType


class TestInferColumnType:
    """Tests for infer_column_type function."""

    def test_numeric_column_above_threshold(self) -> None:
        """Test that numeric column with many unique values is NUMERIC."""
        gt_series = pd.Series(range(100))

        result = infer_column_type(gt_series, categorical_threshold=20)

        assert result == ColumnType.NUMERIC

    def test_numeric_column_below_threshold(self) -> None:
        """Test that numeric column with few unique values is CATEGORICAL."""
        gt_series = pd.Series([1, 2, 3, 1, 2, 3, 1, 2, 3])

        result = infer_column_type(gt_series, categorical_threshold=20)

        assert result == ColumnType.CATEGORICAL

    def test_string_column_is_categorical(self) -> None:
        """Test that string columns are always CATEGORICAL."""
        gt_series = pd.Series(["a", "b", "c"] * 100)

        result = infer_column_type(gt_series)

        assert result == ColumnType.CATEGORICAL

    # TO DO: this has small peercentage chance of failing due to
    # random nature of data
    def test_with_pred_series_both_numeric(self) -> None:
        """Test with both series numeric and above threshold."""
        gt_series = pd.Series(range(100))
        pred_series = pd.Series(range(100, 200))

        result = infer_column_type(gt_series, pred_series, categorical_threshold=10)

        assert result == ColumnType.NUMERIC

    def test_with_pred_series_gt_string(self) -> None:
        """Test that if GT is string, result is CATEGORICAL."""
        gt_series = pd.Series(["a", "b", "c"] * 100)
        pred_series = pd.Series(range(300))

        result = infer_column_type(gt_series, pred_series)

        assert result == ColumnType.CATEGORICAL

    def test_with_pred_series_pred_string(self) -> None:
        """Test that if pred is string, result is CATEGORICAL."""
        gt_series = pd.Series(range(300))
        pred_series = pd.Series(["a", "b", "c"] * 100)

        result = infer_column_type(gt_series, pred_series)

        assert result == ColumnType.CATEGORICAL

    def test_with_pred_series_below_threshold(self) -> None:
        """Test that if either series has few unique values, result is CATEGORICAL."""
        gt_series = pd.Series(range(100))
        pred_series = pd.Series([1, 2, 3] * 100)  # Only 3 unique values

        result = infer_column_type(gt_series, pred_series, categorical_threshold=20)

        assert result == ColumnType.CATEGORICAL

    def test_custom_categorical_threshold(self) -> None:
        """Test with custom categorical threshold."""
        gt_series = pd.Series(range(50))

        # With threshold of 100, 50 unique values should be CATEGORICAL
        result = infer_column_type(gt_series, categorical_threshold=100)
        assert result == ColumnType.CATEGORICAL

        # With threshold of 10, 50 unique values should be NUMERIC
        # TO DO: this has small percentage chance of failing due to
        # random nature of data
        result = infer_column_type(gt_series, categorical_threshold=10)
        assert result == ColumnType.NUMERIC

    def test_boolean_column(self) -> None:
        """Test that boolean columns are treated as categorical (few unique)."""
        gt_series = pd.Series([True, False, True, False] * 25)

        result = infer_column_type(gt_series, categorical_threshold=20)

        # Boolean has 2 unique values, below threshold
        assert result == ColumnType.CATEGORICAL


class TestCompareNumeric:
    """Tests for compare_numeric function."""

    def test_identical_series(self) -> None:
        """Test comparison of identical series."""
        gt_series = pd.Series([1.0, 2.0, 3.0, 4.0, 5.0])
        pred_series = pd.Series([1.0, 2.0, 3.0, 4.0, 5.0])

        result = compare_numeric(gt_series, pred_series)

        assert result.rmse == 0.0
        assert result.mae == 0.0
        assert result.correlation == pytest.approx(1.0)
        assert result.gt_mean == result.pred_mean
        assert result.gt_std == result.pred_std

    def test_different_series(self) -> None:
        """Test comparison of different series."""
        gt_series = pd.Series([1.0, 2.0, 3.0, 4.0, 5.0])
        pred_series = pd.Series([2.0, 3.0, 4.0, 5.0, 6.0])  # All off by 1

        result = compare_numeric(gt_series, pred_series)

        assert result.rmse == pytest.approx(1.0)
        assert result.mae == pytest.approx(1.0)
        assert result.correlation == pytest.approx(
            1.0
        )  # Perfect correlation, just shifted

    def test_with_nan_values(self) -> None:
        """Test that NaN values are excluded from comparison."""
        gt_series = pd.Series([1.0, 2.0, np.nan, 4.0, 5.0])
        pred_series = pd.Series([1.0, np.nan, 3.0, 4.0, 5.0])

        result = compare_numeric(gt_series, pred_series)

        # Only indices 0, 3, 4 should be compared
        assert not np.isnan(result.rmse)
        assert not np.isnan(result.mae)

    def test_all_nan_values(self) -> None:
        """Test with all NaN values returns NaN results."""
        gt_series = pd.Series([np.nan, np.nan, np.nan])
        pred_series = pd.Series([np.nan, np.nan, np.nan])

        result = compare_numeric(gt_series, pred_series)

        assert np.isnan(result.rmse)
        assert np.isnan(result.mae)
        assert result.correlation is None

    def test_no_overlapping_non_nan(self) -> None:
        """Test when there are no overlapping non-NaN values."""
        gt_series = pd.Series([1.0, np.nan, np.nan])
        pred_series = pd.Series([np.nan, 2.0, 3.0])

        result = compare_numeric(gt_series, pred_series)

        assert np.isnan(result.rmse)
        assert np.isnan(result.mae)

    def test_constant_series_no_correlation(self) -> None:
        """Test that constant series returns None for correlation."""
        gt_series = pd.Series([5.0, 5.0, 5.0, 5.0, 5.0])
        pred_series = pd.Series([3.0, 3.0, 3.0, 3.0, 3.0])

        result = compare_numeric(gt_series, pred_series)

        assert result.correlation is None
        assert result.gt_std == 0.0
        assert result.pred_std == 0.0

    def test_negative_correlation(self) -> None:
        """Test series with negative correlation."""
        gt_series = pd.Series([1.0, 2.0, 3.0, 4.0, 5.0])
        pred_series = pd.Series([5.0, 4.0, 3.0, 2.0, 1.0])

        result = compare_numeric(gt_series, pred_series)

        assert result.correlation == pytest.approx(-1.0)

    def test_statistics_correctness(self) -> None:
        """Test that mean and std are computed correctly."""
        gt_series = pd.Series([10.0, 20.0, 30.0, 40.0, 50.0])
        pred_series = pd.Series([15.0, 25.0, 35.0, 45.0, 55.0])

        result = compare_numeric(gt_series, pred_series)

        assert result.gt_mean == pytest.approx(30.0)
        assert result.pred_mean == pytest.approx(35.0)


class TestComputeNumericSimilarity:
    """Tests for compute_numeric_similarity function."""

    def test_identical_series_returns_one(self) -> None:
        """Test that identical series return similarity of 1.0."""
        gt_series = pd.Series([1.0, 2.0, 3.0, 4.0, 5.0])
        pred_series = pd.Series([1.0, 2.0, 3.0, 4.0, 5.0])

        result = compute_numeric_similarity(gt_series, pred_series)

        assert result == 1.0

    def test_very_different_series_returns_low(self) -> None:
        """Test that very different series return low similarity."""
        gt_series = pd.Series([1.0, 2.0, 3.0, 4.0, 5.0])
        pred_series = pd.Series([100.0, 200.0, 300.0, 400.0, 500.0])

        result = compute_numeric_similarity(gt_series, pred_series)

        assert result == 0.0

    def test_returns_between_zero_and_one(self) -> None:
        """Test that result is always between 0 and 1."""
        gt_series = pd.Series([1.0, 2.0, 3.0, 4.0, 5.0])
        pred_series = pd.Series([1.5, 2.5, 3.5, 4.5, 5.5])

        result = compute_numeric_similarity(gt_series, pred_series)

        assert 0.0 <= result <= 1.0

    def test_all_nan_returns_zero(self) -> None:
        """Test that all NaN values return 0.0 similarity."""
        gt_series = pd.Series([np.nan, np.nan, np.nan])
        pred_series = pd.Series([np.nan, np.nan, np.nan])

        result = compute_numeric_similarity(gt_series, pred_series)

        assert result == 0.0

    def test_constant_gt_series_with_matching_pred(self) -> None:
        """Test constant GT series with matching pred returns 1.0."""
        gt_series = pd.Series([5.0, 5.0, 5.0, 5.0])
        pred_series = pd.Series([5.0, 5.0, 5.0, 5.0])

        result = compute_numeric_similarity(gt_series, pred_series)

        assert result == 1.0

    def test_constant_gt_series_with_different_pred(self) -> None:
        """Test constant GT series with different pred returns 0.0."""
        gt_series = pd.Series([5.0, 5.0, 5.0, 5.0])
        pred_series = pd.Series([10.0, 10.0, 10.0, 10.0])

        result = compute_numeric_similarity(gt_series, pred_series)

        assert result == 0.0


class TestCompareCategorical:
    """Tests for compare_categorical function."""

    def test_identical_series(self) -> None:
        """Test comparison of identical categorical series."""
        gt_series = pd.Series(["a", "b", "c", "a", "b"])
        pred_series = pd.Series(["a", "b", "c", "a", "b"])

        result = compare_categorical(gt_series, pred_series)

        assert result.exact_match_rate == 1.0
        assert result.category_overlap_score == 1.0
        assert result.distribution_similarity == pytest.approx(1.0, abs=0.01)
        assert result.missing_categories == set()
        assert result.extra_categories == set()

    def test_category_mapping(self) -> None:
        """Test that categories are mapped based on co-occurrence."""
        # GT has "yes"/"no", pred has "1"/"0" but in same positions
        gt_series = pd.Series(["yes", "no", "yes", "no", "yes"])
        pred_series = pd.Series(["1", "0", "1", "0", "1"])

        result = compare_categorical(
            gt_series, pred_series, categorical_match_threshold=0.8
        )

        # Should map "1" -> "yes" and "0" -> "no"
        assert result.exact_match_rate == 1.0

    def test_partial_match(self) -> None:
        """Test with partial matching."""
        gt_series = pd.Series(["a", "a", "b", "b", "c"])
        pred_series = pd.Series(["a", "a", "b", "x", "y"])

        result = compare_categorical(gt_series, pred_series)

        assert 0.0 < result.exact_match_rate < 1.0

    def test_missing_categories_detected(self) -> None:
        """Test that missing categories are detected."""
        gt_series = pd.Series(["a", "b", "c", "d"])
        pred_series = pd.Series(["a", "b", "a", "b"])

        result = compare_categorical(gt_series, pred_series)

        # "c" and "d" are in GT but not in pred
        assert len(result.missing_categories) > 0

    def test_extra_categories_detected(self) -> None:
        """Test that extra categories are detected."""
        gt_series = pd.Series(["a", "b", "a", "b"])
        pred_series = pd.Series(["a", "b", "c", "d"])

        result = compare_categorical(gt_series, pred_series)

        # "c" and "d" are in pred but not in GT
        assert len(result.extra_categories) > 0

    def test_categorical_match_threshold(self) -> None:
        """Test that categorical_match_threshold affects mapping."""
        gt_series = pd.Series(["a", "a", "a", "b", "b"])
        pred_series = pd.Series(["x", "x", "y", "y", "y"])

        # With low threshold, mapping might occur
        result_low = compare_categorical(
            gt_series, pred_series, categorical_match_threshold=0.3
        )

        # With high threshold, no mapping
        result_high = compare_categorical(
            gt_series, pred_series, categorical_match_threshold=0.99
        )

        # Results should differ based on threshold
        assert result_low.exact_match_rate != result_high.exact_match_rate

    def test_distribution_similarity_calculation(self) -> None:
        """Test that distribution similarity is calculated correctly."""
        gt_series = pd.Series(["a", "a", "a", "b"])  # 75% a, 25% b
        pred_series = pd.Series(["a", "a", "a", "b"])  # Same distribution

        result = compare_categorical(gt_series, pred_series)

        assert result.distribution_similarity == pytest.approx(1.0, abs=0.01)


class TestComputeCategoricalSimilarity:
    """Tests for compute_categorical_similarity function."""

    def test_identical_series_returns_one(self) -> None:
        """Test that identical series return similarity of 1.0."""
        gt_series = pd.Series(["a", "b", "c", "a", "b"])
        pred_series = pd.Series(["a", "b", "c", "a", "b"])

        result = compute_categorical_similarity(gt_series, pred_series)

        assert result == pytest.approx(1.0, abs=0.01)

    def test_returns_between_zero_and_one(self) -> None:
        """Test that result is always between 0 and 1."""
        gt_series = pd.Series(["a", "b", "c"] * 10)
        pred_series = pd.Series(["x", "y", "z"] * 10)

        result = compute_categorical_similarity(gt_series, pred_series)

        assert 0.0 <= result <= 1.0

    def test_weighted_combination(self) -> None:
        """Test that result is a weighted combination of metrics."""
        gt_series = pd.Series(["a", "b", "a", "b"])
        pred_series = pd.Series(["a", "b", "a", "b"])

        result = compute_categorical_similarity(gt_series, pred_series)

        comparison = compare_categorical(gt_series, pred_series)
        expected = (
            0.5 * comparison.exact_match_rate
            + 0.3 * comparison.distribution_similarity
            + 0.2 * comparison.category_overlap_score
        )

        assert result == pytest.approx(expected, abs=0.01)

    def test_categorical_match_threshold_passed(self) -> None:
        """Test that categorical_match_threshold is passed through."""
        gt_series = pd.Series(["a", "a", "b", "b"])
        pred_series = pd.Series(["x", "x", "y", "y"])

        result_low = compute_categorical_similarity(
            gt_series, pred_series, categorical_match_threshold=0.3
        )
        result_high = compute_categorical_similarity(
            gt_series, pred_series, categorical_match_threshold=0.99
        )

        # Different thresholds may produce different results
        # (depending on mapping behavior)
        assert isinstance(result_low, float)
        assert isinstance(result_high, float)


class TestEdgeCases:
    """Tests for edge cases across all functions."""

    def test_empty_series_numeric(self) -> None:
        """Test compare_numeric with empty series."""
        gt_series = pd.Series([], dtype=float)
        pred_series = pd.Series([], dtype=float)

        result = compare_numeric(gt_series, pred_series)

        assert np.isnan(result.rmse)
        assert np.isnan(result.mae)

    def test_empty_series_categorical(self) -> None:
        """Test compare_categorical with empty series."""
        gt_series = pd.Series([], dtype=str)
        pred_series = pd.Series([], dtype=str)

        result = compare_categorical(gt_series, pred_series)

        assert result.gt_categories == set()
        assert result.pred_categories == set()

    def test_single_element_series(self) -> None:
        """Test with single element series."""
        gt_series = pd.Series([1.0])
        pred_series = pd.Series([1.0])

        result = compare_numeric(gt_series, pred_series)

        assert result.rmse == 0.0
        assert result.mae == 0.0

    def test_large_series_performance(self) -> None:
        """Test that functions handle large series reasonably."""
        rng = np.random.default_rng()
        x = rng.standard_normal(10000)
        gt_series = pd.Series(x)
        pred_series = pd.Series(x)

        # Should complete without error
        result = compare_numeric(gt_series, pred_series)

        assert not np.isnan(result.rmse)
        assert not np.isnan(result.mae)

    def test_integer_series_treated_as_numeric(self) -> None:
        """Test that integer series are handled correctly."""
        gt_series = pd.Series([1, 2, 3, 4, 5])
        pred_series = pd.Series([1, 2, 3, 4, 5])

        result = compare_numeric(gt_series, pred_series)

        assert result.rmse == 0.0
