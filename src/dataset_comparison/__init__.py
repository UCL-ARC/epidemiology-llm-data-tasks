"""Dataset comparison tools for ground truth vs predicted dataframes."""

from .column_matcher import ColumnMatcher
from .comparisons import (
    compare_categorical,
    compare_numeric,
    compute_categorical_similarity,
    compute_numeric_similarity,
    infer_column_type,
)
from .data_comparator import DataComparator
from .models import (
    CategoricalComparison,
    ColumnComparison,
    ColumnMatch,
    ColumnType,
    DataComparisonResult,
    JoinCompleteness,
    MatchMethod,
    NumericComparison,
)
from .report import print_comparison_report

__all__ = [
    "CategoricalComparison",
    "ColumnComparison",
    "ColumnMatch",
    "ColumnMatcher",
    "ColumnType",
    "DataComparator",
    "DataComparisonResult",
    "JoinCompleteness",
    "MatchMethod",
    "NumericComparison",
    "compare_categorical",
    "compare_numeric",
    "compute_categorical_similarity",
    "compute_numeric_similarity",
    "infer_column_type",
    "print_comparison_report",
]
