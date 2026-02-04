"""Data models for dataset comparison results."""

from dataclasses import dataclass, field
from enum import Enum


class MatchMethod(Enum):
    """Available matching methods."""

    LEVENSHTEIN = "levenshtein"
    SEMANTIC = "semantic"
    DATA_NUMERIC = "data_numeric"
    DATA_CATEGORICAL = "data_categorical"


class ColumnType(Enum):
    """Column data types for comparison."""

    NUMERIC = "numeric"
    CATEGORICAL = "categorical"


@dataclass
class ColumnMatch:
    """Result of matching a ground truth column to a predicted column."""

    gt_column: str
    pred_column: str | None
    score: float
    method: MatchMethod | None


@dataclass
class JoinCompleteness:
    """Results of join completeness check."""

    gt_row_count: int
    pred_row_count: int
    joined_row_count: int
    missing_in_pred: int
    extra_in_pred: int
    completeness_score: float
    gt_duplicate_keys: int
    pred_duplicate_keys: int

    @property
    def has_duplicates(self) -> bool:
        """Check if there are duplicates in either dataframe based on counts."""
        return self.gt_duplicate_keys > 0 or self.pred_duplicate_keys > 0


@dataclass
class NumericComparison:
    """Results of numeric column comparison."""

    rmse: float
    mae: float
    nrmse: float
    nmae: float
    correlation: float | None
    gt_mean: float
    pred_mean: float
    gt_std: float
    pred_std: float
    data_match: bool | None


@dataclass
class CategoricalComparison:
    """Results of categorical column comparison."""

    exact_match_rate: float
    gt_categories: set[str]
    pred_categories: set[str]
    missing_categories: set[str]
    extra_categories: set[str]
    category_overlap_score: float
    distribution_similarity: float
    data_match: bool | None


@dataclass
class ColumnComparison:
    """Full comparison result for a single matched column pair."""

    gt_column: str
    pred_column: str
    column_type: ColumnType
    numeric_comparison: NumericComparison | None = None
    categorical_comparison: CategoricalComparison | None = None
    data_match: bool | None = None


@dataclass
class DataComparisonResult:
    """Full comparison result between ground truth and predicted dataframes."""

    primary_key: str
    join_completeness: JoinCompleteness
    column_matches: list[ColumnMatch]
    column_comparisons: list[ColumnComparison]
    unmatched_gt_columns: list[str] = field(default_factory=list)
    unmatched_pred_columns: list[str] = field(default_factory=list)
    task_completion_percentage: float | None = None
