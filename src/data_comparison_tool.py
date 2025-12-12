"""Column matching and data comparison for ground truth and generated dataframes."""

from dataclasses import dataclass, field
from enum import Enum

import numpy as np
import pandas as pd
from IPython import embed  # noqa: F401
from jellyfish import levenshtein_distance
from loguru import logger
from sentence_transformers import CrossEncoder


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
    missing_in_pred: int  # rows in GT but not in pred
    extra_in_pred: int  # rows in pred but not in GT
    completeness_score: (
        float  # joined / gt_row_count TO DO: this should be plus or minus
    )
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
    correlation: float | None  # None if can't compute (e.g., constant column)
    gt_mean: float
    pred_mean: float
    gt_std: float
    pred_std: float


@dataclass
class CategoricalComparison:
    """Results of categorical column comparison."""

    exact_match_rate: float  # proportion of rows with identical values
    gt_categories: set[str]
    pred_categories: set[str]
    missing_categories: set[str]  # in GT but not in pred
    extra_categories: set[str]  # in pred but not in GT
    category_overlap_score: float  # Jaccard similarity of category sets
    distribution_similarity: float  # 1 - Jensen-Shannon divergence


@dataclass
class ColumnComparison:
    """Full comparison result for a single matched column pair."""

    gt_column: str
    pred_column: str
    column_type: ColumnType
    numeric_comparison: NumericComparison | None = None
    categorical_comparison: CategoricalComparison | None = None


@dataclass
class DataComparisonResult:
    """Full comparison result between ground truth and predicted dataframes."""

    primary_key: str
    join_completeness: JoinCompleteness
    column_matches: list[ColumnMatch]
    column_comparisons: list[ColumnComparison]
    unmatched_gt_columns: list[str] = field(default_factory=list)
    unmatched_pred_columns: list[str] = field(default_factory=list)


class ColumnMatcher:
    """
    Match columns between a ground truth dataframe and a predicted dataframe.

    Uses both Levenshtein distance and semantic similarity, taking the
    best score for each potential match.
    """

    def __init__(
        self,
        cross_encoder_model_name: str = "cross-encoder/stsb-roberta-base",
        match_threshold: float = 0.5,
    ) -> None:
        """
        Initialise the ColumnMatcher.

        Args:
            cross_encoder_model_name: Name of the cross-encoder model
            for semantic matching.
            match_threshold: Minimum score (0-1) required to consider a match valid.

        """
        self.cross_encoder = CrossEncoder(cross_encoder_model_name)
        self.match_threshold = match_threshold

    @staticmethod
    def _normalise(s: str) -> str:
        """Normalise a string for comparison."""
        return str(s).lower().strip().replace("_", " ").replace("-", " ")

    def _levenshtein_similarity(self, a: str, b: str) -> float:
        """
        Calculate normalised Levenshtein similarity (0-1).

        Returns 1.0 for identical strings, 0.0 for completely different.
        """
        a_norm = self._normalise(a)
        b_norm = self._normalise(b)

        if not a_norm or not b_norm:
            return 0.0

        max_len = max(len(a_norm), len(b_norm))
        distance = levenshtein_distance(a_norm, b_norm)

        return 1.0 - (distance / max_len)

    def _semantic_similarity(self, a: str, b: str) -> float:
        """Calculate semantic similarity using cross-encoder (0-1)."""
        a_norm = self._normalise(a)
        b_norm = self._normalise(b)

        score = self.cross_encoder.predict([[a_norm, b_norm]])
        return float(max(0.0, min(1.0, score)))

    def _best_similarity(
        self, a: str, b: str, semantic_weighting: float
    ) -> tuple[float, MatchMethod]:
        """
        Calculate similarity using both methods and return the best score.

        Returns:
            Tuple of (best_score, method_used).

        """
        lev_score = self._levenshtein_similarity(a, b)
        sem_score = self._semantic_similarity(a, b) * semantic_weighting

        if lev_score >= sem_score:
            return lev_score, MatchMethod.LEVENSHTEIN
        return sem_score, MatchMethod.SEMANTIC

    def _compute_similarity_matrix(
        self,
        gt_columns: list[str],
        pred_columns: list[str],
        semantic_weighting: float = 1.0,
    ) -> dict[str, dict[str, tuple[float, MatchMethod]]]:
        """
        Compute similarity scores for all column pairs.

        Returns:
            Nested dict: {gt_col: {pred_col: (score, method), ...}, ...}

        """
        matrix: dict[str, dict[str, tuple[float, MatchMethod]]] = {}

        for gt_col in gt_columns:
            matrix[gt_col] = {}
            for pred_col in pred_columns:
                score, method = self._best_similarity(
                    gt_col, pred_col, semantic_weighting
                )
                matrix[gt_col][pred_col] = (score, method)
                logger.debug(
                    f"Similarity '{gt_col}' <-> '{pred_col}': {score:.3f} "
                    f"({method.value})"
                )

        return matrix

    def match_columns(
        self,
        gt_df: pd.DataFrame,
        pred_df: pd.DataFrame,
        semantic_weighting: float = 1.0,
    ) -> list[ColumnMatch]:
        """
        Match ground truth columns to predicted columns.

        Uses a greedy algorithm: for each gt column, find the best available
        pred column match. Once a pred column is matched, it's removed from
        consideration.

        Args:
            gt_df: Ground truth dataframe.
            pred_df: Predicted/generated dataframe.

        Returns:
            List of ColumnMatch objects, one per ground truth column.

        """
        gt_columns = list(gt_df.columns)
        pred_columns = list(pred_df.columns)

        logger.info(
            f"Matching {len(gt_columns)} GT columns to {len(pred_columns)} pred columns"
        )

        similarity_matrix = self._compute_similarity_matrix(
            gt_columns, pred_columns, semantic_weighting
        )

        matches: list[ColumnMatch] = []
        available_pred_columns = set(pred_columns)

        # get best scores for each gt column
        gt_best_scores = [
            (gt_col, max(similarity_matrix[gt_col].values(), key=lambda x: x[0]))
            for gt_col in gt_columns
        ]

        # for our greedy algorithm we start with the best column match
        gt_sorted = sorted(gt_best_scores, key=lambda x: x[1][0], reverse=True)

        for gt_col, _ in gt_sorted:
            best_pred_col = None
            best_score = 0.0
            best_method = None

            for pred_col in available_pred_columns:
                score, method = similarity_matrix[gt_col][pred_col]
                if score > best_score:
                    best_score = score
                    best_pred_col = pred_col
                    best_method = method

            if best_score >= self.match_threshold and best_pred_col is not None:
                matches.append(
                    ColumnMatch(
                        gt_column=gt_col,
                        pred_column=best_pred_col,
                        score=best_score,
                        method=best_method,
                    )
                )
                available_pred_columns.remove(best_pred_col)
                method_str = (
                    best_method.value if best_method is not None else "incompatible"
                )
                logger.info(
                    f"Matched '{gt_col}' -> '{best_pred_col}' "
                    f"(score={best_score:.3f}, method={method_str})"
                )
            else:
                matches.append(
                    ColumnMatch(
                        gt_column=gt_col,
                        pred_column=None,
                        score=best_score,
                        method=None,
                    )
                )
                logger.warning(
                    f"No match found for '{gt_col}' (best score={best_score:.3f})"
                )

        return matches


class DataComparator:
    """
    Compare data between ground truth and predicted dataframes.

    Handles join completeness checking and per-column distribution comparison.
    """

    def __init__(
        self,
        categorical_threshold: int = 20,
        cross_encoder_model_name: str = "cross-encoder/stsb-roberta-base",
        match_threshold: float = 0.5,
        data_match_threshold: float = 0.7,
        categorical_mathch_threshold: float = 0.8,
    ) -> None:
        """
        Initialise the DataComparator.

        Args:
            categorical_threshold: Max unique values to treat a column as categorical.
            cross_encoder_model_name: Model for semantic column matching.
            match_threshold: Minimum score for column matching.
            data_match_threshold: Minimum score for data (data-based) matching.

        """
        self.categorical_threshold = categorical_threshold
        self.categorical_match_threshold = categorical_mathch_threshold
        self.data_match_threshold = data_match_threshold
        self.column_matcher = ColumnMatcher(
            cross_encoder_model_name=cross_encoder_model_name,
            match_threshold=match_threshold,
        )

    def _check_join_completeness(
        self,
        gt_df: pd.DataFrame,
        pred_df: pd.DataFrame,
    ) -> tuple[JoinCompleteness, pd.DataFrame]:
        """
        Join dataframes on index and check completeness.

        Returns:
            Tuple of (JoinCompleteness result, joined DataFrame).

        """
        gt_keys = gt_df.index
        pred_keys = pred_df.index

        # Check for duplicate keys
        gt_duplicate_keys = int(gt_keys.duplicated().sum())
        pred_duplicate_keys = int(pred_keys.duplicated().sum())

        if gt_duplicate_keys > 0:
            logger.warning(
                f"GT has {gt_duplicate_keys} duplicate primary keys "
                f"({gt_duplicate_keys / len(gt_df):.1%} of rows)"
            )

        if pred_duplicate_keys > 0:
            logger.warning(
                f"Pred has {pred_duplicate_keys} duplicate primary keys "
                f"({pred_duplicate_keys / len(pred_df):.1%} of rows)"
            )

        gt_keys_unique = set(gt_keys)
        pred_keys_unique = set(pred_keys)

        missing_in_pred = len(gt_keys_unique - pred_keys_unique)
        extra_in_pred = len(pred_keys_unique - gt_keys_unique)

        joined_df = gt_df.merge(
            pred_df,
            left_index=True,
            right_index=True,
            how="inner",
            suffixes=("_gt", "_pred"),
        )

        # Completeness: what proportion of GT rows have a match?
        # If there are duplicates, this can exceed 1.0, so we report raw and capped
        if len(gt_df) > 0:
            matched_keys = set(joined_df.index)
            gt_keys_unique = set(gt_keys)
            completeness_score = len(matched_keys) / len(gt_keys_unique)
        else:
            completeness_score = 0.0

        completeness = JoinCompleteness(
            gt_row_count=len(gt_df),
            pred_row_count=len(pred_df),
            joined_row_count=len(joined_df),
            missing_in_pred=missing_in_pred,
            extra_in_pred=extra_in_pred,
            gt_duplicate_keys=gt_duplicate_keys,
            pred_duplicate_keys=pred_duplicate_keys,
            completeness_score=completeness_score,
        )

        logger.info(
            f"Join completeness: {len(joined_df)}/{len(gt_df)} rows "
            f"({completeness_score:.1%}), "
            f"missing={missing_in_pred}, extra={extra_in_pred}, "
            f"gt_dupes={gt_duplicate_keys}, pred_dupes={pred_duplicate_keys}"
        )

        return completeness, joined_df

    def _compute_numeric_similarity(
        self,
        gt_series: pd.Series,
        pred_series: pd.Series,
    ) -> float:
        """
        Compute similarity score for two numeric columns based on normalised RMSE.

        Uses _compare_numeric internally and converts RMSE to a [0, 1] similarity.

        Returns:
            Score in [0, 1] where 1 is perfect match.

        """
        comparison = self._compare_numeric(gt_series, pred_series)

        if np.isnan(comparison.rmse):
            return 0.0

        # Normalise RMSE by the range of gt values
        gt_range = comparison.gt_std * 4  # ~99.7% of data for normal distribution
        if gt_range == 0:
            # Constant column: perfect match if RMSE is 0
            return 1.0 if comparison.rmse == 0 else 0.0

        nrmse = comparison.rmse / gt_range

        return max(0.0, 1.0 - nrmse)

    def _compute_categorical_similarity(
        self,
        gt_series: pd.Series,
        pred_series: pd.Series,
    ) -> float:
        """
        Compute similarity score for two categorical columns.

        Uses _compare_categorical internally and combines metrics into a [0, 1] score.

        Returns:
            Score in [0, 1] where 1 is perfect match.

        """
        comparison = self._compare_categorical(gt_series, pred_series)

        # Weighted combination of metrics

        return (
            0.5 * comparison.exact_match_rate
            + 0.3 * comparison.distribution_similarity
            + 0.2 * comparison.category_overlap_score
        )

    def _infer_column_type(
        self,
        gt_series: pd.Series,
        pred_series: pd.Series | None = None,
    ) -> ColumnType:
        """
        Infer whether column should be treated as numeric or categorical.

        A column is categorical if:
        - It's non-numeric (object/string), OR
        - It's numeric but has <= categorical_threshold unique values.

        Args:
            gt_series: The ground truth (or single) series to check.
            pred_series: Optional predicted series. If provided, both must be
                         numeric for the result to be NUMERIC.

        """
        gt_is_numeric = pd.api.types.is_numeric_dtype(gt_series)

        if pred_series is not None:
            pred_is_numeric = pd.api.types.is_numeric_dtype(pred_series)
            if not gt_is_numeric or not pred_is_numeric:
                return ColumnType.CATEGORICAL
            pred_unique = pred_series.nunique()
        else:
            # Single series mode: only check gt
            if not gt_is_numeric:
                return ColumnType.CATEGORICAL
            pred_unique = float("inf")  # Won't trigger categorical threshold

        gt_unique = gt_series.nunique()

        if (
            gt_unique <= self.categorical_threshold
            or pred_unique <= self.categorical_threshold
        ):
            return ColumnType.CATEGORICAL

        return ColumnType.NUMERIC

    def _compare_numeric(
        self,
        gt_series: pd.Series,
        pred_series: pd.Series,
    ) -> NumericComparison:
        """Compare two numeric columns using RMSE, MAE, and correlation."""
        # TO DO: is this the correct thing to do with NaNs?
        # Drop rows where either is NaN for comparison
        mask = gt_series.notna() & pred_series.notna()
        gt_clean = gt_series[mask].astype(float)
        pred_clean = pred_series[mask].astype(float)

        if len(gt_clean) == 0:
            return NumericComparison(
                rmse=float("nan"),
                mae=float("nan"),
                correlation=None,
                gt_mean=float("nan"),
                pred_mean=float("nan"),
                gt_std=float("nan"),
                pred_std=float("nan"),
            )

        diff = gt_clean - pred_clean
        rmse = float(np.sqrt((diff**2).mean()))
        mae = float(diff.abs().mean())

        # Correlation (handle constant columns)
        try:
            if gt_clean.std() > 0 and pred_clean.std() > 0:
                correlation = float(gt_clean.corr(pred_clean))
            else:
                correlation = None
        # TO DO: are we just ignoring this exception here?
        except Exception as e:  # noqa: BLE001
            error_msg = f"Error computing correlation: {e}"
            logger.exception(error_msg)
            correlation = None

        return NumericComparison(
            rmse=rmse,
            mae=mae,
            correlation=correlation,
            gt_mean=float(gt_clean.mean()),
            pred_mean=float(pred_clean.mean()),
            gt_std=float(gt_clean.std()),
            pred_std=float(pred_clean.std()),
        )

    def _jensen_shannon_divergence(
        self,
        p: dict[str, float],
        q: dict[str, float],
    ) -> float:
        """
        Calculate Jensen-Shannon divergence between two distributions.

        Args:
            p: First distribution (category -> probability).
            q: Second distribution (category -> probability).

        Returns:
            JS divergence in [0, 1]. Lower = more similar.

        """
        all_categories = set(p.keys()) | set(q.keys())

        p_vec = np.array([p.get(c, 0.0) for c in all_categories])
        q_vec = np.array([q.get(c, 0.0) for c in all_categories])

        # Add small epsilon to avoid log(0)
        eps = 1e-10
        p_vec = p_vec + eps
        q_vec = q_vec + eps

        # Normalise
        p_vec = p_vec / p_vec.sum()
        q_vec = q_vec / q_vec.sum()

        # JS divergence = 0.5 * KL(p||m) + 0.5 * KL(q||m) where m = (p+q)/2
        m_vec = 0.5 * (p_vec + q_vec)

        kl_p_m = float((p_vec * np.log(p_vec / m_vec)).sum())
        kl_q_m = float((q_vec * np.log(q_vec / m_vec)).sum())

        return 0.5 * kl_p_m + 0.5 * kl_q_m

    def _compare_categorical(
        self,
        gt_series: pd.Series,
        pred_series: pd.Series,
    ) -> CategoricalComparison:
        """
        Compare two categorical columns using exact match rate and distribution
        similarity.
        """
        # Convert to string for consistent comparison
        gt_str = gt_series.astype(str).fillna("__NA__")
        pred_str = (
            pred_series.astype(str).fillna("__NA__").copy()
        )  # copy to avoid SettingWithCopyWarning

        # produce category sets
        gt_categories = set(gt_str.unique())
        pred_categories = set(pred_str.unique())

        # now for each category set in gt_categories look at what they are mapped
        # to based on primary key
        # series already aligned via primary key join
        category_mapping = {}
        for category in gt_categories:
            mapped_values = pred_str[gt_str == category]
            value_counts = mapped_values.value_counts(normalize=True).to_dict()
            category_mapping[category] = value_counts

        # Sort categories by their best match score (descending) for greedy matching
        categories_with_best_scores = [
            (category, max(mapping.values()) if mapping else 0.0)
            for category, mapping in category_mapping.items()
        ]
        categories_sorted = sorted(
            categories_with_best_scores, key=lambda x: x[1], reverse=True
        )

        # Track which pred categories have been used
        available_pred_categories = set(pred_categories)
        final_mapping: dict[str, str] = {}

        # Use greedy algorithm to map categories (highest scores first)
        for category, _ in categories_sorted:
            mapping = category_mapping[category]
            # Find the best available match
            best_match = None
            best_score = 0.0
            for pred_cat in available_pred_categories:
                score = mapping.get(pred_cat, 0.0)
                if score > best_score:
                    best_score = score
                    best_match = pred_cat

            # Only map if above threshold
            if (
                best_match is not None
                and best_score >= self.categorical_match_threshold
            ):
                logger.info(
                    f"Mapping '{category}' to '{best_match}' with similarity "
                    f"{best_score:.2f}"
                )
                final_mapping[best_match] = category
                available_pred_categories.remove(best_match)
            else:
                logger.debug(
                    f"No mapping for '{category}' (best score={best_score:.2f} "
                    f"< threshold={self.categorical_match_threshold})"
                )

        # Apply the mapping to pred_str
        pred_str_mapped = pred_str.map(lambda x: final_mapping.get(x, x))

        # Exact match rate
        exact_match_rate = float((gt_str == pred_str_mapped).mean())

        # Category sets after mapping
        pred_categories_mapped = set(pred_str_mapped.unique())
        missing_categories = gt_categories - pred_categories_mapped
        extra_categories = pred_categories_mapped - gt_categories

        # Jaccard similarity of category sets
        intersection = len(gt_categories & pred_categories_mapped)
        union = len(gt_categories | pred_categories_mapped)
        category_overlap_score = intersection / union if union > 0 else 0.0

        # Distribution similarity using Jensen-Shannon divergence
        gt_dist = gt_str.value_counts(normalize=True).to_dict()
        pred_dist = pred_str_mapped.value_counts(normalize=True).to_dict()

        js_divergence = self._jensen_shannon_divergence(gt_dist, pred_dist)
        distribution_similarity = 1.0 - js_divergence  # Convert to similarity

        return CategoricalComparison(
            exact_match_rate=exact_match_rate,
            gt_categories=gt_categories,
            pred_categories=pred_categories,
            missing_categories=missing_categories,
            extra_categories=extra_categories,
            category_overlap_score=category_overlap_score,
            distribution_similarity=distribution_similarity,
        )

    def _compare_column_pair(
        self,
        joined_df: pd.DataFrame,
        gt_column: str,
        pred_column: str,
    ) -> ColumnComparison:
        """
        Compare a single matched column pair in the joined dataframe.

        Handles column suffixes from merge (_gt, _pred).
        """
        # Determine actual column names in joined df
        if f"{gt_column}_gt" in joined_df.columns:
            gt_col_name = f"{gt_column}_gt"
        else:
            gt_col_name = gt_column

        if f"{pred_column}_pred" in joined_df.columns:
            pred_col_name = f"{pred_column}_pred"
        else:
            pred_col_name = pred_column

        gt_series = joined_df[gt_col_name]
        pred_series = joined_df[pred_col_name]

        col_type = self._infer_column_type(gt_series, pred_series)

        if col_type == ColumnType.NUMERIC:
            numeric_comparison = self._compare_numeric(gt_series, pred_series)
            logger.info(
                f"Numeric comparison '{gt_column}': "
                f"RMSE={numeric_comparison.rmse:.4f}, "
                f"corr={numeric_comparison.correlation}"
            )
            return ColumnComparison(
                gt_column=gt_column,
                pred_column=pred_column,
                column_type=col_type,
                numeric_comparison=numeric_comparison,
            )
        else:
            categorical_comparison = self._compare_categorical(gt_series, pred_series)
            logger.info(
                f"Categorical comparison '{gt_column}': "
                f"exact_match={categorical_comparison.exact_match_rate:.1%}, "
                f"dist_sim={categorical_comparison.distribution_similarity:.3f}"
            )
            return ColumnComparison(
                gt_column=gt_column,
                pred_column=pred_column,
                column_type=col_type,
                categorical_comparison=categorical_comparison,
            )

    def _data_match_columns(  # noqa: PLR0912
        self,
        joined_df: pd.DataFrame,
        unmatched_gt_columns: list[str],
        unmatched_pred_columns: list[str],
    ) -> list[ColumnMatch]:
        """
        Match unmatched columns purely by data similarity (ignoring column names).

        For each unmatched GT column:
        - If numeric: find the pred column with lowest RMSE (highest similarity)
        - If categorical: find the pred column with highest categorical similarity

        Uses greedy matching: once a pred column is matched, it's removed.

        Args:
            joined_df: The joined dataframe (with _gt and _pred suffixes).
            unmatched_gt_columns: GT columns that weren't matched by name.
            unmatched_pred_columns: Pred columns that weren't matched by name.

        Returns:
            List of ColumnMatch objects for data matches.

        """
        if not unmatched_gt_columns or not unmatched_pred_columns:
            logger.info("No unmatched columns for data matching")
            return []

        logger.info(
            f"Attempting data match for {len(unmatched_gt_columns)} GT columns "
            f"against {len(unmatched_pred_columns)} pred columns"
        )

        data_matches: list[ColumnMatch] = []
        available_pred_cols = set(unmatched_pred_columns)

        # Build similarity matrix for all unmatched pairs
        similarity_matrix: dict[str, dict[str, tuple[float, MatchMethod | None]]] = {}

        for gt_col in unmatched_gt_columns:
            # Get GT series from joined df
            # TO DO: do we need to handle suffixes?
            # I think not, since that would not occur in unmatched columns
            # for our greedy matching as we sort by match score
            gt_series = joined_df[gt_col]
            gt_type = self._infer_column_type(gt_series)

            similarity_matrix[gt_col] = {}

            for pred_col in unmatched_pred_columns:
                # TO DO: do we need to handle suffixes?
                # I think not, since that would not occur in unmatched columns
                # for our greedy matching as we sort by match score
                pred_series = joined_df[pred_col]
                pred_type = self._infer_column_type(pred_series)

                # Only compare compatible types
                if gt_type == ColumnType.NUMERIC and pred_type == ColumnType.NUMERIC:
                    score = self._compute_numeric_similarity(gt_series, pred_series)
                    method = MatchMethod.DATA_NUMERIC
                elif (
                    gt_type == ColumnType.CATEGORICAL
                    and pred_type == ColumnType.CATEGORICAL
                ):
                    score = self._compute_categorical_similarity(gt_series, pred_series)
                    method = MatchMethod.DATA_CATEGORICAL
                else:
                    # Incompatible types: zero similarity
                    score = 0.0
                    method = None

                similarity_matrix[gt_col][pred_col] = (score, method)
                logger.debug(
                    f"data similarity '{gt_col}' <-> '{pred_col}': "
                    f"{score:.3f} ({method.value if method else 'incompatible'})"
                )

        # Greedy matching: sort GT columns by their best score descending
        gt_best_scores = []
        for gt_col in unmatched_gt_columns:
            if similarity_matrix[gt_col]:
                best = max(similarity_matrix[gt_col].values(), key=lambda x: x[0])
                gt_best_scores.append((gt_col, best[0]))
            else:
                gt_best_scores.append((gt_col, 0.0))

        # start with the columns with the highest scores
        gt_sorted = sorted(gt_best_scores, key=lambda x: x[1], reverse=True)

        for gt_col, _ in gt_sorted:
            best_pred_col = None
            best_score = 0.0
            best_method = None

            for pred_col in available_pred_cols:
                score, method = similarity_matrix[gt_col].get(pred_col, (0.0, None))
                if score > best_score:
                    best_score = score
                    best_pred_col = pred_col
                    best_method = method

            if best_score >= self.data_match_threshold and best_pred_col is not None:
                data_matches.append(
                    ColumnMatch(
                        gt_column=gt_col,
                        pred_column=best_pred_col,
                        score=best_score,
                        method=best_method,
                    )
                )
                available_pred_cols.remove(best_pred_col)
                logger.info(
                    f"data matched '{gt_col}' -> '{best_pred_col}' "
                    f"(score={best_score:.3f}, "
                    f"method={best_method.value if best_method else 'N/A'})"
                )
            else:
                data_matches.append(
                    ColumnMatch(
                        gt_column=gt_col,
                        pred_column=None,
                        score=best_score,
                        method=None,
                    )
                )
                logger.warning(
                    f"data match failed for '{gt_col}' (best score={best_score:.3f})"
                )

        return data_matches

    def compare(
        self,
        gt_df: pd.DataFrame,
        pred_df: pd.DataFrame,
        semantic_weighting: float = 1.0,  # TO DO add to config
        *,
        use_data_matching: bool = True,
    ) -> DataComparisonResult:
        """
        Perform full comparison between ground truth and predicted dataframes.

        Args:
            gt_df: Ground truth dataframe.
            pred_df: Predicted/generated dataframe.
            use_data_matching: If True, attempt data matching for unmatched columns.

        Returns:
            DataComparisonResult with all comparison metrics.

        """
        logger.info("Starting data comparison")
        # Ensure both dataframes have an index (primary key)
        if gt_df.index.name is None:
            error_msg = "Ground truth dataframe must have an index as primary key"
            raise ValueError(error_msg)
        if pred_df.index.name is None:
            error_msg = "Predicted dataframe must have an index as primary key"
            raise ValueError(error_msg)

        primary_key = gt_df.index.name + " | " + pred_df.index.name

        # Step 1: Check join completeness
        completeness, joined_df = self._check_join_completeness(gt_df, pred_df)

        # Step 2: Match columns by name
        column_matches = self.column_matcher.match_columns(
            gt_df, pred_df, semantic_weighting
        )

        # Identify initially unmatched columns
        matched_gt_cols = {
            m.gt_column for m in column_matches if m.pred_column is not None
        }
        matched_pred_cols = {
            m.pred_column for m in column_matches if m.pred_column is not None
        }

        unmatched_gt = [c for c in gt_df.columns if c not in matched_gt_cols]
        unmatched_pred = [c for c in pred_df.columns if c not in matched_pred_cols]

        # Step 3: data matching for unmatched columns
        if use_data_matching and unmatched_gt and unmatched_pred:
            data_matches = self._data_match_columns(
                joined_df, unmatched_gt, unmatched_pred
            )

            # Update column_matches with data matches
            # Remove unmatched entries for GT columns that now have data matches
            data_matched_gt = {
                m.gt_column for m in data_matches if m.pred_column is not None
            }
            column_matches = [
                m for m in column_matches if m.gt_column not in data_matched_gt
            ]
            column_matches.extend(data_matches)

            # Update unmatched lists
            matched_pred_cols.update(
                m.pred_column for m in data_matches if m.pred_column is not None
            )

        # Step 4: Compare all matched column pairs
        column_comparisons: list[ColumnComparison] = []

        for match in column_matches:
            if match.pred_column is not None:
                comparison = self._compare_column_pair(
                    joined_df,
                    match.gt_column,
                    match.pred_column,
                )
                column_comparisons.append(comparison)

        # Final unmatched columns
        final_unmatched_gt = [
            m.gt_column for m in column_matches if m.pred_column is None
        ]
        final_unmatched_pred = [
            c for c in pred_df.columns if c not in matched_pred_cols
        ]

        return DataComparisonResult(
            primary_key=primary_key,
            join_completeness=completeness,
            column_matches=column_matches,
            column_comparisons=column_comparisons,
            unmatched_gt_columns=final_unmatched_gt,
            unmatched_pred_columns=final_unmatched_pred,
        )


def print_comparison_report(result: DataComparisonResult) -> None:  # noqa: PLR0915
    """Print a human-readable comparison report using rich."""
    from rich.console import Console
    from rich.panel import Panel
    from rich.table import Table
    from rich.text import Text

    console = Console()

    # Header
    console.print()
    console.rule("[bold blue]DATA COMPARISON REPORT[/bold blue]")
    console.print(f"[bold]Primary Key Join:[/bold] {result.primary_key}\n")

    # Join Completeness Panel
    jc = result.join_completeness
    completeness_text = Text()
    completeness_text.append(f"Ground truth rows:    {jc.gt_row_count}\n")
    completeness_text.append(f"Predicted rows:       {jc.pred_row_count}\n")
    completeness_text.append(f"Joined rows:          {jc.joined_row_count}\n")
    completeness_text.append(
        f"Missing in pred:      {jc.missing_in_pred}",
        style="red" if jc.missing_in_pred > 0 else "green",
    )
    completeness_text.append("\n")
    completeness_text.append(
        f"Extra in pred:        {jc.extra_in_pred}",
        style="yellow" if jc.extra_in_pred > 0 else "green",
    )
    completeness_text.append("\n")
    completeness_text.append(
        f"GT duplicate keys:    {jc.gt_duplicate_keys}",
        style="red" if jc.gt_duplicate_keys > 0 else "green",
    )
    completeness_text.append("\n")
    completeness_text.append(
        f"Pred duplicate keys:  {jc.pred_duplicate_keys}",
        style="red" if jc.pred_duplicate_keys > 0 else "green",
    )
    completeness_text.append("\n")
    completeness_text.append(
        f"Completeness:         {jc.completeness_score:.1%}",
        style="green" if jc.completeness_score == 1.0 else "yellow",
    )

    if jc.has_duplicates:
        completeness_text.append("\n\n")
        completeness_text.append(
            "⚠ Duplicate keys detected! Join may have inflated row count.",
            style="bold red",
        )

    console.print(
        Panel(
            completeness_text,
            title="[bold]Join Completeness[/bold]",
            border_style="blue",
        )
    )

    # Column Matches Table
    match_table = Table(
        title="Column Matches", show_header=True, header_style="bold cyan"
    )
    match_table.add_column("GT Column", style="white")
    match_table.add_column("Pred Column", style="white")
    match_table.add_column("Score", justify="right")
    match_table.add_column("Method", style="dim")

    # TO DO: using scores for colouring
    for match in result.column_matches:
        if match.pred_column:
            score_style = (
                "green"
                if match.score >= 0.8  # noqa: PLR2004
                else "yellow"
                if match.score >= 0.5  # noqa: PLR2004
                else "red"
            )
            match_table.add_row(
                match.gt_column,
                match.pred_column,
                f"[{score_style}]{match.score:.3f}[/{score_style}]",
                match.method.value if match.method else "-",
            )
        else:
            match_table.add_row(
                match.gt_column,
                "[red](unmatched)[/red]",
                f"[red]{match.score:.3f}[/red]",
                "-",
            )

    console.print(match_table)
    console.print()

    # Column Comparisons
    for comp in result.column_comparisons:
        comp_table = Table(
            title=f"{comp.gt_column} ↔ {comp.pred_column} ({comp.column_type.value})",
            show_header=True,
            header_style="bold magenta",
        )
        comp_table.add_column("Metric", style="white")
        comp_table.add_column("Value", justify="right")

        if comp.numeric_comparison:
            nc = comp.numeric_comparison
            comp_table.add_row("RMSE", f"{nc.rmse:.4f}")
            comp_table.add_row("MAE", f"{nc.mae:.4f}")
            corr_str = f"{nc.correlation:.4f}" if nc.correlation is not None else "N/A"
            comp_table.add_row("Correlation", corr_str)
            comp_table.add_row("GT mean / std", f"{nc.gt_mean:.4f} / {nc.gt_std:.4f}")
            comp_table.add_row(
                "Pred mean / std", f"{nc.pred_mean:.4f} / {nc.pred_std:.4f}"
            )

        if comp.categorical_comparison:
            cc = comp.categorical_comparison
            match_style = (
                "green"
                if cc.exact_match_rate >= 0.9  # noqa: PLR2004
                else "yellow"
                if cc.exact_match_rate >= 0.7  # noqa: PLR2004
                else "red"
            )
            comp_table.add_row(
                "Exact match rate",
                f"[{match_style}]{cc.exact_match_rate:.1%}[/{match_style}]",
            )
            comp_table.add_row("Category overlap", f"{cc.category_overlap_score:.3f}")
            comp_table.add_row(
                "Distribution similarity", f"{cc.distribution_similarity:.3f}"
            )
            comp_table.add_row(
                "Missing categories",
                str(cc.missing_categories)
                if cc.missing_categories
                else "[green]none[/green]",
            )
            comp_table.add_row(
                "Extra categories",
                str(cc.extra_categories)
                if cc.extra_categories
                else "[green]none[/green]",
            )

        console.print(comp_table)
        console.print()

    # Unmatched columns
    if result.unmatched_gt_columns:
        console.print(
            Panel(
                "\n".join(result.unmatched_gt_columns),
                title="[bold red]Unmatched GT Columns[/bold red]",
                border_style="red",
            )
        )

    if result.unmatched_pred_columns:
        console.print(
            Panel(
                "\n".join(result.unmatched_pred_columns),
                title="[bold yellow]Unmatched Pred Columns[/bold yellow]",
                border_style="yellow",
            )
        )

    console.rule("[bold blue]END REPORT[/bold blue]")
    console.print()


if __name__ == "__main__":
    from pathlib import Path

    for output_dir in Path("tmp/smolagent_context").glob("sample*/data/output"):
        gt_file = output_dir / "output.csv"
        pred_file = output_dir / "cleaned_data.csv"

        if not gt_file.exists() or not pred_file.exists():
            logger.warning(f"Skipping {output_dir}: missing files")
            continue

        logger.info(f"\n\nProcessing: {output_dir}")

        # requirement that first column is primary key
        gt_df = pd.read_csv(gt_file, index_col=0)
        pred_df = pd.read_csv(pred_file, index_col=0)

        comparator = DataComparator(
            categorical_threshold=20,
            match_threshold=0.5,
        )

        result = comparator.compare(gt_df, pred_df)
        print_comparison_report(result)
