"""Statistical comparison functions for numeric and categorical data."""

import numpy as np
import pandas as pd
from loguru import logger

from .models import CategoricalComparison, ColumnType, NumericComparison


def infer_column_type(
    gt_series: pd.Series,
    pred_series: pd.Series | None = None,
    categorical_threshold: int = 20,
) -> ColumnType:
    """
    Infer whether column should be treated as numeric or categorical.

    Args:
        gt_series: The ground truth series.
        pred_series: Optional predicted series.
        categorical_threshold: Max unique values for categorical.

    """
    gt_is_numeric = pd.api.types.is_numeric_dtype(gt_series)

    # TO DO: something to consider here, if the data types are different then
    # maybe column
    # should not be a match. I.e. we match on colum names first, then check types.
    # and if there is no type match then we do not consider it a match.
    if pred_series is not None:
        pred_is_numeric = pd.api.types.is_numeric_dtype(pred_series)
        if not gt_is_numeric or not pred_is_numeric:
            return ColumnType.CATEGORICAL
        pred_unique = pred_series.nunique()
    else:
        if not gt_is_numeric:
            return ColumnType.CATEGORICAL
        pred_unique = float("inf")

    gt_unique = gt_series.nunique()

    if gt_unique <= categorical_threshold or pred_unique <= categorical_threshold:
        return ColumnType.CATEGORICAL

    return ColumnType.NUMERIC


def compare_numeric(
    gt_series: pd.Series,
    pred_series: pd.Series,
) -> NumericComparison:
    """Compare two numeric columns using RMSE, MAE, and correlation."""
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

    try:
        if gt_clean.std() > 0 and pred_clean.std() > 0:
            correlation = float(gt_clean.corr(pred_clean))
        else:
            correlation = None
    except Exception as e:  # noqa: BLE001
        logger.exception(f"Error computing correlation: {e}")
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


def compute_numeric_similarity(
    gt_series: pd.Series,
    pred_series: pd.Series,
) -> float:
    """Compute similarity score for numeric columns based on normalised RMSE."""
    comparison = compare_numeric(gt_series, pred_series)

    if np.isnan(comparison.rmse):
        return 0.0

    gt_range = comparison.gt_std * 4
    if gt_range == 0:
        return 1.0 if comparison.rmse == 0 else 0.0

    nrmse = comparison.rmse / gt_range
    return max(0.0, 1.0 - nrmse)


# TO DO: is this the correct metric to use here?
def jensen_shannon_divergence(
    p: dict[str, float],
    q: dict[str, float],
) -> float:
    """Calculate Jensen-Shannon divergence between two distributions."""
    all_categories = set(p.keys()) | set(q.keys())

    p_vec = np.array([p.get(c, 0.0) for c in all_categories])
    q_vec = np.array([q.get(c, 0.0) for c in all_categories])

    eps = 1e-10
    p_vec = p_vec + eps
    q_vec = q_vec + eps

    p_vec = p_vec / p_vec.sum()
    q_vec = q_vec / q_vec.sum()

    m_vec = 0.5 * (p_vec + q_vec)

    kl_p_m = float((p_vec * np.log(p_vec / m_vec)).sum())
    kl_q_m = float((q_vec * np.log(q_vec / m_vec)).sum())

    return 0.5 * kl_p_m + 0.5 * kl_q_m


def compare_categorical(
    gt_series: pd.Series,
    pred_series: pd.Series,
    categorical_match_threshold: float = 0.8,
) -> CategoricalComparison:
    """Compare two categorical columns with automatic category mapping."""
    gt_str = gt_series.astype(str).fillna("__NA__")
    pred_str = pred_series.astype(str).fillna("__NA__").copy()

    gt_categories = set(gt_str.unique())
    pred_categories = set(pred_str.unique())

    # Build category mapping based on co-occurrence
    category_mapping = {}
    for category in gt_categories:
        mapped_values = pred_str[gt_str == category]
        value_counts = mapped_values.value_counts(normalize=True).to_dict()
        category_mapping[category] = value_counts

    # Greedy matching for categories
    categories_with_best_scores = [
        (category, max(mapping.values()) if mapping else 0.0)
        for category, mapping in category_mapping.items()
    ]
    categories_sorted = sorted(
        categories_with_best_scores, key=lambda x: x[1], reverse=True
    )

    available_pred_categories = set(pred_categories)
    final_mapping: dict[str, str] = {}

    for category, _ in categories_sorted:
        mapping = category_mapping[category]
        best_match = None
        best_score = 0.0
        for pred_cat in available_pred_categories:
            score = mapping.get(pred_cat, 0.0)
            if score > best_score:
                best_score = score
                best_match = pred_cat

        if best_match is not None and best_score >= categorical_match_threshold:
            logger.info(
                f"Mapping '{category}' to '{best_match}' with similarity "
                f"{best_score:.2f}"
            )
            final_mapping[best_match] = category
            available_pred_categories.remove(best_match)
        else:
            logger.debug(f"No mapping for '{category}' (best score={best_score:.2f})")

    pred_str_mapped = pred_str.map(lambda x: final_mapping.get(x, x))

    exact_match_rate = float((gt_str == pred_str_mapped).mean())

    pred_categories_mapped = set(pred_str_mapped.unique())
    missing_categories = gt_categories - pred_categories_mapped
    extra_categories = pred_categories_mapped - gt_categories

    intersection = len(gt_categories & pred_categories_mapped)
    union = len(gt_categories | pred_categories_mapped)
    category_overlap_score = intersection / union if union > 0 else 0.0

    gt_dist = gt_str.value_counts(normalize=True).to_dict()
    pred_dist = pred_str_mapped.value_counts(normalize=True).to_dict()

    js_divergence = jensen_shannon_divergence(gt_dist, pred_dist)
    distribution_similarity = 1.0 - js_divergence

    return CategoricalComparison(
        exact_match_rate=exact_match_rate,
        gt_categories=gt_categories,
        pred_categories=pred_categories,
        missing_categories=missing_categories,
        extra_categories=extra_categories,
        category_overlap_score=category_overlap_score,
        distribution_similarity=distribution_similarity,
    )


# TO DO: i think this is fairly arbirtrary weighting
def compute_categorical_similarity(
    gt_series: pd.Series,
    pred_series: pd.Series,
    categorical_match_threshold: float = 0.8,
) -> float:
    """Compute similarity score for categorical columns."""
    comparison = compare_categorical(
        gt_series, pred_series, categorical_match_threshold
    )

    return (
        0.5 * comparison.exact_match_rate
        + 0.3 * comparison.distribution_similarity
        + 0.2 * comparison.category_overlap_score
    )
