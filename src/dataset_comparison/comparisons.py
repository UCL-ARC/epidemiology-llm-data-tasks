"""Statistical comparison functions for numeric and categorical data."""
# TO DO: CHECK NUMERIC METRICS

import numpy as np
import pandas as pd
from loguru import logger
from scipy.spatial.distance import jensenshannon

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
    data_match_threshold: float = 0,
) -> NumericComparison:
    """Compare two numeric columns using RMSE, MAE, and correlation."""
    mask = gt_series.notna() & pred_series.notna()
    gt_clean = gt_series[mask].astype(float)
    pred_clean = pred_series[mask].astype(float)

    if len(gt_clean) == 0:
        return NumericComparison(
            rmse=float("nan"),
            mae=float("nan"),
            nrmse=float("nan"),
            nmae=float("nan"),
            correlation=None,
            gt_mean=float("nan"),
            pred_mean=float("nan"),
            gt_std=float("nan"),
            pred_std=float("nan"),
            data_match=None,
        )

    diff = gt_clean - pred_clean
    rmse = float(np.sqrt((diff**2).mean()))
    mae = float(diff.abs().mean())

    iqr = float(np.percentile(gt_clean, 75) - np.percentile(gt_clean, 25))
    if iqr == 0:
        # Fall back to range if IQR is zero (e.g. heavily skewed/constant)
        gt_range = abs(float(gt_clean.max()) - float(gt_clean.min()))
        normaliser = gt_range
    else:
        normaliser = iqr

    if normaliser == 0:
        nrmse = 0.0 if rmse == 0 else float("inf")
        nmae = 0.0 if mae == 0 else float("inf")
    else:
        nrmse = rmse / normaliser
        nmae = mae / normaliser

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
        nrmse=nrmse,
        nmae=nmae,
        correlation=correlation,
        gt_mean=float(gt_clean.mean()),
        pred_mean=float(pred_clean.mean()),
        gt_std=float(gt_clean.std()),
        pred_std=float(pred_clean.std()),
        data_match=nrmse <= data_match_threshold,
    )


def compute_numeric_similarity(
    gt_series: pd.Series,
    pred_series: pd.Series,
) -> float:
    """Compute similarity score for numeric columns based on normalised RMSE."""
    comparison = compare_numeric(gt_series, pred_series)
    if comparison.nrmse is None or np.isnan(comparison.nrmse):
        return 0.0
    return max(0.0, 1.0 - comparison.nrmse)


def compute_js_similarity(p: dict, q: dict) -> float:
    """Compute similarity between two categorical distributions using Jensen-Shannon."""
    all_cats = sorted(set(p) | set(q))
    p_vec = np.array([p.get(c, 0.0) for c in all_cats]) + 1e-10
    q_vec = np.array([q.get(c, 0.0) for c in all_cats]) + 1e-10
    p_vec /= p_vec.sum()
    q_vec /= q_vec.sum()
    return 1.0 - float(jensenshannon(p_vec, q_vec))  # already [0,1]


def compare_categorical(
    gt_series: pd.Series,
    pred_series: pd.Series,
    categorical_match_threshold: float = 0.8,
    data_match_threshold: float = 1.0,
) -> CategoricalComparison:
    """Compare two categorical columns with automatic category mapping."""
    gt_str = gt_series.fillna("__NA__").astype(str)
    pred_str = pred_series.fillna("__NA__").astype(str).copy()

    gt_categories = set(gt_str.unique())
    pred_categories = set(pred_str.unique())

    # Get ground truth category counts
    gt_counts = gt_str.value_counts().to_dict()

    # Build category mapping based on co-occurrence
    category_mapping = {}
    for category in gt_categories:
        mapped_values = pred_str[gt_str == category]
        value_counts = mapped_values.value_counts(normalize=True).to_dict()
        category_mapping[category] = value_counts

    # Greedy matching for categories
    # Sort by best score (descending), then by ground truth count (descending)
    categories_with_best_scores = [
        (
            category,
            max(mapping.values()) if mapping else 0.0,
            gt_counts.get(category, 0),
        )
        for category, mapping in category_mapping.items()
    ]
    categories_sorted = sorted(
        categories_with_best_scores, key=lambda x: (x[1], x[2]), reverse=True
    )

    available_pred_categories = set(pred_categories)
    final_mapping: dict[str, str] = {}

    for category, _, _ in categories_sorted:
        mapping = category_mapping[category]
        best_match = None
        best_score = 0.0
        for pred_cat in available_pred_categories:
            score = mapping.get(pred_cat, 0.0)
            if score > best_score:
                best_score = score
                best_match = pred_cat

        if best_match is not None and best_score >= categorical_match_threshold:
            logger.debug(
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
    distribution_similarity = compute_js_similarity(gt_dist, pred_dist)

    return CategoricalComparison(
        exact_match_rate=exact_match_rate,
        gt_categories=gt_categories,
        pred_categories=pred_categories,
        missing_categories=missing_categories,
        extra_categories=extra_categories,
        category_overlap_score=category_overlap_score,
        distribution_similarity=distribution_similarity,
        data_match=exact_match_rate >= data_match_threshold,
        category_mapping=final_mapping,
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
