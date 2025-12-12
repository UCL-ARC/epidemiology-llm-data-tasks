"""Column matching using name similarity (Levenshtein and semantic)."""

import pandas as pd
from jellyfish import levenshtein_distance
from loguru import logger
from sentence_transformers import CrossEncoder

from .models import ColumnMatch, MatchMethod


class ColumnMatcher:
    """
    Match columns between ground truth and predicted dataframes by name.

    Uses both Levenshtein distance and semantic similarity.
    """

    def __init__(
        self,
        cross_encoder_model_name: str = "cross-encoder/stsb-roberta-base",
        match_threshold: float = 0.5,
    ) -> None:
        """
        Initialise the ColumnMatcher.

        Args:
            cross_encoder_model_name: Name of the cross-encoder model.
            match_threshold: Minimum score (0-1) required for a valid match.

        """
        self.cross_encoder = CrossEncoder(cross_encoder_model_name)
        self.match_threshold = match_threshold

    @staticmethod
    def _normalise(s: str) -> str:
        """Normalise a string for comparison."""
        return str(s).lower().strip().replace("_", " ").replace("-", " ")

    def _levenshtein_similarity(self, a: str, b: str) -> float:
        """Calculate normalised Levenshtein similarity (0-1)."""
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
        """Calculate similarity using both methods and return the best score."""
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
        """Compute similarity scores for all column pairs."""
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
        Match ground truth columns to predicted columns using greedy algorithm.

        Args:
            gt_df: Ground truth dataframe.
            pred_df: Predicted/generated dataframe.
            semantic_weighting: Weight for semantic similarity scores.

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

        # Get best scores for each gt column
        gt_best_scores = [
            (gt_col, max(similarity_matrix[gt_col].values(), key=lambda x: x[0]))
            for gt_col in gt_columns
        ]

        # Greedy: start with the best column match
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
                method_str = best_method.value if best_method else "incompatible"
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
