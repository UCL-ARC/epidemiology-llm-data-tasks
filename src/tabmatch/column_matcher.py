"""Column matching using name similarity (Levenshtein and semantic)."""

import numpy as np
import pandas as pd
from jellyfish import levenshtein_distance
from loguru import logger
from sentence_transformers import CrossEncoder

from src.config import CROSS_ENCODER_MODEL, MATCH_THRESHOLD

from .models import ColumnMatch, MatchMethod


class ColumnMatcher:
    """
    Match columns between ground truth and predicted dataframes by name.

    Uses both Levenshtein distance and semantic similarity.
    """

    def __init__(
        self,
        cross_encoder_model_name: str = CROSS_ENCODER_MODEL,
        match_threshold: float = MATCH_THRESHOLD,
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

    def _levenshtein_similarities(self, pairs: list[tuple[str, str]]) -> list[float]:
        """Calculate normalised Levenshtein similarity for a batch of pairs."""
        return [self._levenshtein_similarity(a, b) for a, b in pairs]

    def _semantic_similarities(self, pairs: list[tuple[str, str]]) -> list[float]:
        """Calculate semantic similarity for a batch of pairs in one forward pass."""
        if not pairs:
            return []
        normalised = [[self._normalise(a), self._normalise(b)] for a, b in pairs]
        raw_scores = np.atleast_1d(self.cross_encoder.predict(normalised))
        return [float(max(0.0, min(1.0, s))) for s in raw_scores]

    def _semantic_similarity(self, a: str, b: str) -> float:
        """Calculate semantic similarity using cross-encoder (0-1)."""
        return self._semantic_similarities([(a, b)])[0]

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
        if not gt_columns or not pred_columns:
            return {gt_col: {} for gt_col in gt_columns}

        pairs = [
            (gt_col, pred_col) for gt_col in gt_columns for pred_col in pred_columns
        ]

        lev_scores = self._levenshtein_similarities(pairs)
        sem_scores = self._semantic_similarities(pairs)

        matrix: dict[str, dict[str, tuple[float, MatchMethod]]] = {}
        for (gt_col, pred_col), lev, sem in zip(
            pairs, lev_scores, sem_scores, strict=False
        ):
            if gt_col not in matrix:
                matrix[gt_col] = {}
            weighted_sem = sem * semantic_weighting
            if lev >= weighted_sem:
                matrix[gt_col][pred_col] = (lev, MatchMethod.LEVENSHTEIN)
            else:
                matrix[gt_col][pred_col] = (weighted_sem, MatchMethod.SEMANTIC)
            logger.debug(
                f"Similarity '{gt_col}' <-> '{pred_col}': "
                f"{matrix[gt_col][pred_col][0]:.3f} "
                f"({matrix[gt_col][pred_col][1].value})"
            )

        return matrix

    def match_columns(
        self,
        gt_df: pd.DataFrame | list[str],
        pred_df: pd.DataFrame | list[str],
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
        gt_columns = list(gt_df.columns) if isinstance(gt_df, pd.DataFrame) else gt_df
        pred_columns = (
            list(pred_df.columns) if isinstance(pred_df, pd.DataFrame) else pred_df
        )

        logger.info(
            f"Matching {len(gt_columns)} GT columns to {len(pred_columns)} pred columns"
        )

        similarity_matrix = self._compute_similarity_matrix(
            gt_columns, pred_columns, semantic_weighting
        )

        if not gt_columns or not pred_columns:
            return [
                ColumnMatch(gt_column=gt_col, pred_column=None, score=0.0, method=None)
                for gt_col in gt_columns
            ]

        if not gt_columns or not pred_columns:
            return [
                ColumnMatch(gt_column=gt_col, pred_column=None, score=0.0, method=None)
                for gt_col in gt_columns
            ]

        # Greedy matching: anchor best-scoring GT columns first
        matches: list[ColumnMatch] = []
        available_pred_columns = set(pred_columns)

        # Sort GT columns by their best available score (descending)
        gt_best_scores = [
            (gt_col, max(similarity_matrix[gt_col].values(), key=lambda x: x[0]))
            for gt_col in gt_columns
        ]
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
                method_str = best_method.value if best_method else "unknown"
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
