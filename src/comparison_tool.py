"""Tool to compare data produced by running R code produced by agents with gold standard data."""

from pathlib import Path
from typing import Literal

import pandas as pd
from jellyfish import levenshtein_distance  # could implement other algos
from loguru import logger
from pydantic import BaseModel
from sentence_transformers import SentenceTransformer

DEFAULT_WEIGHTS = {"columns": 8.3}
FUZZY_MATCH_THRESHOLD = 0.7

embedding_model = SentenceTransformer("all-MiniLM-L6-v2")


class EvalScoreConfig(BaseModel):
    """Configuration class to feed into a given comparison method."""

    metric: str


def check_file_type(filename: str, filetype: str = "csv") -> None:
    """Check a file has a given extension."""
    if filename.split(".")[-1] != filetype:
        bad_filetype = f"{filename} is not of filetype {filetype}"
        raise ValueError(bad_filetype)


class ComparisonTool:
    def __init__(
        self,
        ground_truth_data_path: Path,
        pred_data_path: Path,
        primary_key: str,
        weights: dict[str, float] = DEFAULT_WEIGHTS,
    ) -> None:
        """Class for performing comparison between gold standard and predicted datasets."""
        [
            check_file_type(file.name, "csv")  # type: ignore[func-returns-value]
            for file in [ground_truth_data_path, pred_data_path]
        ]
        self.ground_truth_df = pd.read_csv(ground_truth_data_path)
        self.pred_df = pd.read_csv(pred_data_path)

        if primary_key not in self.ground_truth_df.columns:
            missing_pk = f"primary key {primary_key} is not in ground truth columns."
            raise ValueError(missing_pk)

    def __call__(self) -> float:
        """Run default comparison flow and produce metric of similarity."""
        return 1.0

    @staticmethod
    def produce_comparison_accuracy_score() -> float:
        """Procude accuracy score."""
        # for now, return a dict of all scores; no amalgamation.
        # we can determine amalgamation later on.
        return 1.0

    @staticmethod
    def _normalise_string(s: str) -> str:
        """Normalise a string."""
        return str(s).lower().strip()

    def _semantic_string_similarity(self, string_a: str, string_b: str) -> float:
        """Calculate semantic string similarity between two normalised strings."""
        payload = [self._normalise_string(string_a), self._normalise_string(string_b)]
        embeddings = embedding_model.encode(payload)
        return float(embedding_model.similarity(embeddings, embeddings)[0][1])

    def _fuzzy_string_similarity(self, string_a: str, string_b: str) -> float:
        """
        Return normalised fuzzy string distance b/w 2 strings.

        NOTE: Here, we're using Levenshtein distance.

        """
        return levenshtein_distance(
            self._normalise_string, self._normalise_string
        ) / max(len(self._normalise_string), len(self._normalise_string))

    def compare_column_lengths(
        self, return_type: Literal["is_match", "difference"] = "is_match"
    ) -> bool | int:
        """Compare the number of columns between ground truth and pred."""
        if return_type == "is_match":
            return len(self.ground_truth_df.columns) == len(self.pred_df.columns)
        if return_type == "difference":
            return len(self.ground_truth_df.columns) - len(self.pred_df.columns)
        bad_return_type = (
            f"method {return_type} is not implemented. use `is_match` or `difference`."
        )
        raise NotImplementedError(bad_return_type)

    def compare_row_length(
        self, return_type: Literal["is_match", "difference"] = "is_match"
    ) -> bool | int:
        """Compare the number of rows between ground truth and pred."""
        if return_type == "is_match":
            return len(self.ground_truth_df) == len(self.pred_df)
        if return_type == "difference":
            return len(self.ground_truth_df) - len(self.pred_df)
        bad_return_type = (
            f"method {return_type} is not implemented. use `is_match` or `difference`."
        )
        raise NotImplementedError(bad_return_type)

    def match_column_value_names(
        self, column_names: list, column_mapping: dict | None = None
    ) -> bool:
        """
        Assess whether available value names match.

        Columns in gold_standard and pred match, e.g.
        gold_standard_df['sex'] = ['F', 'M']
        pred_df['gender'] = [0, 1]

        Different methods can be used to compare this,
        e.g. direct, fuzz, semantic.

        Args:
            column_names (list): _description_
            column_mapping (dict | None, optional): _description_. Defaults to None.

        """
        return False

    def _match_column_value_counts(self):
        pass
