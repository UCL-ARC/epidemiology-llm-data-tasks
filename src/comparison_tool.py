"""Tool to compare data produced by running R code produced by agents with gold standard data."""

from pathlib import Path
from typing import Literal

import pandas as pd
from jellyfish import levenshtein_distance  # could implement other algos
from loguru import logger
from pydantic import BaseModel
from sentence_transformers import SentenceTransformer

DEFAULT_WEIGHTS = {"columns": 8.3}
FUZZY_MATCH_THRESHOLD = 0.8
SEMANTIC_MATCH_THRESHOLD = 0.85

embedding_model = SentenceTransformer("all-MiniLM-L6-v2")


class ValueMapping(BaseModel):
    """Output data model for ground truth <> pred value mapping."""

    gt_value_name: str
    pred_value_name: str
    match_method: Literal["determininstic", "fuzzy", "semantic"]
    match_value: float | None = None


class ColumnValueMapping(BaseModel):
    """Output data model for ground truth <> pred column value mapping."""

    gt_column_name: str
    pred_column_name: str
    value_map: list[ValueMapping]


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
        Return 'normalised' fuzzy string distance b/w 2 strings.

        There's an argument that normalising this isn't really that useful,
        as it's contingent on the number of characters.

        NOTE: Here, we're using Levenshtein distance.

        """
        return 1 - levenshtein_distance(
            self._normalise_string(string_a), self._normalise_string(string_b)
        ) / max(
            len(self._normalise_string(string_a)), len(self._normalise_string(string_b))
        )

    def _exact_string_similarity(self, string_a: str, string_b: str) -> bool:
        """Check if two strings are exact matches."""
        return string_a == string_b

    def compare_column_lengths(self) -> int:
        """Compare the number of columns between ground truth and pred."""
        return len(self.ground_truth_df.columns) - len(self.pred_df.columns)

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
        self,
        column_names_mapping: dict | None = None,
        fuzzy_match_threshold: int = FUZZY_MATCH_THRESHOLD,
        semantic_match_threshold: int = SEMANTIC_MATCH_THRESHOLD,
    ) -> list[ColumnValueMapping]:
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
        if not column_names_mapping:
            column_names_mapping = {x: x for x in self.ground_truth_df.columns}

        logger.debug(column_names_mapping)

        mappings = []
        for gt_column, pred_column in column_names_mapping.items():
            # ensure both columns exist
            try:
                self.ground_truth_df[gt_column] is not None
                self.pred_df[pred_column] is not None
            except KeyError as e:
                logger.error(e)
                continue

            # find matches for unique value labels in each df
            column_maps = []
            # cast to str to make pydantic model easier.
            gt_values = [str(x) for x in self.ground_truth_df[gt_column].unique()]
            pred_values = [str(x) for x in self.ground_truth_df[pred_column].unique()]
            logger.debug(
                f"ground truth column {gt_column} has {len(gt_values)} unique values."
            )
            logger.debug(
                f"pred column {pred_column} has {len(pred_values)} unique values."
            )

            for gt_val in gt_values:
                # perform comparison in 3 steps of increasing complexity
                match = None
                for pred_val in pred_values:
                    if gt_val == pred_val:
                        match = ValueMapping(
                            gt_value_name=gt_val,
                            pred_value_name=pred_val,
                            match_method="determininstic",
                        )
                        break
                    fuzzy_score = self._fuzzy_string_similarity(gt_val, pred_val)
                    if fuzzy_score >= fuzzy_match_threshold:
                        match = ValueMapping(
                            gt_value_name=gt_val,
                            pred_value_name=pred_val,
                            match_method="fuzzy",
                            match_value=fuzzy_score,
                        )
                        break
                    sem_score = self._semantic_string_similarity(gt_val, pred_val)
                    if sem_score >= semantic_match_threshold:
                        match = ValueMapping(
                            gt_value_name=gt_val,
                            pred_value_name=pred_val,
                            match_method="semantic",
                            match_value=sem_score,
                        )
                        break
                if match:
                    column_maps.append(match)

            mappings.append(
                ColumnValueMapping(
                    gt_column_name=gt_column,
                    pred_column_name=pred_column,
                    value_map=column_maps,
                )
            )

        return mappings

    def _match_column_value_counts(self):
        pass
