"""
Tool to compare data produced by running
R code produced by agents with gold standard data.
"""

from pathlib import Path
from typing import Literal

import pandas as pd
from jellyfish import levenshtein_distance  # could implement other algos
from loguru import logger
from pydantic import BaseModel
from sentence_transformers import SentenceTransformer

DEFAULT_WEIGHTS = {"columns": 8.3}
MATCH_THRESHOLD = 0.75  # completely arbitrary, no idea what the best val is

embedding_model = SentenceTransformer("all-MiniLM-L6-v2")


class ValueMapping(BaseModel):
    """Output data model for ground truth <> pred value mapping."""

    gt_value_name: str
    pred_value_name: str | None
    match_method: Literal["deterministic", "fuzzy", "semantic", "no_match"]
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
    """
    Class for comparing a ground truth to predicted dataframe
    by means of computing several similarity metrics.
    """

    def __init__(
        self,
        ground_truth_data_path: Path,
        pred_data_path: Path,
        # primary_key: str,
        weights: dict[str, float] = DEFAULT_WEIGHTS,  # not implemented yet
    ) -> None:
        """Init ComparisonTool instance."""
        [
            check_file_type(file.name, "csv")  # type: ignore[func-returns-value]
            for file in [ground_truth_data_path, pred_data_path]
        ]
        self.ground_truth_df = pd.read_csv(ground_truth_data_path)
        self.pred_df = pd.read_csv(pred_data_path)

        # if primary_key not in self.ground_truth_df.columns:
        #     missing_pk = f"primary key {primary_key} is not in ground truth columns."
        #     raise ValueError(missing_pk)

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

    def _exact_string_similarity(self, string_a: str, string_b: str) -> float:
        """Check if two strings are exact matches."""
        return float(
            self._normalise_string(string_a) == self._normalise_string(string_b)
        )

    def calculate_normalised_string_similarity(
        self,
        string_a: str,
        string_b: str,
        method: Literal["determininstic", "fuzzy", "semantic"],
        **kwargs,
    ) -> float:
        """
        Calculate normalised (0-1) string similarity.
        (we also _normalise our strings, i.e. remove whitespace, to-lower).

        Add more methods as required, first implementing as _method.

        Args:
            string_a (str): string a
            string_b (str): string b
            method (Literal[&#39;determininstic&#39;,
            &#39;fuzzy&#39;, &#39;semantic&#39;]): permitted methods

        Raises:
            NotImplementedError: _description_

        Returns:
            float: b/w 0 and 1

        """
        if method == "deterministic":
            return self._exact_string_similarity(string_a, string_b)
        elif method == "fuzzy":
            return self._fuzzy_string_similarity(string_a, string_b)
        elif method == "semantic":
            return self._semantic_string_similarity(string_a, string_b)
        else:
            method_not_implemented = (
                f"method {method} is not implemented."  # noqa: S608
                " select one from determininstic, fuzzy, semantic."
            )
            raise NotImplementedError(method_not_implemented)

    def compare_column_lengths(self) -> int:
        """Compare the number of columns between ground truth and pred."""
        return len(self.ground_truth_df.columns) - len(self.pred_df.columns)

    def compare_row_length(
        self, return_type: Literal["is_match", "difference"] = "difference"
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

    def match_column_names(
        self, match_threshold: float = MATCH_THRESHOLD
    ) -> dict[str, str]:
        """
        Produce mapping of column names between gt and pred.

        NOTE: if we can't match a column, we simply don't add it to
        the output dict resulting from this method. this might be a bad
        choice.

        Args:
            match_threshold (float, optional): 0-1. Defaults to MATCH_THRESHOLD.

        Returns:
            dict[str, str]: a dict of column matches in gt vs pred.

        """
        column_names_mapping = {}
        for gt_column in self.ground_truth_df.columns:
            for method in ["deterministic", "fuzzy", "semantic"]:
                if gt_column in column_names_mapping:
                    logger.debug(f"gt col {gt_column} has alrdy been mapped")
                    continue

                for pred_column in self.pred_df.columns:
                    match = self.calculate_normalised_string_similarity(
                        string_a=gt_column,
                        string_b=pred_column,
                        method=method,  # type: ignore[arg-type]
                    )
                    if match > match_threshold:
                        column_names_mapping[gt_column] = pred_column
                        break

        return column_names_mapping

    def match_all_column_value_names(
        self,
        column_names_mapping: dict[str, str] | None = None,
        match_threshold: float = MATCH_THRESHOLD,  # we may want to implement diff
        # thresholds for diff methods
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
            if (
                gt_column not in self.ground_truth_df.columns
                and pred_column not in self.pred_df.columns
            ):
                logger.info("columns don't exist in df")
                continue

            # find matches for unique value labels in each df
            column_maps: list[ValueMapping] = []
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
                for method in ["deterministic", "fuzzy", "semantic"]:
                    if gt_val in [x.gt_value_name for x in column_maps]:
                        logger.debug(
                            f"val {gt_val} for gt col {gt_column} has alrdy been mapped"
                        )
                        continue

                    for pred_val in pred_values:
                        match = self.calculate_normalised_string_similarity(
                            string_a=gt_val,
                            string_b=pred_val,
                            method=method,  # type: ignore[arg-type]
                        )
                        if match > match_threshold:
                            column_maps.append(
                                ValueMapping(
                                    gt_value_name=gt_val,
                                    pred_value_name=pred_val,
                                    match_method=method,  # type: ignore[arg-type]
                                    match_value=match,
                                )
                            )
                            break

                    if method == "semantic" and match < match_threshold:  # bit clunky
                        # and needs changing if we add more methods. 'semantic' here is
                        # a placeholder for reaching the end of the line.
                        logger.debug(
                            f"unable to find a match for {gt_val}. writing None."
                        )
                        column_maps.append(
                            ValueMapping(
                                gt_value_name=gt_val,
                                pred_value_name=None,
                                match_method="no_match",  # type: ignore[arg-type]
                                match_value=None,
                            )
                        )

            mappings.append(
                ColumnValueMapping(
                    gt_column_name=gt_column,
                    pred_column_name=pred_column,
                    value_map=column_maps,
                )
            )

        return mappings

    def _match_column_value_counts(
        self, column_value_mapping: ColumnValueMapping
    ) -> dict[str, int]:
        """Compare column value counts in gt vs pred."""
        val_counts_gt = {
            str(k): v
            for k, v in self.ground_truth_df[column_value_mapping.gt_column_name]
            .value_counts()
            .to_dict()
            .items()
        }  # casting keys in both to strings to ensure consistency with
        # pydantic model.
        # we don't need to check for None; as it won't exist in the input data if
        # there's no column match.
        val_counts_pred = {
            str(k): v
            for k, v in self.pred_df[column_value_mapping.pred_column_name]
            .value_counts()
            .to_dict()
            .items()
        }
        out = {}
        for col_map in column_value_mapping.value_map:
            if col_map.match_method == "no_match" or col_map.pred_value_name is None:
                logger.debug(f"no match for {col_map.gt_value_name}. next.")
                continue

            try:
                diff = (
                    val_counts_gt[col_map.gt_value_name]
                    - val_counts_pred[col_map.pred_value_name]
                )
            except KeyError as e:
                logger.error("keyerror!")
                logger.error(e)
                continue
            out[col_map.gt_value_name] = diff

        return out

    def match_all_column_value_counts(
        self, column_value_map: list[ColumnValueMapping]
    ) -> dict[str, dict[str, int]]:
        """Run _match_column_value_counts for all data."""
        out = {}
        for col_val_map in column_value_map:
            col_val_count_diffs = self._match_column_value_counts(col_val_map)
            out[col_val_map.gt_column_name] = col_val_count_diffs

        return out
