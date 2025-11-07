"""Tool to compare data produced by running R code produced by agents with gold standard data."""

from pathlib import Path

import pandas as pd
from loguru import logger
from pydantic import BaseModel

DEFAULT_WEIGHTS = {"columns": 8}


class EvalScoreConfig(BaseModel):
    """Configuration class to feed into a given comparison method."""

    metric: str


def check_file_type(filename: str, filetype: str = "csv") -> None:
    """Check a file has a given extension."""
    if not filename.split(".")[-1] == filetype:
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
        pass

    @staticmethod
    def produce_comparison_accuracy_score() -> float:
        """"""
        # for now, return a dict of all scores; no amalgamation.
        # we can determine amalgamation later on.
        pass

    def _semantic_column_match(self):
        pass

    def _fuzzy_column_match(self):
        pass

    def compare_columns(self):
        pass

    def compare_row_length(self):
        pass

    def _match_column_value_names(self):
        pass

    def _match_column_value_counts(self):
        pass
