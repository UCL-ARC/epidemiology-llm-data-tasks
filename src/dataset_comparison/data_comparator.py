"""Main data comparison orchestration."""

import pandas as pd
from loguru import logger

from .column_matcher import ColumnMatcher
from .comparisons import (
    compare_categorical,
    compare_numeric,
    compute_categorical_similarity,
    compute_numeric_similarity,
    infer_column_type,
)
from .models import (
    ColumnComparison,
    ColumnMatch,
    ColumnType,
    DataComparisonResult,
    JoinCompleteness,
    MatchMethod,
)


class DataComparator:
    """
    Compare data between ground truth and predicted dataframes.

    Handles join completeness checking and per-column distribution comparison.
    """

    def __init__(  # NOQA: PLR0913
        self,
        categorical_threshold: int = 20,
        cross_encoder_model_name: str = "cross-encoder/stsb-roberta-base",
        match_threshold: float = 0.5,
        data_match_threshold: float = 0.7,
        categorical_data_match_threshold: float = 1.0,
        numerical_data_match_threshold: float = 0.0,
        categorical_match_threshold: float = 0.8,
    ) -> None:
        """
        Initialise the DataComparator.

        Args:
            categorical_threshold: Max unique values for categorical treatment.
            cross_encoder_model_name: Model for semantic column matching.
            match_threshold: Minimum score for column name matching.
            data_match_threshold: Minimum score for data-based matching
            i.e. these two columns can be considered for comparison.
            categorical_data_match_threshold: Minimum score for categorical data match,
            i.e. will we report a match 1 implies every row matches.
            numerical_data_match_threshold: Minimum score for numeric data match,
            i.e. will we report a match 0 implies every row matches.
            categorical_match_threshold: Minimum score for category mapping.

        """
        self.categorical_threshold = categorical_threshold
        self.categorical_match_threshold = categorical_match_threshold
        # TO DO: rename as this data match threshold differes from the other
        self.data_match_threshold = data_match_threshold
        self.categorical_data_match_threshold = categorical_data_match_threshold
        self.numerical_data_match_threshold = numerical_data_match_threshold
        self.column_matcher = ColumnMatcher(
            cross_encoder_model_name=cross_encoder_model_name,
            match_threshold=match_threshold,
        )

    def _check_join_completeness(
        self,
        gt_df: pd.DataFrame,
        pred_df: pd.DataFrame,
    ) -> tuple[JoinCompleteness, pd.DataFrame]:
        """Join dataframes on index and check completeness."""
        gt_keys = gt_df.index
        pred_keys = pred_df.index

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

        if len(gt_df) > 0:
            matched_keys = set(joined_df.index)
            join_completeness_score = len(matched_keys) / len(gt_keys_unique)
        else:
            join_completeness_score = 0.0

        completeness = JoinCompleteness(
            gt_row_count=len(gt_df),
            pred_row_count=len(pred_df),
            joined_row_count=len(joined_df),
            missing_in_pred=missing_in_pred,
            extra_in_pred=extra_in_pred,
            gt_duplicate_keys=gt_duplicate_keys,
            pred_duplicate_keys=pred_duplicate_keys,
            join_completeness_score=join_completeness_score,
        )

        logger.info(
            f"Join completeness: {len(joined_df)}/{len(gt_df)} rows "
            f"({join_completeness_score:.1%}), "
            f"missing={missing_in_pred}, extra={extra_in_pred}"
        )

        return completeness, joined_df

    def _compare_column_pair(
        self,
        joined_df: pd.DataFrame,
        gt_column: str,
        pred_column: str,
    ) -> ColumnComparison:
        """Compare a single matched column pair in the joined dataframe."""
        gt_col_name = (
            f"{gt_column}_gt" if f"{gt_column}_gt" in joined_df.columns else gt_column
        )
        pred_col_name = (
            f"{pred_column}_pred"
            if f"{pred_column}_pred" in joined_df.columns
            else pred_column
        )

        gt_series = joined_df[gt_col_name]
        pred_series = joined_df[pred_col_name]

        col_type = infer_column_type(gt_series, pred_series, self.categorical_threshold)

        if col_type == ColumnType.NUMERIC:
            numeric_comparison = compare_numeric(
                gt_series, pred_series, self.numerical_data_match_threshold
            )
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
                data_match=numeric_comparison.data_match,
            )
        else:
            categorical_comparison = compare_categorical(
                gt_series,
                pred_series,
                self.categorical_match_threshold,
                self.categorical_data_match_threshold,
            )
            logger.info(
                f"Categorical comparison '{gt_column}': "
                f"exact_match={categorical_comparison.exact_match_rate:.1%}"
            )
            return ColumnComparison(
                gt_column=gt_column,
                pred_column=pred_column,
                column_type=col_type,
                categorical_comparison=categorical_comparison,
                data_match=categorical_comparison.data_match,
            )

    def _data_match_columns(
        self,
        joined_df: pd.DataFrame,
        unmatched_gt_columns: list[str],
        unmatched_pred_columns: list[str],
    ) -> list[ColumnMatch]:
        """Match unmatched columns purely by data similarity."""
        if not unmatched_gt_columns or not unmatched_pred_columns:
            return []

        logger.info(
            f"Attempting data match for {len(unmatched_gt_columns)} GT columns "
            f"against {len(unmatched_pred_columns)} pred columns"
        )

        similarity_matrix: dict[str, dict[str, tuple[float, MatchMethod | None]]] = {}

        for gt_col in unmatched_gt_columns:
            gt_series = joined_df[gt_col]
            gt_type = infer_column_type(
                gt_series, categorical_threshold=self.categorical_threshold
            )
            similarity_matrix[gt_col] = {}

            for pred_col in unmatched_pred_columns:
                logger.debug(f"Comparing data of GT '{gt_col}' to pred '{pred_col}'")
                pred_series = joined_df[pred_col]
                pred_type = infer_column_type(
                    pred_series, categorical_threshold=self.categorical_threshold
                )

                if gt_type == ColumnType.NUMERIC and pred_type == ColumnType.NUMERIC:
                    score = compute_numeric_similarity(gt_series, pred_series)
                    method = MatchMethod.DATA_NUMERIC
                elif (
                    gt_type == ColumnType.CATEGORICAL
                    and pred_type == ColumnType.CATEGORICAL
                ):
                    score = compute_categorical_similarity(
                        gt_series, pred_series, self.categorical_match_threshold
                    )
                    method = MatchMethod.DATA_CATEGORICAL
                else:
                    score = 0.0
                    method = None

                similarity_matrix[gt_col][pred_col] = (score, method)

        # Greedy matching
        data_matches: list[ColumnMatch] = []
        available_pred_cols = set(unmatched_pred_columns)

        gt_best_scores = [
            (gt_col, max(similarity_matrix[gt_col].values(), key=lambda x: x[0])[0])
            for gt_col in unmatched_gt_columns
            if similarity_matrix[gt_col]
        ]
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

            if best_score >= self.data_match_threshold and best_pred_col:
                data_matches.append(
                    ColumnMatch(gt_col, best_pred_col, best_score, best_method)
                )
                available_pred_cols.remove(best_pred_col)
                logger.info(
                    f"Data matched '{gt_col}' -> '{best_pred_col}' "
                    f"(score={best_score:.3f})"
                )
            else:
                data_matches.append(ColumnMatch(gt_col, None, best_score, None))

        return data_matches

    def compare(
        self,
        gt_df: pd.DataFrame,
        pred_df: pd.DataFrame,
        semantic_weighting: float = 1.0,
        *,
        use_data_matching: bool = True,
    ) -> tuple[DataComparisonResult, pd.DataFrame]:
        """
        Perform full comparison between ground truth and predicted dataframes.

        Args:
            gt_df: Ground truth dataframe.
            pred_df: Predicted/generated dataframe.
            semantic_weighting: Weight for semantic similarity in column matching.
            use_data_matching: Whether to attempt data-based matching for unmatched
            columns.

        Returns:
            DataComparisonResult with full comparison details.

        """
        logger.info("Starting data comparison")

        if gt_df.index.name is None:
            error_msg = "Ground truth dataframe must have an index as primary key"
            raise ValueError(error_msg)
        if pred_df.index.name is None:
            error_msg = "Predicted dataframe must have an index as primary key"
            raise ValueError(error_msg)

        primary_key = f"{gt_df.index.name} | {pred_df.index.name}"

        completeness, joined_df = self._check_join_completeness(gt_df, pred_df)
        column_matches = self.column_matcher.match_columns(
            gt_df, pred_df, semantic_weighting
        )

        matched_gt_cols = {m.gt_column for m in column_matches if m.pred_column}
        matched_pred_cols = {m.pred_column for m in column_matches if m.pred_column}

        unmatched_gt = [c for c in gt_df.columns if c not in matched_gt_cols]
        unmatched_pred = [c for c in pred_df.columns if c not in matched_pred_cols]

        if use_data_matching and unmatched_gt and unmatched_pred:
            logger.info(
                f"Attempting data-based matching for "
                f"the following unmatched GT columns: {unmatched_gt}"
            )

            data_matches = self._data_match_columns(
                joined_df, unmatched_gt, unmatched_pred
            )

            data_matched_gt = {m.gt_column for m in data_matches if m.pred_column}
            column_matches = [
                m for m in column_matches if m.gt_column not in data_matched_gt
            ]

            # only extend with successful matches
            column_matches.extend([m for m in data_matches if m.pred_column])
            matched_pred_cols.update(
                m.pred_column for m in data_matches if m.pred_column
            )

        column_comparisons = [
            self._compare_column_pair(joined_df, m.gt_column, m.pred_column)
            for m in column_matches
            if m.pred_column
        ]

        # build output dataframe with matched and compared columns
        output_df = pd.DataFrame(index=joined_df.index)

        for match in column_matches:
            if match.pred_column:
                # Matched columns - place side by side
                gt_col_name = (
                    f"{match.gt_column}_gt"
                    if f"{match.gt_column}_gt" in joined_df.columns
                    else match.gt_column
                )
                pred_col_name = (
                    f"{match.pred_column}_pred"
                    if f"{match.pred_column}_pred" in joined_df.columns
                    else match.pred_column
                )

                output_df[f"{match.gt_column}_gt"] = joined_df[gt_col_name]
                output_df[f"{match.gt_column}_pred"] = joined_df[pred_col_name]
            else:
                # Unmatched GT columns - append with _unmatched suffix
                gt_col_name = (
                    f"{match.gt_column}_gt"
                    if f"{match.gt_column}_gt" in joined_df.columns
                    else match.gt_column
                )
                output_df[f"{match.gt_column}_unmatched"] = joined_df[gt_col_name]

        # Remap categorical _pred columns using the co-occurrence mapping
        for comp in column_comparisons:
            if (
                comp.categorical_comparison
                and comp.categorical_comparison.category_mapping
            ):
                pred_col = f"{comp.gt_column}_pred"
                if pred_col in output_df.columns:
                    mapping = comp.categorical_comparison.category_mapping
                    output_df[pred_col] = (
                        output_df[pred_col]
                        .fillna("__NA__")
                        .astype(str)
                        .map(lambda x, m=mapping: m.get(x, x))
                        .replace("__NA__", pd.NA)
                    )

        task_completion_percentage = (
            (
                [
                    m.data_match
                    for m in column_comparisons
                    if m.numeric_comparison or m.categorical_comparison
                ].count(True)
                / len(gt_df.columns)
                * 100
            )
            if column_comparisons
            else 0.0
        )

        return DataComparisonResult(
            primary_key=primary_key,
            join_completeness=completeness,
            task_completion_percentage=task_completion_percentage,
            column_matches=column_matches,
            column_comparisons=column_comparisons,
            unmatched_gt_columns=[
                m.gt_column for m in column_matches if not m.pred_column
            ],
            unmatched_pred_columns=[
                c for c in pred_df.columns if c not in matched_pred_cols
            ],
        ), output_df
