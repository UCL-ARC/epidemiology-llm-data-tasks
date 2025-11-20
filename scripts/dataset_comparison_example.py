"""Some code to illustrate how ComparisonTool can be used."""

from pathlib import Path

from loguru import logger

from src.comparison_tool import ComparisonTool

comp_tool = ComparisonTool(
    ground_truth_data_path=Path("misc/gt_test.csv"),
    pred_data_path=Path("misc/pred_test.csv"),
)

col_diff = (
    comp_tool.compare_column_lengths()
)  # 0 is no diff; we're subtracting pred from gt
row_diff = comp_tool.compare_row_length()
column_names_mapping = comp_tool.match_column_names()  # will produce a dict
column_value_mapping = comp_tool.match_all_column_value_names(
    column_names_mapping=column_names_mapping
)
# the contents will be a list of these in its own pydantic model:
# (
#     ColumnValueMapping(
#         gt_column_name="colname",
#         pred_column_name="colname",
#         value_map=[
#             ValueMapping(
#                 gt_value_name="0",
#                 pred_value_name="0",
#                 match_method="deterministic",
#                 match_value=1.0,
#             ),
#             ValueMapping(
#                 gt_value_name="1",
#                 pred_value_name="1",
#                 match_method="deterministic",
#                 match_value=1.0,
#             ),
#         ],
#     ),
# )
# we can then calculate some sort of metric of
# how good the prediction was from this; or export to json

# establish diffs in value counts
# for one column
column_value_count_diffs = comp_tool._match_column_value_counts(  # noqa: SLF001
    column_value_mapping=column_value_mapping[0]
)

# for the whole df (or, at least, each gt col we have a match for in pred)
df_column_value_count_diffs = comp_tool.match_all_column_value_counts(
    column_value_map=column_value_mapping
)


# let's produce an overview dict (this isnt implemented
# in the class, yet)
comparison_overview = {
    "column_number_diff": col_diff,
    "row_number_diff": row_diff,
    "perc_columns_matched": len(column_names_mapping)
    / len(comp_tool.ground_truth_df.columns)
    * 100,
    "perc_column_vals_matched": None,  # to do,
    "perc_column_val_counts_matched": None,  # to do
}

logger.info(f"comparison overview: {comparison_overview}")
