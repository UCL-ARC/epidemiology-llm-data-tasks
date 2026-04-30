"""Central configuration constants for epidemiology-llm-data-tasks."""

from pathlib import Path

# ---------------------------------------------------------------------------
# Filesystem layout
# ---------------------------------------------------------------------------

#: Root of all temporary agent execution contexts
AGENT_CONTEXTS_ROOT: Path = Path("tmp/agent_contexts")

#: Root of smolagent experiment output directories (single-run, no model suffix)
SMOLAGENT_CONTEXT_ROOT: Path = Path("tmp/smolagent_context")

#: Prefix shared by all smolagent context directory names (used with startswith)
SMOLAGENT_CONTEXT_PREFIX: str = "smolagent_context"

#: Default top-level tmp directory
TMP_DIR: Path = Path("tmp")


# ---------------------------------------------------------------------------
# Task data structure (relative paths within a task directory)
# ---------------------------------------------------------------------------

#: Subdirectory containing raw input data files
DATA_INPUT_DIR: str = "data/input"

#: Subdirectory containing processed output data files
DATA_OUTPUT_DIR: str = "data/output"


# ---------------------------------------------------------------------------
# Filenames
# ---------------------------------------------------------------------------

#: Ground truth output filename
GT_FILENAME: str = "output.csv"

#: Agent-produced prediction filename
PRED_FILENAME: str = "cleaned_data.csv"

#: R script that produces ground truth data
R_TRUTH_SCRIPT: str = "rtruth.R"

#: R script that produces predicted data (written and executed by the agent)
R_PRED_SCRIPT: str = "rpred.R"


# ---------------------------------------------------------------------------
# Task IDs
# ---------------------------------------------------------------------------

#: The full set of task IDs used in this study
TASK_IDS: list[int] = list(range(1, 21))


# ---------------------------------------------------------------------------
# tabmatch — column matching & comparison defaults
# ---------------------------------------------------------------------------

#: HuggingFace cross-encoder model used for semantic column-name matching
CROSS_ENCODER_MODEL: str = "cross-encoder/stsb-roberta-base"

#: Maximum distinct values for a column to be treated as categorical
CATEGORICAL_THRESHOLD: int = 20

#: Minimum similarity score for a column-name match to be accepted (ColumnMatcher)
MATCH_THRESHOLD: float = 0.5

#: Minimum data-similarity score for a column pair to be considered a candidate match
COLUMN_DATA_MATCH_THRESHOLD: float = 0.7

#: Minimum exact-match rate for a categorical column to be reported as correct
#: (paper / CLI value; DataComparator class default is 1.0 for strict standalone use)
CATEGORICAL_DATA_MATCH_THRESHOLD: float = 0.95

#: Maximum NRMSE for a numeric column to be reported as correct
#: (paper / CLI value; DataComparator class default is 0.0 for strict standalone use)
NUMERICAL_DATA_MATCH_THRESHOLD: float = 0.0001

#: Minimum conditional probability to accept a predicted → ground-truth category mapping
CATEGORICAL_MATCH_THRESHOLD: float = 0.8

#: Column name suffixes applied when merging ground truth and predicted DataFrames
GT_SUFFIX: str = "_gt"
PRED_SUFFIX: str = "_pred"
JOIN_SUFFIXES: tuple[str, str] = (GT_SUFFIX, PRED_SUFFIX)
