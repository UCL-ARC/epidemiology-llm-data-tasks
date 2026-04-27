# epidemiology-llm-data-tasks

How good are ~open llms at performing routine data cleaning and analysis tasks for epidemiology?

## tl, dr

This repo contains software aimed at evaluation to usefulness, efficacy/accuracy and performance of leveraging ~open (source, weights, etc.) Large Language Models (LLMs) for typical, traditionally cumbersome data tasks in the field of epidemiology, such as

- data cleaning & aligning with codebooks/metadata dictionaries
- exploratory data analysis

In order to test this, we employ an agentic framework, and evaluate its output given different LLM/compute configurations. In descending order, we are interested in the following metrics:

1. Accuracy (how well does the LLM perform the tasks)
2. Performance (how fast/resource-effecient does it perform the task)

## Workflow

The pipeline works as follows:

1. **Ground truth** — each `ground_truth/sampleN/` directory contains a task definition (`task.yml`), dataset metadata (`metadata.json`), and a reference R script (`rtruth.R`) that produces the correct output.
2. **Agent run** — `src/agents.py` reads `experiment.yml` (or CLI args), constructs a prompt from `ground_truth/tasks.yml` and the sample's metadata, then runs the LLM agent in an isolated temp directory under `tmp/`.
3. **Evaluation** — `src/dataset_comparison/` compares the agent's output CSV against the ground truth CSV and reports per-column and aggregate accuracy metrics.

## Installation

**NOTE**: Currently, this software is only supported on Ubuntu systems.

### Installing & configuring `R`

This software writes and executes `R` code. For this purpose, you need R installed and ready to run. For this purpose, you can run `scripts/install_configure_r.sh`, which does everything for you.

```shell
chmod +x scripts/install_configure_r.sh
sudo sh scripts/install_configure_r.sh
```

This will also install `renv`, used to manage R virtual environments. This is where this software will run R code, and install dependencies if required.

### Installing `uv`

[uv](https://docs.astral.sh/uv) is used for Python dependency management and managing virtual environments. You can install uv either using pipx or the uv installer script:

```sh
curl -LsSf https://astral.sh/uv/install.sh | sh
```

### Installing Dependencies

Once uv is installed, install dependencies:

```sh
uv sync
```

### Activate your Python environment

```sh
source .venv/bin/activate
```

### Installing pre-commit hooks

Install `pre-commit` locally (in your activated `venv`) to aid code consistency (if you're looking to contribute).

```sh
pre-commit install
```

### To run src/initialise_ground_truth.py

Run uv run python -m src.initialise_ground_truth -i data/UKDA-5545-tab/tab/safeguarded_eul, making sure you have the corresponding raw data downloaded


## Agents

This project includes a small agent framework in `src/agents.py` that wraps existing agentic frameworks.

### Running an experiment

Experiments are configured via `experiment.yml` at the project root:

```yaml
model:
  provider: ollama              # ollama | huggingface
  model_id: gemma4:31b
  temperature: 0.8

agent:
  type: tool_calling            # tool_calling | code

experiment:
  runs: 1                       # number of times to repeat the full sample loop
  use_overrides: false          # if true, use per-sample override prompts where present
  persist_context: true         # if true, keep the temp working directory after each run
  samples:                      # list of sample numbers to run; leave empty [] to run all
    - 1
```

Run with the default config:

```sh
uv run python3 -m src.agents
```

Point to a different config file (useful for running multiple experiments):

```sh
uv run python3 -m src.agents --config experiments/qwen_run.yml
```

Any config field can be overridden via CLI args without editing the YAML:

```sh
# Override model and run 3 times across samples 1, 2, 3
uv run python3 -m src.agents --model_id qwen3.5:9b --runs 3 --samples 1 2 3

# Use HuggingFace instead of Ollama (reads HF_TOKEN from environment)
uv run python3 -m src.agents --provider huggingface --model_id Qwen/Qwen3-30B-A3B-Instruct-2507

# Use per-sample override prompts and discard temp directories after each run
uv run python3 -m src.agents --use_overrides --no_persist_context
```

CLI args always take precedence over `experiment.yml`. Run `uv run python3 -m src.agents --help` for the full list of options.

### Prompt construction

For each sample, the prompt is built as follows:

- If `use_overrides: true` and the sample's `task.yml` contains an `override` key, that prompt is used verbatim (with `{metadata}` interpolated).
- Otherwise, the base prompt is taken from `ground_truth/tasks.yml` by matching `task_type`. If the sample's `task.yml` has `additional_requirements`, these are injected into the base prompt before the metadata block.

### SmolAgent

`SmolAgent` is a thin wrapper around `ToolCallingAgent` (or `CodeAgent` for `type: code`) that:

- Runs with one or more Python **tools** (see `src/tools.py`, e.g. `produce_and_execute_r`).
- Uses a **temporary working directory** per run:
  - If you pass `context_path=Path("ground_truth/sample1")`, the agent copies that directory into `./tmp/smolagent_context/<sample1_hash>/` and runs there.
  - If `context_path` is `None`, it just runs in the current working directory.
- Returns a structured `AgentResult` (Pydantic model) with:
  - `result`: final LLM output
  - `state`: run status
  - `time_taken`: duration in seconds
  - `steps`: number of agent steps
  - `token_usage`: total tokens used

## Dataset Comparison

The `src/dataset_comparison/` module compares agent-generated output CSVs against ground truth CSVs. It handles mismatched column names, different category labels, and partial outputs.

### Overview

`DataComparator` orchestrates the full comparison pipeline:

1. **Join completeness** — rows are joined on the dataframe index (primary key). Reports how many ground truth rows are present in the predicted output, and flags duplicate keys.
2. **Column matching** — ground truth columns are matched to predicted columns by name.
3. **Data-based fallback matching** — any columns that could not be matched by name are matched by data similarity.
4. **Per-column comparison** — each matched column pair is compared using type-appropriate metrics.
5. **Reporting** — results are printed per-sample and aggregated across experiments.

### Column Matching (`column_matcher.py`)

Columns are matched using a two-stage process:

- **Levenshtein similarity** — normalised edit distance between lowercased, stripped column names.
- **Semantic similarity** — a `cross-encoder/stsb-roberta-base` cross-encoder scores name pairs for meaning similarity.

The best score across both methods is used. The Hungarian algorithm (`scipy.optimize.linear_sum_assignment`) then finds the globally optimal one-to-one assignment across all column pairs. Matches below
`match_threshold` (default `0.5`) are left unmatched and passed to the data-based fallback.

**Data-based fallback** computes actual data similarity between all unmatched column pairs (numeric: normalised RMSE; categorical: weighted exact match + distribution + overlap) and again uses the Hungarian
algorithm to assign the best pairs. Only matches above `data_match_threshold` (default `0.7`) are accepted.

### Column Type Inference (`comparisons.py`)

A column is treated as categorical if either series is non-numeric, or if either series has fewer than `categorical_threshold` (default 20) unique values. Otherwise it is treated as numeric.

### Numeric Comparison

Matched numeric columns are compared using:

| Metric | Description |
|--------|-------------|
| RMSE | Root mean squared error |
| MAE | Mean absolute error |
| NRMSE | RMSE normalised by IQR (falls back to range if IQR is zero) |
| NMAE | MAE normalised by IQR |
| Correlation | Pearson correlation (omitted if either series is constant) |

A column is flagged as a **data match** if `NRMSE <= numerical_data_match_threshold` (default `0.0`, i.e. exact).

### Categorical Comparison

Categorical columns often use different label strings for the same underlying category (e.g. `"Paid work"` vs `"FT paid work"`). The comparison handles this with automatic category mapping:

1. **Co-occurrence matrix** — for each ground truth category, the normalised distribution of predicted values co-occurring with it is computed. This gives a score for how strongly each (gt category, pred
category) pair co-occurs.
2. **Greedy assignment** — categories are sorted by their best co-occurrence score (descending), then by ground truth count (descending). Each ground truth category claims its best available predicted category.
Pairs below `categorical_match_threshold` (default `0.8`) are left unmapped.
3. **Remapping** — predicted values are remapped to ground truth labels before row-wise comparison.

Metrics reported:

| Metric | Description |
|--------|-------------|
| Exact match rate | Proportion of rows where (remapped) pred == gt |
| Category overlap score | Jaccard similarity between gt and pred category sets after remapping |
| Distribution similarity | Jensen-Shannon similarity (1 − JS divergence) between gt and pred distributions |

A column is a **data match** if `exact_match_rate >= categorical_data_match_threshold` (default `1.0`, i.e. every row must match).

### Result Models (`models.py`)

| Model | Description |
|-------|-------------|
| `JoinCompleteness` | Row-level join statistics |
| `ColumnMatch` | Matched column pair with score and method |
| `NumericComparison` | Full numeric metrics for one column pair |
| `CategoricalComparison` | Full categorical metrics for one column pair, including the category mapping used |
| `ColumnComparison` | Wraps one matched pair with its numeric or categorical comparison |
| `DataComparisonResult` | Full result for one sample: join completeness, all column matches and comparisons, unmatched columns, task completion percentage |

### Reporting and Aggregate Metrics (`report.py`)

`print_comparison_report()` prints a per-sample rich-formatted report covering join completeness, column match table, and per-column metric tables.

`aggregate_comparison_results()` aggregates across multiple samples into a summary dataframe and prints an aggregate metrics table. Key aggregate metrics are:

| Metric | Definition |
|--------|------------|
| **Correctness** | TP / (TP + FP) — of matched columns, what fraction are data matches |
| **Completeness** | (TP + FP) / (TP + FP + FN) — fraction of gt columns that were matched at all |
| **Output yield** | Correctness × Completeness — overall fraction of gt columns correctly reproduced |

Where TP = matched column with data match, FP = matched column without data match, FN = unmatched gt column.


## Experiment Data

Experiment outputs live under `tmp/`. The `data/` directories within each sample (containing raw `.tab` inputs and generated `.csv` outputs) are excluded from version control to keep the repository lightweight. All other experiment files — R scripts, task definitions, metadata, and summaries — are tracked.

### Rebuilding experiment data

After cloning, use `src/rebuild_experiments.py` to regenerate the `data/input/` and `data/output/` directories for each experiment sample. This copies raw `.tab` files from your local data directory and re-runs the R scripts (`rtruth.R` and `rpred.R`) to produce `cleaned_data.csv` and `output.csv`.

```sh
# Rebuild all experiments (requires raw data in data/input/)
python -m src.rebuild_experiments

# Rebuild only a specific model's experiments
python -m src.rebuild_experiments --model "qwen3.5:9b"

# Rebuild a specific sample across all models
python -m src.rebuild_experiments --sample 4

# Combine filters and enable verbose R output
python -m src.rebuild_experiments --model "qwen3.5:9b_1" --sample 13 -v

# Use a custom raw data directory
python -m src.rebuild_experiments -i data/UKDA-5545-tab/tab/safeguarded_eul
```

| Flag | Default | Description |
|------|---------|-------------|
| `-i`, `--input_dir` | `data/input` | Directory containing raw `.tab` data files |
| `-t`, `--tmp_dir` | `tmp` | Root directory of experiment outputs |
| `-m`, `--model` | all | Filter by model name substring (e.g. `qwen3.5:9b`, `devstral-small-2:24b_2`) |
| `-s`, `--sample` | all | Filter by sample number (e.g. `4`) |
| `-v`, `--verbose` | off | Print R script stdout |
