# epidemiology-llm-data-tasks

Evaluation of open-weight LLMs for routine data cleaning and analysis tasks in epidemiology.

## tl;dr

A substantial share of research time in health and social sciences is spent on repetitive data preparation — recoding survey responses, constructing composite indices, handling missingness, and merging datasets. This repo contains a benchmark and agentic framework to evaluate how well large language models (LLMs) can automate such tasks.

To evaluate model outputs, we introduce **tabmatch** (`src/tabmatch/`): a novel approach for comparing LLM-generated tabular datasets against a ground truth that is robust to the naming and label differences LLMs routinely introduce, without requiring manual intervention. See [`src/tabmatch/README.md`](src/tabmatch/README.md) for full details.

We are interested in the following metrics, in descending order of importance:

1. **Accuracy** — how accurately does the model reproduce the target dataset?
2. **Performance** — how fast and resource-efficiently does it do so?

## Workflow

The pipeline works as follows:

1. **Ground truth** — each `ground_truth/taskN/` directory contains a task definition (`task.yml`), dataset metadata (`metadata.json`), and a reference R script (`rtruth.R`) that produces the correct output.
2. **Agent run** — `src/agents.py` reads `experiment.yml` (or CLI args), constructs a prompt from `ground_truth/tasks.yml` and the task's metadata, then runs the LLM agent in an isolated temp directory under `tmp/`.
3. **Evaluation** — `src/tabmatch/` compares the agent's output CSV against the ground truth CSV and reports per-column and aggregate accuracy metrics.

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

### Initialising ground truth data

Requires the corresponding raw data to be downloaded. Run:

```sh
uv run python -m scripts.initialise_ground_truth -i data/UKDA-5545-tab/tab/safeguarded_eul
```


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
  runs: 1                       # number of times to repeat the full task loop
  use_overrides: false          # if true, use per-task override prompts where present
  persist_context: true         # if true, keep the temp working directory after each run
  tasks:                        # list of task numbers to run; leave empty [] to run all
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
# Override model and run 3 times across tasks 1, 2, 3
uv run python3 -m src.agents --model_id qwen3.5:9b --runs 3 --tasks 1 2 3

# Use HuggingFace instead of Ollama (reads HF_TOKEN from environment)
uv run python3 -m src.agents --provider huggingface --model_id Qwen/Qwen3-30B-A3B-Instruct-2507

# Use per-task override prompts and discard temp directories after each run
uv run python3 -m src.agents --use_overrides --no_persist_context
```

CLI args always take precedence over `experiment.yml`. Run `uv run python3 -m src.agents --help` for the full list of options.

### Prompt construction

For each task, the prompt is built as follows:

- If `use_overrides: true` and the task's `task.yml` contains an `override` key, that prompt is used verbatim (with `{metadata}` interpolated).
- Otherwise, the base prompt is taken from `ground_truth/tasks.yml` by matching `task_type`. If the task's `task.yml` has `additional_requirements`, these are injected into the base prompt before the metadata block.

### SmolAgent

`SmolAgent` is a thin wrapper around `ToolCallingAgent` (or `CodeAgent` for `type: code`) that:

- Runs with one or more Python **tools** (see `src/tools.py`, e.g. `produce_and_execute_r`).
- Uses a **temporary working directory** per run:
  - If you pass `context_path=Path("ground_truth/task1")`, the agent copies that directory into `./tmp/smolagent_context/<task1_hash>/` and runs there.
  - If `context_path` is `None`, it just runs in the current working directory.
- Returns a structured `AgentResult` (Pydantic model) with:
  - `result`: final LLM output
  - `state`: run status
  - `time_taken`: duration in seconds
  - `steps`: number of agent steps
  - `token_usage`: total tokens used

## Dataset Comparison

The `src/tabmatch/` module compares agent-generated output CSVs against ground truth CSVs. It is designed to be robust to the naming and encoding differences that LLM agents routinely introduce — mismatched column names, alternative category labels, and partial outputs — while remaining strict about semantic correctness.

For full algorithmic details, see [`src/tabmatch/README.md`](src/tabmatch/README.md).


## Experiment Data

Experiment outputs live under `tmp/`. The `data/` directories within each task (containing raw `.tab` inputs and generated `.csv` outputs) are excluded from version control to keep the repository lightweight. All other experiment files — R scripts, task definitions, metadata, and summaries — are tracked.

### Rebuilding experiment data

After cloning, use `src/rebuild_experiments.py` to regenerate the `data/input/` and `data/output/` directories for each experiment task. This copies raw `.tab` files from your local data directory and re-runs the R scripts (`rtruth.R` and `rpred.R`) to produce `cleaned_data.csv` and `output.csv`.

```sh
# Rebuild all experiments (requires raw data in data/input/)
python -m src.rebuild_experiments

# Rebuild only a specific model's experiments
python -m src.rebuild_experiments --model "qwen3.5:9b"

# Rebuild a specific task across all models
python -m src.rebuild_experiments --task 4

# Combine filters and enable verbose R output
python -m src.rebuild_experiments --model "qwen3.5:9b_1" --task 13 -v

# Use a custom raw data directory
python -m src.rebuild_experiments -i data/UKDA-5545-tab/tab/safeguarded_eul
```

| Flag | Default | Description |
|------|---------|-------------|
| `-i`, `--input_dir` | `data/input` | Directory containing raw `.tab` data files |
| `-t`, `--tmp_dir` | `tmp` | Root directory of experiment outputs |
| `-m`, `--model` | all | Filter by model name substring (e.g. `qwen3.5:9b`, `devstral-small-2:24b_2`) |
| `-s`, `--task` | all | Filter by task number (e.g. `4`) |
| `-v`, `--verbose` | off | Print R script stdout |
