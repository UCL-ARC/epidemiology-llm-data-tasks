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

Here: something about the core components of this project, and how to run it. A diagram?

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

This project includes a small agent framework in `src/agents.py` that wraps existing agentic frameworks

### SmolAgent

`SmolAgent` is a thin wrapper around `ToolCallingAgent` that:

- Runs with one or more Python **tools** (see `src/tools.py`, e.g. `produce_and_execute_r`).
- Uses a **temporary working directory** per run:
  - If you pass `context_path=Path("ground_truth/sample1")`, the agent copies that directory into `./tmp/smolagent_context/<sample1>/` and runs there.
  - If `context_path` is `None`, it just runs in the current working directory.
- Returns a structured `AgentResult` (Pydantic model) with:
  - `result`: final LLM output
  - `state`: run status
  - `time_taken`: duration in seconds
  - `steps`: number of agent steps
  - `token_usage`: total tokens used

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
