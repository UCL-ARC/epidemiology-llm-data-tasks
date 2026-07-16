# Research Ready Bench

Research Ready Bench evaluates open-weight LLMs on routine longitudinal-cohort data-preparation tasks. Each task asks an agent to write and execute R code that produces an analysis-ready dataset; the generated dataset is then compared with a human-curated ground truth.

Although the included benchmark focuses on longitudinal cohort data preparation, the framework can be extended to any task whose expected output is a tabular dataset with a comparable ground truth.

The repository also contains [tabmatch](src/tabmatch/README.md), a reusable Python comparator for tabular datasets whose column names or categorical labels differ.

## Workflow

1. **Create ground truth.** Each [ground_truth](ground_truth/README.md) task contains metadata, a task specification, and an `rtruth.R` reference script. `scripts/initialise_ground_truth.py` copies the required raw `.tab` files and runs `rtruth.R` to produce `data/output/output.csv`.
2. **Run an agent.** `scripts/run_experiment.py` reads `experiment.yml`, builds a prompt from `ground_truth/tasks.yml` and each task's metadata, then runs `SmolAgent` in an isolated context under `tmp/`. The agent must create `data/output/cleaned_data.csv`.
3. **Evaluate outputs.** `scripts/run_comparison.py` uses `tabmatch` to compare `cleaned_data.csv` with `output.csv` and writes task-level mappings plus an aggregate summary.

## Requirements

- Python 3.13.5 or later, as declared in [pyproject.toml](pyproject.toml).
- [uv](https://docs.astral.sh/uv) for Python dependency management.
- R available as `Rscript` on `PATH`.
- Access to the raw UK Data Service `.tab` files; they are not distributed with this repository.
- One supported model provider:
  - **Ollama:** a running local Ollama service with the selected model available.
  - **Hugging Face:** `HF_TOKEN` in the environment.
  - **vLLM:** an OpenAI-compatible endpoint; set `api_base` in `experiment.yml`. `VLLM_API_KEY` is optional and defaults to `EMPTY`.

The first comparison can download the cross-encoder model used for semantic column-name matching, so it needs network access and may take longer.

### Python environment

Install `uv`, then create the environment and install the project dependencies from the repository root:

```sh
uv sync
```

Run project commands with `uv run python`; activating `.venv` is optional.

### R environment

On Ubuntu, [scripts/install_configure_r.sh](scripts/install_configure_r.sh) installs R and the `renv` package:

```sh
chmod +x scripts/install_configure_r.sh
sudo sh scripts/install_configure_r.sh
```

The script is Ubuntu-specific and does **not** install the R packages used by the reference scripts. On any platform, install R first and then install the required packages from CRAN:

```r
install.packages(c(
  "haven", "dplyr", "tidyr", "purrr", "here", "labelled", "readr", "stringr"
), repos = "https://cloud.r-project.org")
```

> **Windows:** a tested setup guide is forthcoming. No Windows installation script is provided yet.

### Optional contributor tooling

```sh
uv run pre-commit install
```

## Quick start

### 1. Initialise ground-truth data

Point the initializer at the directory containing the raw `.tab` files. This writes inputs and reference outputs into each task directory by default:

```sh
uv run python -m scripts.initialise_ground_truth \
  --input_dir data/UKDA-5545-tab/tab/safeguarded_eul
```

See [ground_truth/README.md](ground_truth/README.md) for output-directory options and task-authoring guidance.

### 2. Configure and run an experiment

[experiment.yml](experiment.yml) is the default configuration. The following is a minimal representative configuration:

```yaml
model:
  provider: ollama             # ollama | huggingface | vllm
  model_id: gpt-oss:20b
  api_base: null               # required for vllm; must end in /v1
  temperature: 0.8

agent:
  type: tool_calling           # tool_calling | code

experiment:
  runs: 1
  use_overrides: false
  use_lite_requirements: true
  ground_truth_dir: ground_truth
  persist_context: true
  tasks: [1]                   # [] runs every task
```

Run the configured tasks:

```sh
uv run python scripts/run_experiment.py
```

Configuration values can be overridden without editing YAML:

```sh
# Run tasks 1–3 three times with a different model.
uv run python scripts/run_experiment.py --model_id qwen3.5:9b --runs 3 --tasks 1 2 3

# Use Hugging Face; the command reads HF_TOKEN from the environment.
uv run python scripts/run_experiment.py --provider huggingface --model_id Qwen/Qwen3-30B-A3B-Instruct-2507

# Use task-specific override prompts and remove temporary contexts after completion.
uv run python scripts/run_experiment.py --use_overrides --no_persist_context
```

CLI arguments take precedence over `experiment.yml`. Run `uv run python scripts/run_experiment.py --help` for all options.

Each run creates `tmp/smolagent_context_<sanitised-model-id>_<run>/`. Each task context contains the copied task definition, generated `rpred.R`, runtime information, and its `data/` directory. Persisted contexts may be large because they include raw inputs.

### Prompt construction

For each task, the runner either uses the task's `override` prompt (when `use_overrides` is enabled) or selects a base prompt from `ground_truth/tasks.yml` using `task_type`. It formats that prompt with the task `metadata.json` and either `additional_requirements` or `additional_requirements_lite`.

## Compare experiment outputs

Compare one model run by supplying the context-directory suffix. Model IDs are sanitised for directory names, so `gpt-oss:20b` becomes `gpt_oss_20b`:

```sh
uv run python scripts/run_comparison.py _gpt_oss_20b_1
```

Or compare every `smolagent_context_*` directory under `tmp/`:

```sh
uv run python scripts/run_comparison.py --all
```

The comparison writes `comparison_output.csv`, `column_mapping.csv`, and, when applicable, `category_mapping.csv` beside each task's outputs. It writes `comparison_summary.csv` at the model-run root. See [src/tabmatch/README.md](src/tabmatch/README.md) for comparison behavior and thresholds.

## Rebuild archived experiment data

Raw task inputs and generated CSVs within `tmp/` are excluded from version control. If an archived experiment includes its R scripts and metadata but not its data directories, rebuild them from a local raw-data directory:

```sh
# Rebuild all archived experiment contexts using data/input by default.
uv run python -m scripts.rebuild_experiments

# Rebuild a model's contexts or one task across contexts.
uv run python -m scripts.rebuild_experiments --model "qwen3.5:9b"
uv run python -m scripts.rebuild_experiments --task 4

# Combine filters and provide the location of raw .tab files.
uv run python -m scripts.rebuild_experiments \
  --model "qwen3.5:9b_1" --task 13 --verbose \
  --input_dir data/UKDA-5545-tab/tab/safeguarded_eul
```

| Flag | Default | Description |
|------|---------|-------------|
| `-i`, `--input_dir` | `data/input` | Directory containing raw `.tab` files |
| `-t`, `--tmp_dir` | `tmp` | Root directory of archived experiment contexts |
| `-m`, `--model` | all | Filter model-run directory names by substring |
| `-s`, `--task` | all | Filter to one task number |
| `-v`, `--verbose` | off | Print R-script standard output |
