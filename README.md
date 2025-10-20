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
