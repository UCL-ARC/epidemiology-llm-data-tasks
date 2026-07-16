# Ground-truth tasks

This directory contains the human-curated reference tasks used by the benchmark. Raw cohort data are not stored in the repository. Initialise a task set locally to copy its required raw `.tab` files and generate its reference output.

## Initialise tasks

From the repository root:

```sh
uv run python -m scripts.initialise_ground_truth \
  --input_dir <path-to-raw-tab-files>
```

Use `--output_dir` to create an initialised copy elsewhere instead of writing raw inputs and outputs into this source directory:

```sh
uv run python -m scripts.initialise_ground_truth \
  --input_dir <path-to-raw-tab-files> \
  --ground_truth_dir ground_truth \
  --output_dir <initialised-task-directory>
```

| Flag | Description | Default |
|------|-------------|---------|
| `-i`, `--input_dir` | Directory containing the raw `.tab` files | required |
| `-g`, `--ground_truth_dir` | Source directory containing `tasks.yml` and task definitions | `ground_truth` |
| `-o`, `--output_dir` | Destination for copied tasks, raw inputs, and reference outputs | same as `--ground_truth_dir` |
| `-v`, `--verbose` | Print R-script standard output | off |

The initializer copies `tasks.yml`, then processes every `task<n>` directory. For each task it reads the top-level keys in `metadata.json`, copies files with those names from `--input_dir` into `data/input/`, and runs `rtruth.R` with the task directory as the working directory.

## Task layout

```text
task<n>/
├── data/
│   ├── input/                 # created or populated by the initializer
│   └── output/                # contains the reference output
├── metadata.json
├── rtruth.R
└── task.yml
```

- `task.yml` defines the task ID, prompt type, name, and requirements given to the agent.
- `metadata.json` is a JSON object keyed by raw filename. Its values contain the file and variable metadata used in prompt construction. Its keys must exactly match filenames in the raw-data directory, because the initializer uses them to select files to copy.
- `rtruth.R` is the human-curated reference implementation. It must write `data/output/output.csv` and include the source ID column.
- `data/input/` contains the copied raw inputs after initialization.
- `data/output/` contains the reference `output.csv`. During an agent experiment, the copied task context also receives `rpred.R` and `cleaned_data.csv`; those are predictions, not ground truth.

## Create a task

1. Copy `task_template` to a new directory named `task<n>`.
2. Replace all placeholders in `task.yml`, `metadata.json`, and `rtruth.R`.
3. Add the task's prompt type to `tasks.yml` if its `task_type` is new.
4. Ensure the metadata keys name every raw file needed by the task.
5. Run the initializer with a separate `--output_dir` first, inspect `data/output/output.csv`, then add or update tests as appropriate.

The template is a documented scaffold, not an executable task. Its R script deliberately stops until it is tailored to a real input file and derivation.
