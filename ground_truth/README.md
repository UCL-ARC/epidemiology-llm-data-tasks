## Ground Truth Creation

Use `scripts/initialise_ground_truth.py` to build each ground truth set. We have avoided storing and sharing this data and opted for on-demand recreation from the raw data.

```
python scripts/initialise_ground_truth.py \
  -i <path/to/raw_data> \
  [-g <ground_truth_dir>] \
  [-o <output_dir>] \
  [-v]
```

| Flag | Description | Default |
|------|-------------|---------|
| `-i` / `--input_dir` | Directory containing the raw `.tab` data files (required) | — |
| `-g` / `--ground_truth_dir` | Directory containing task definitions (metadata, R scripts) | `ground_truth/` |
| `-o` / `--output_dir` | Directory where generated data will be saved. When omitted, data is written back into `--ground_truth_dir` | same as `-g` |
| `-v` / `--verbose` | Print R script stdout | off |

The script will:
1. Copy `tasks.yml` from `--ground_truth_dir` to `--output_dir`.
2. For each `task<n>` subdirectory: copy the required raw data files into `<output_dir>/task<n>/data/input/` and run `rtruth.R` with its working directory set to `<output_dir>/task<n>/`.

To create a new task copy `task_template` and edit `metadata.json`, `task.yml` and `rtruth.R` as required.

Each task consists of 

```
data/
├─ input/
├─ output/
metadata.json
rtruth.R
task.yml
```

`input/`
A comprehensive list of all input files, each input file contains only a sample and is anonymised.

`output/`
The output produced by rtuth.R

`metadata.json`
A json dictionary containing task specific metadata, at a top level it MUST be indexed by each filename to allow for standardisation of `initialise_ground_truth.py`.

`rtruth.R`
The target R script to produce.

`task.txt`
The text required for the LLM.
