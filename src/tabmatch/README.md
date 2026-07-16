# tabmatch

`tabmatch` compares an LLM-generated tabular dataset with human-curated ground truth. It matches columns by normalised name and semantic similarity, falls back to data similarity for unmatched columns, maps compatible categorical labels, and reports row coverage and per-column accuracy.

The comparator works with any two pandas DataFrames. Both DataFrames must have a named index containing the primary key; the index names may differ, but their values are used to join the datasets. Duplicate keys are reported.

Technical supplement forthcoming. See [demo.ipynb](demo.ipynb) for worked programmatic examples.

## Programmatic use

```python
import pandas as pd

from src.tabmatch import DataComparator, print_comparison_report

ground_truth = pd.read_csv("ground_truth.csv", index_col="NSID")
prediction = pd.read_csv("prediction.csv", index_col="NSID")

result, matched_columns = DataComparator().compare(ground_truth, prediction)
print_comparison_report(result)
```

`matched_columns` places matched ground-truth and prediction columns side by side. By default, direct `DataComparator` use is strict: categorical values must match exactly and numeric error must be zero for a column to be marked correct. Pass thresholds explicitly to relax that behavior.

## Experiment CLI

The repository CLI compares task outputs within an experiment context. From the repository root:

```sh
# Compare tmp/smolagent_context_gpt_oss_20b_1.
uv run python scripts/run_comparison.py _gpt_oss_20b_1

# Compare all smolagent_context_* directories under tmp/.
uv run python scripts/run_comparison.py --all
```

The CLI reads `data/output/output.csv` as ground truth and `data/output/cleaned_data.csv` as the prediction, using the first CSV column as the index. For every completed task it writes:

- `comparison_output.csv` — matched values side by side.
- `column_mapping.csv` — ground-truth-to-prediction column mappings.
- `category_mapping.csv` — predicted-to-ground-truth category mappings, when categorical mappings are available.

It also writes `comparison_summary.csv` in the experiment-context root.

`uv run python -m src.tabmatch` is an alias for the same CLI.

## CLI options

| Flag | Default | Description |
|------|---------|-------------|
| `--all` | — | Compare every `smolagent_context_*` directory under `--base-dir` |
| `--base-dir` | `tmp` | Root directory containing experiment contexts |
| `--gt-filename` | `output.csv` | Ground-truth filename within each task output directory |
| `--pred-filename` | `cleaned_data.csv` | Prediction filename within each task output directory |
| `--categorical-threshold` | `20` | Maximum distinct values for categorical treatment |
| `--match-threshold` | `0.9` | Minimum name- or semantic-similarity score for a column match |
| `--column-data-match-threshold` | `0.7` | Minimum data-similarity score for the fallback column match |
| `--categorical-match-threshold` | `0.8` | Minimum conditional probability for a category mapping |
| `--categorical-data-match-threshold` | `0.95` | Minimum categorical exact-match rate reported as correct by the CLI |
| `--numerical-data-match-threshold` | `0.0001` | Maximum numeric NRMSE reported as correct by the CLI |
| `-v`, `--verbose` | off | Enable debug logging |

## Module structure

| File | Purpose |
|------|---------|
| `data_comparator.py` | `DataComparator` orchestration and join checks |
| `column_matcher.py` | Name-, semantic-, and data-based column matching |
| `comparisons.py` | Column-type inference and value comparisons |
| `models.py` | Pydantic result models |
| `report.py` | Console reports and aggregate summaries |
| `__main__.py` | CLI alias delegating to `scripts/run_comparison.py` |
