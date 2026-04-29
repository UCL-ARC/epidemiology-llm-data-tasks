# tabmatch

`tabmatch` compares LLM-generated tabular datasets against human-curated ground truth. It is robust to the naming and label differences that LLM agents routinely introduce — mismatched column names, alternative category labels, and partial outputs — without requiring manual intervention.

It can also be used as a **standalone tool** to compare any two pandas DataFrames or CSV files, independent of the LLM experiment pipeline. See [demo.ipynb](demo.ipynb) for worked examples covering common real-world scenarios.

For a full description of the algorithm, see the [technical supplement](https://example.com/supplement).

## Usage

Compare a single model's experiment output:

```sh
python -m src.tabmatch <model>
```

where `<model>` is the suffix of the context directory (e.g. `qwen3.5:9b_3` targets `tmp/smolagent_context_qwen3.5:9b_3`).

Compare all experiment outputs:

```sh
python -m src.tabmatch --all
```

## Options

| Flag | Default | Description |
|------|---------|-------------|
| `--all` | — | Run comparison for all `smolagent_context_*` directories |
| `--base-dir` | `tmp` | Root directory containing experiment outputs |
| `--gt-filename` | `output.csv` | Ground truth filename within each task directory |
| `--pred-filename` | `cleaned_data.csv` | Predicted output filename within each task directory |
| `--categorical-threshold` | `20` | Max unique values for a column to be treated as categorical |
| `--match-threshold` | `0.8` | Minimum score for a name-based column match |
| `--column-data-match-threshold` | `0.7` | Minimum data-similarity score for the data-based fallback |
| `--categorical-match-threshold` | `0.8` | Minimum conditional probability to map a predicted category label |
| `--categorical-data-match-threshold` | `0.95` | Minimum exact-match rate for a categorical column to count as correct |
| `--numerical-data-match-threshold` | `0.0001` | Maximum NRMSE for a numeric column to count as correct |
| `-v`, `--verbose` | off | Enable debug logging |

## Module structure

| File | Purpose |
|------|---------|
| `data_comparator.py` | `DataComparator` — orchestrates the full pipeline |
| `column_matcher.py` | Name-based and data-based column matching |
| `comparisons.py` | Column type inference and per-column comparison |
| `models.py` | Pydantic result models |
| `report.py` | `print_comparison_report()` and `aggregate_comparison_results()` |
| `__main__.py` | CLI entry point |
