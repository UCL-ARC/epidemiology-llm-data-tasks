# `tabmatch` — Technical documentation

**How the comparison works.**

A component of the **RRBench** evaluation framework. This document describes the comparison algorithm as implemented in `src/tabmatch`; it is intended for developers reading or extending the code.

## Contents

- [1. What `tabmatch` does](#1-what-tabmatch-does)
- [2. Pipeline overview](#2-pipeline-overview)
- [3. Row alignment](#3-row-alignment)
- [4. Column matching](#4-column-matching)
- [5. Type inference](#5-type-inference)
- [6. Per-column comparison](#6-per-column-comparison)
- [7. From matches to TP / FP / FN](#7-from-matches-to-tp--fp--fn)
- [8. Metrics](#8-metrics)
- [9. Configuration](#9-configuration)
- [10. Principled extension: taxonomy-robust categorical comparison](#10-principled-extension-taxonomy-robust-categorical-comparison)
- [Appendix: module map](#appendix-module-map)

---

## 1. What `tabmatch` does

`tabmatch` compares two tabular datasets and reduces the comparison to a small set of interpretable metrics. It was written to evaluate the datasets produced by LLM agents in the data-preparation benchmark, where the generated output may be correct in substance while differing from the ground truth in surface detail: column names, category labels, and data encodings are all chosen freely by the model and rarely match the ground truth exactly.

A useful comparison therefore cannot rely on exact name or value equality. The comparator instead does three things automatically:

1. **Recovers the column correspondence** — works out which generated column corresponds to which ground-truth column, even when the names differ, using name similarity first and data similarity as a fallback.
2. **Reconciles category labels** — for categorical columns, infers a mapping between predicted and ground-truth labels from the data before checking agreement, so that a column encoded `1/2` can still match one encoded `Male/Female`.
3. **Decides per-column correctness** — applies a threshold to decide whether each matched column reproduces the ground-truth values closely enough to count as correct, and rolls the per-column outcomes up into task- and experiment-level metrics.

The comparator operates on any two pandas DataFrames. Both must carry a named index holding the primary key (a unique row identifier); the two index names may differ, but their values are used to align rows. The entry point for programmatic use is the `DataComparator` class in `data_comparator.py`; `report.py` turns one or many comparison results into console reports and aggregate summary tables.

---

## 2. Pipeline overview

A single call to `DataComparator.compare(gt_df, pred_df)` runs the following stages in order. Each is described in detail in the sections that follow.

| Stage | What happens | Section |
|---|---|---|
| Row alignment | Inner-join the two frames on the primary-key index; record how many keys are missing, extra, or duplicated. | [§3](#3-row-alignment) |
| Name-based column matching | Score every ground-truth/predicted name pair by the better of Levenshtein and semantic similarity, then assign greedily. | [§4.1](#41-name-based-matching) |
| Data-based fallback matching | For columns left unmatched by name, score pairs by the similarity of their values and assign greedily. | [§4.2](#42-data-based-fallback) |
| Type inference | Classify each matched pair as numeric or categorical. | [§5](#5-type-inference) |
| Per-column comparison | Compare each matched pair with the rule for its type, producing a data-match decision. | [§6](#6-per-column-comparison) |
| Accounting & metrics | Convert per-column outcomes into TP/FP/FN, then into Completeness, Correctness, and Balanced performance. | [§7](#7-from-matches-to-tp--fp--fn)–[8](#8-metrics) |

The design is deliberately asymmetric between the two datasets: the ground truth defines what *should* be present, and the metrics measure how much of it the prediction recovered. Predicted columns with no ground-truth counterpart are reported but not penalised (see [§7](#7-from-matches-to-tp--fp--fn)).

---

## 3. Row alignment

Comparison begins by joining the two frames on their primary-key index (`_check_join_completeness`). The join is an inner join, so only rows whose key appears in both frames are compared value-by-value. Before joining, the comparator records a set of diagnostics: the number of ground-truth keys absent from the prediction (`missing_in_pred`), the number of predicted keys with no ground-truth counterpart (`extra_in_pred`), and the number of duplicate keys on each side.

Duplicate keys raise a warning but do not stop the comparison; they are surfaced in the result so downstream summaries can flag affected tasks. The join-completeness score is the fraction of unique ground-truth keys retained by the join. Because all later stages run on the joined frame, rows are aligned by participant throughout and row order never enters the comparison.

> [!IMPORTANT]
> **Requirement.** Both DataFrames must have a named index (`df.index.name is not None`). `compare()` raises `ValueError` otherwise. In the benchmark the index is the Next Steps identifier; the requirement is enforced because a missing or silently reset index would make row alignment meaningless.

---

## 4. Column matching

Deciding which predicted column corresponds to which ground-truth column is the core of the comparator. Two stages run in sequence: a name-based stage that resolves the large majority of columns, and a data-based fallback for the remainder. Both stages use the same greedy assignment strategy.

### 4.1 Name-based matching

For every pair of ground-truth and predicted column names, two similarity scores are computed (`ColumnMatcher._compute_similarity_matrix`). Names are first normalised by lower-casing, trimming, and replacing underscores and hyphens with spaces.

**Levenshtein similarity** is the edit distance normalised by the longer string:

$$\text{sim}_{\text{lev}}(a, b) = 1 - \frac{\text{lev}(a, b)}{\max(|a|, |b|)}$$

This catches abbreviations and small edits (e.g. `education` vs `educ`).

**Semantic similarity** is produced by a cross-encoder model (`cross-encoder/stsb-roberta-base` by default), clipped to $[0, 1]$. This catches synonyms that are not textually close (e.g. `sex` vs `gender`). Scores for all pairs are computed in batched forward passes.

The two scores are combined by taking the larger of the two (the semantic score may first be down-weighted by an optional `semantic_weighting` factor $w$, which defaults to `1.0`):

$$\text{sim}_{\text{name}}(a, b) = \max({\text{sim}_{\text{lev}}(a, b),\\ w \cdot \text{sim}_{\text{sem}}(a, b) \\})$$

Taking the maximum is intentionally permissive: a pair scores highly if *either* the surface form or the meaning is a close match. The method that produced the winning score (`LEVENSHTEIN` or `SEMANTIC`) is retained for reporting.

#### Greedy assignment

Given the score matrix, columns are assigned greedily (`match_columns`):

1. Compute each ground-truth column's best available score, and sort ground-truth columns by that score in descending order.
2. Walk the sorted list. For each ground-truth column, assign the highest-scoring predicted column that is still available.
3. Accept the assignment only if the score meets the name-match threshold (`match_threshold`); once accepted, the predicted column is removed from the available pool.
4. Ground-truth columns whose best available score falls below the threshold are left unmatched and passed to the data-based fallback.

Anchoring the highest-confidence matches first means a strong, unambiguous pair claims its partner before weaker pairs compete for it. This is a heuristic rather than a globally optimal assignment, which is appropriate here because in practice the score matrix is close to diagonal — most ground-truth names have one clearly best predicted partner — and the threshold filters out the ambiguous remainder rather than forcing a match.

### 4.2 Data-based fallback

An LLM sometimes emits a column whose name resembles nothing in the ground truth — for instance an invented abbreviation, or a name describing a derivation step rather than the underlying concept. To recover these, a second assignment runs over the columns left unmatched after the name stage, this time scoring pairs by their *values* rather than their names (`_data_match_columns`).

Each unmatched pair is scored as follows:

- Both columns are classified as numeric or categorical ([§5](#5-type-inference)). If the types disagree, the score is `0`.
- If both are numeric, the score is the numeric similarity `max(0, 1 − NRMSE)` ([§6.1](#61-numeric-columns)).
- If both are categorical, the score is the exact-match rate after label remapping ([§6.2](#62-categorical-columns)).

The resulting matrix is assigned greedily, exactly as in [§4.1](#41-name-based-matching), with acceptance governed by `column_data_match_threshold`. Successful data matches are merged into the set of matched columns; the matching method is recorded as `DATA_NUMERIC` or `DATA_CATEGORICAL` so the provenance of each match is visible in the report.

Predicted columns still unmatched after both stages are collected as `unmatched_pred_columns`. They are reported but do not affect the headline metrics — see [§7](#7-from-matches-to-tp--fp--fn) for why.

---

## 5. Type inference

Each matched pair is classified as numeric or categorical to select the comparison rule (`infer_column_type`). Dispatching on the pandas dtype alone is unreliable, because survey data routinely stores categorical variables as integer codes (sex as `1/2`, for example). The rule is:

- If either column has a non-numeric dtype → `CATEGORICAL`.
- Else if either column has at most `categorical_threshold` (default `20`) distinct values → `CATEGORICAL`.
- Otherwise → `NUMERIC`.

> [!NOTE]
> **Operational definition.** This makes "categorical" and "numeric" operational categories, not the standard statistical discrete/continuous distinction. A numeric scale with few distinct values (e.g. a 5-point Likert item) is treated as categorical, and "numeric" in this codebase means a numeric column with more than `categorical_threshold` distinct values (e.g. BMI, income, age). The threshold of `20` was chosen from inspection of the benchmark variables.

---

## 6. Per-column comparison

### 6.1 Numeric columns

Numeric pairs are compared on the rows where both values are present (`compare_numeric`). Let $Y$ and $\hat{Y}$ be the aligned non-missing ground-truth and predicted values. The comparator computes RMSE and MAE, then normalises each by the range of the ground-truth column:

$$\text{NRMSE} = \frac{\text{RMSE}}{\max(Y) - \min(Y)}, \qquad \text{NMAE} = \frac{\text{MAE}}{\max(Y) - \min(Y)}$$

Normalising by range (rather than mean or standard deviation) gives a scale-aware error that is stable for the bounded variables common in the benchmark and well defined even when the mean is near zero. If the ground-truth range is zero (a constant column), NRMSE and NMAE are defined as `0` when the corresponding error is `0` and `+∞` otherwise.

A numeric pair is a **data match** when `NRMSE ≤ numerical_data_match_threshold`. The value used for the benchmark is `1e-4` (the CLI default). This effectively requires exact reproduction, with a small tolerance for floating-point rounding — appropriate because the benchmark's numeric variables are produced by deterministic recoding rules, where any real deviation signals an error rather than acceptable noise. Pearson correlation, per-column means and standard deviations, and NMAE are also reported as diagnostics but do not affect the match decision.

> [!WARNING]
> **Note on the class default.** The `compare_numeric` default for `data_match_threshold` is `0` (strict exact match), and `DataComparator` also defaults `numerical_data_match_threshold` to `0.0`. The benchmark results use the CLI's `--numerical-data-match-threshold 0.0001`. Pass the threshold explicitly to reproduce the reported numbers.

### 6.2 Categorical columns

Categorical comparison is harder because the prediction may label the same underlying categories differently (`Male/Female` vs `M/F`, or `1/2` vs `Yes/No`). A direct cell-by-cell check would penalise this unfairly, so the comparator first recovers a label mapping from the data, then checks agreement (`compare_categorical`). Missing values on both sides are replaced with a sentinel (`__NA__`) so they participate on equal footing with observed categories.

#### Recovering the label mapping

For each ground-truth category $g$, the comparator looks at the predicted values on exactly the rows where the ground-truth value is $g$, and forms the conditional distribution over predicted labels:

$$P(p \mid g) = \frac{\bigl|\\{\text{rows with } gt = g \text{ and } pred = p\\}\bigr|}{\bigl|\\{\text{rows with } gt = g\\}\bigr|}$$

The mapping is then built greedily:

1. Sort ground-truth categories by their single best conditional score (descending), breaking ties by ground-truth category size (larger first, so a populous category claims its label before a small idiosyncratic one).
2. For each ground-truth category in turn, take the highest-scoring predicted label not already claimed, provided the score clears `categorical_match_threshold` (default `0.8`).
3. Predicted labels that clear the threshold for no category, and ground-truth categories with no acceptable candidate, are left unmapped.

Greedy assignment is used here — rather than the same reasoning one might apply to a global optimum — because the mapping should respect the conditional structure directly: a category that overwhelmingly maps to one label should claim that label first, even at some cost to the total assignment score.

#### Scoring agreement

The predicted column is rewritten by substituting each label with its mapped ground-truth label (unmapped labels are left as they are), and agreement reduces to a row-wise equality check. The decision statistic is the exact-match rate:

```
exact_match_rate = mean(gt == mapped_pred)
```

A categorical pair is a **data match** when `exact_match_rate ≥ categorical_data_match_threshold`. The benchmark uses `0.95` (the CLI default), which tolerates up to 5% disagreement to absorb minor differences in how residual or missing categories are partitioned. Three further diagnostics are reported: the Jaccard category-overlap score, the Jensen–Shannon distribution similarity, and the lists of categories present on only one side.

> [!TIP]
> **Crosstab as the natural view.** Because the mapping and the agreement check both derive from the ground-truth × predicted contingency table, a crosstab is the most informative way to inspect a categorical result: a clean match concentrates mass on the diagonal after remapping, and off-diagonal mass shows exactly where and how the encodings diverge. The benchmark's categorical figures are presented this way.

---

## 7. From matches to TP / FP / FN

Each ground-truth column contributes one outcome to the per-task accounting (`report.py`):

- **True positive (TP)** — matched to a predicted column (either stage) and the pair is a data match.
- **False positive (FP)** — matched to a predicted column, but the pair is not a data match — the right variable was identified but the values are wrong.
- **False negative (FN)** — not matched to any predicted column in either stage — the variable was missing, or too dissimilar for any correspondence to be found.

Predicted columns never matched to a ground-truth column are *not* counted as false positives. They are reported separately as extra columns. This asymmetry is deliberate: LLM agents routinely emit intermediate working columns (a recoded input used to build a final variable, say), and counting these as errors would understate practical usefulness, since a researcher can simply ignore them. The consequence — that a model producing the full ground truth plus noise scores the same as one producing the ground truth alone — is a known trade-off; the count of extra columns is reported so it stays visible.

---

## 8. Metrics

From the per-column counts, three summary metrics are computed per task and then aggregated across tasks. In the code these are `correctness`, `completeness`, and `output_yield`; the paper refers to the last as **Balanced performance**.

**Completeness** — the share of ground-truth columns the model produced and got matched, regardless of value correctness:

$$\text{Completeness} = \frac{TP + FP}{TP + FP + FN}$$

**Correctness** — among matched columns, the share whose values met the threshold:

$$\text{Correctness} = \frac{TP}{TP + FP}$$

**Balanced performance** (`output_yield`) — the product of the two:

$$\text{Balanced} = \text{Completeness} \times \text{Correctness} = \frac{TP}{TP + FP + FN}$$

The product is used rather than the harmonic mean (F1). It has a direct reading: the proportion of *all* ground-truth variables that were both attempted and correctly reproduced — equivalently, the fraction of the ground-truth output the model successfully delivered. Note that Completeness here is not standard recall (it counts every matched column in the numerator, correct or not), so the F1 of Correctness and Completeness would not carry this interpretation.

When aggregating across tasks, the TP/FP/FN counts are summed first and the metrics computed from the totals (micro-averaging), so tasks are weighted by their number of variables. This is done in both the per-task rows and the `AGGREGATE` row of the summary table.

### 8.1 Task-level metrics

**Task completion percentage** for one task is the share of its ground-truth columns that were true positives (`task_completion_percentage`):

$$\text{TaskCompletion} = \frac{TP_{\text{task}}}{\text{total ground-truth columns in task}} \times 100$$

The denominator is the full ground-truth column count for the task, including unmatched columns, so a model cannot raise its score by omitting hard variables. A task counts as **fully complete** when this reaches 100% (`task_complete`). The number of fully complete tasks and the mean task-completion percentage are both reported: the former measures whether a model can deliver an entire analysis-ready table in one pass, the latter credits partial success.

---

## 9. Configuration

The parameters below govern the comparison. Two sets of defaults exist and it is worth being aware of the distinction: the **class defaults** on `DataComparator` are strict (exact numeric and categorical matching), while the **experiment CLI** (`scripts/run_comparison.py`) applies the more permissive values used for the benchmark results. The "Benchmark" column below is the CLI value.

| Parameter | Meaning | Class default | Benchmark (CLI) |
|---|---|:---:|:---:|
| `categorical_threshold` | Max distinct values for a column to be treated as categorical. | `20` | `20` |
| `match_threshold` | Min name/semantic score to accept a name-based column match. | `0.9`&nbsp;* | `0.9` |
| `column_data_match_threshold` | Min data-similarity score to accept a fallback (data-based) column match. | `0.7` | `0.7` |
| `categorical_match_threshold` | Min conditional probability $P(p \mid g)$ to map a predicted label to a ground-truth label. | `0.8` | `0.8` |
| `categorical_data_match_threshold` | Min exact-match rate (after remapping) for a categorical pair to count as correct. | `1.0` | `0.95` |
| `numerical_data_match_threshold` | Max NRMSE for a numeric pair to count as correct. | `0.0` | `0.0001` |
| `semantic_weighting` | Multiplier applied to the semantic score before taking the max with Levenshtein. | `1.0` | `1.0` |
| `cross_encoder_model` | Cross-encoder used for semantic name similarity. | `stsb-roberta-base` | `stsb-roberta-base` |

\* The class `MATCH_THRESHOLD` is imported from config; the CLI sets it to `0.9`. Confirm the config value if reproducing exactly.

---

## 10. Principled extension: taxonomy-robust categorical comparison

> [!NOTE]
> **Status: planned extension, not current behaviour.** This section documents the reasoning behind a known limitation of the exact-match categorical rule ([§6.2](#62-categorical-columns)); it is not implemented in the current code.

The current categorical rule and its label-remapping step assume an essentially one-to-one correspondence between predicted and ground-truth categories. But a model may legitimately choose a *different granularity* — collapsing an 8-level classification into a sensible 5-level one, for example. Under the detailed prompt this is a departure from the specified categories and exact match is the right standard; under the 'lite' prompt, where categories are not fully specified, such a regrouping can be defensible, and the current rule would mark it wrong. This matters here because taxonomy compression is one of the operation types on which models struggle most, so some of that measured difficulty may be defensible divergence rather than error.

The cleaner way to frame the question is to stop comparing labels and compare *partitions* of the joined rows. Each categorical column groups participants into cells; two columns can then be compared through their contingency table, read as a bipartite graph linking ground-truth categories to predicted ones. The structure of that graph classifies the relationship:

- **Clean merge (coarsening)** — several ground-truth categories each map almost entirely into one predicted category. The model is coarser but consistent; the ground truth is recoverable by grouping.
- **Clean split (refinement)** — one ground-truth category maps into several predicted categories, each drawn almost entirely from it. The model is finer; no information is lost.
- **Crossing (conflation)** — a predicted category mixes rows from ground-truth categories that are themselves split across other predicted categories. This is a genuine error: neither taxonomy is recoverable from the other.

A recoding is valid (any mixture of merges and splits) precisely when every connected component of the table is a "star" — one category on one side linked to several on the other, with no crossing. A continuous version scores the fraction of rows lying on cells that are deterministic in at least one direction (each ground-truth category concentrated on one predicted label, or each predicted label pure in its ground-truth origin), which degrades gracefully as conflation increases.

> [!WARNING]
> **Degeneracy guard.** A test that simply accepts coarsening has an exploit: collapsing every category into one label is trivially a "clean merge" and would pass, despite discarding all information. Any implementation must therefore pair the structural test with an information-retention floor (e.g. a minimum adjusted mutual information, or homogeneity, between the two partitions), which the total-collapse case fails. Splits never trigger this floor because a refinement loses no information; only excessive merging does.

For the two benchmark variables with published hierarchies — NS-SEC and education — the sensible route is not to infer validity from the data at all, but to check the predicted grouping against the official collapse (e.g. the NS-SEC 8→5→3 lookup), turning a judgement into an exact check. Where such a scheme is adopted, the recommended reporting is to keep strict exact-match as the headline number and add the taxonomy-robust outcome as a separate category between TP and FP, so the stricter result stays interpretable and the amount of defensible divergence is quantified rather than hidden.

If implemented, the natural home is a `partition_relationship()` helper in `comparisons.py` returning the relationship class (`exact` / `merge` / `split` / `mixture` / `crossing` / `degenerate`) alongside the continuous score, feeding an additional column in the accounting of [§7](#7-from-matches-to-tp--fp--fn).

---

## Appendix: module map

| File | Responsibility |
|---|---|
| `data_comparator.py` | `DataComparator`: orchestration, primary-key join and completeness checks, both matching stages, and per-column dispatch. |
| `column_matcher.py` | `ColumnMatcher`: normalisation, Levenshtein and cross-encoder scoring, and greedy name-based assignment. |
| `comparisons.py` | Type inference and the numeric / categorical comparison functions, plus the similarity scores used by the data-based fallback. |
| `models.py` | Dataclasses for results: `ColumnMatch`, `NumericComparison`, `CategoricalComparison`, `ColumnComparison`, `JoinCompleteness`, `DataComparisonResult`. |
| `report.py` | Console report for a single result, and the aggregation that builds per-task and experiment-level summary tables (TP/FP/FN, the three metrics, task completion). |
| `__main__.py` | CLI alias delegating to `scripts/run_comparison.py`. |
