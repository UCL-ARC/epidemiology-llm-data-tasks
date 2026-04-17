## Ground Truth Creation
Running `initialise.sh` will build each ground truth set. We have avoided storing and sharing this data and opted for on demand recreation from the raw data. It is required to ensure that `../raw_data` is populated with associated data required for each sample.

To create a new sample copy `sample_template` and rename it using the pattern
`sample<number>_<topic>`, then edit `metadata.json`, `task.yml` and `rtruth.R`
as required.

Sample `task.yml` files should define the common `task_type` and can optionally
add sample-specific prompt content via an `append` block.

Prompt templates are available in:
- `ground_truth/tasks_brief.yml`
- `ground_truth/tasks_detailed.yml`

The default compatibility template remains in `ground_truth/tasks.yml`.

Each sample consists of 

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
A json dictionary containing task specific metadata, at a top level it MUST be indexed by each filename to allow for standardisation of `initialise.sh`.

`rtruth.R`
The target R script to produce.

`task.txt`
The text required for the LLM.
