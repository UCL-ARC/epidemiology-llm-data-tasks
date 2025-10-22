## Ground Truth Creation
Running initialise.sh will build each ground truth test set. We have avoided storing and sharing this data and opted for on demand recreation from the raw data. It is required to ensure that ../raw_data is populated with associated data required for each test case.

To create a new test case copy `test_template` and edit `metadata.json`, `task.yml` and `rtruth.R` as required.

Each test case consists of 

data/
├─ input/
├─ output/
metadata.json
rtruth.R
task.yml

input/
a comprehensive list of all input files, each input file contains only a sample and is anonymised

output/
the output produced by rtuth.R

metadata.json
a json dictionary containing task specific metadata, at a top level it MUST indexed by each filename to allow for standardisation of initialise.sh

rtruth.R
the target R script to produce

task.txt
the text required for the LLM

initialise.sh
a shell script to initialise the test case, this should pull in the correct data to inpuuts and then run the rscript to produce the outputs
