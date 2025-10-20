## This is a template for creating a test case

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
a json dictionary containing task specific metadata

rtruth.R
the target R script to produce

task.txt
the text required for the LLM  
