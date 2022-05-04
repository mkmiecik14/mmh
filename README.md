# mmh

Code for data processing and analysis of the Multimodal Hypersensitivity project.

More details of this project and data can be found on [Open Science Framework](https://osf.io/27ky9/?view_only=90108c59ca8a4431800492a5686b7ef2) (DOI: 10.17605/OSF.IO/27KY9).

Packages, functions, and variables that were used across several scripts are located in `r-prep.R`

Data were processed and analyzed in the following stages:

# Stage 1

Raw data were first preprocessed using the following scripts:

* `sscodes-prepro.R` - prepared master list of participant id numbers
* `avisit-1-prepro.R` - prepared assessment visit 1 data exported from REDCap
* `ppt-prepro.R` - prepared data from quantitative sensory testing (PPT, CPM, TS)
* `auditory-eprime-prepro.R` - prepared the auditory stimulation task data (behavioral)
* `visual-eprime-prepro.R` - prepared the visual stimulation task data (behavioral)
* `screenvisit-prepo.R` - prepared data from screen visit

# Stage 2

After preprocessing, data were examined and explored for outliers and trends in the following scripts:

* `avisit-1-explore.R` - explored REDCap data from assessment 1
* `ppt-explore.R` - explored QST data
* `vis-aud-explore.R`- explored the visual and auditory tasks 
* `extradata-explore.R` - explored questionnaire data not included in PCA

# Stage 3

Data were prepared for PCA analysis by ensuring equivalent size of the dataframes and converting to wide format in the `pca-data-prep.R` script. Extra data used for coloring was visualized, prepared, and saved out in `extradata-explore.R`.

To "refresh" the data for PCA analysis, run the `data-refresh.R` script to compute stage 1 and stage 3 in one go. (This will not refresh "extra data").

# Stage 4

Principal component analysis and inferential testing was performed using the `pca-proc.R` script.

# Stage 5

Annual data for the ICSI (our CPP outcome measure) were pulled from Redcap using the API (`api-calls.R`) and preprocessed using `api-prepro.R`. As more participants complete their annual questionnaires, these data can be refreshed by running `api-calls.R`.

Later longitudinal analyses were completed using `api-icsiplus-prepro.R` and `api-icsiplus-explore.R` where pelvic pain outcome was more comprehensively modeled.

Sensory analyses were conducted in `sensory-analysis.R` and `sensory-analysis-explore.R`. Sensitivity analyses were conducted in `sensitivity-analyses-proc.R`. 

# MISC

Annual questionnaires are processed in `annuals.R` for a grant prep submission.
