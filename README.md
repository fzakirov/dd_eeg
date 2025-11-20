# dd_eeg
This repository contains data frames with measures of interest reported in the paper, as well as the code to reproduce the statistical results.

## Directory Structure

### Data
The `data/` directory contains the datasets used in the analysis:
- `data/alpha_poz_raw.csv` raw values of anticipatory alpha PSD at POz
- `data/delta_power.csv` raw values of feedback-related delta power (100-600 ms) at the centro-parietal cluster
- `data/theta_power.csv` raw values of feedback-related theta power (200-400 ms) at the fronto-central cluster

### Code
The `code/` directory contains the source code for the project:
- `code/statistics.R`: R script to reproduce the statistical results.
- `code/plots.R`: R script to reproduce the regression scatter plots.
