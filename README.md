# Silicon-Sample Validity - Analysis Code

This repository contains the analysis code and model-call runner for the study on human-LLM behavioral correspondence in the dictator game.

## Structure
- `R/`: archived R analysis scripts for the meta-regression, robustness checks, CoT analyses, and Bayesian sensitivity analyses.
- `model_runner/`: the Python runner used to call the model APIs and save raw outputs.
- `prompts/`: the prompt documents used in the study.
- `metadata/model_call_metadata.csv`: model identifiers, snapshots, and generation settings.
- `environment/`: software and package notes.

## Data
The analysis data are deposited separately in the OSF data package: https://osf.io/zg9ys/. Update `DG_COMBINED_META` and `DG_OUTPUT_DIR` in `R/00_config.R`, or set the corresponding environment variables, before running the scripts.

## Model-call notes
- GPT-5.5 used the OpenAI API snapshot `gpt-5_5-2026-04-23`.
- DeepSeek-R1 and DeepSeek-V3 were called through SiliconCloud.
- The runner configuration was manually set to `temperature = 1.0`; `top_p = 1.0`; maximum completion length `= 2048`.
- Per-response raw outputs are stored in the result XLSX files. The runner did not save full API response JSON or a separate text log by default.

## Citation
Update `CITATION.cff` with the repository URL and final DOI after the first release.
