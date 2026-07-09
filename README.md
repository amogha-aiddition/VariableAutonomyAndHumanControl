# VariableAutonomyAndHumanControl

This repository contains the data, R code, and supporting materials for the manuscript *Variable Autonomy and Human Control*. The project examines how variable autonomy is associated with employee reactions to AI-enabled systems, including willingness to use the system, objection to the system, positive affect, negative affect, and related appraisal processes.
Authored by Amogha Ammava.

## Repository contents

- `CleanedV3.csv`: cleaned dataset used for the analyses.
- R scripts: code used to clean the data, construct variables, and reproduce all analyses.
- Output files: model output and supporting analysis results.

## Data

The dataset includes repeated observations nested within four levels of autonomy. The main constructs used in the analyses are:
- System support / social influence.
- Human meaningfulness.
- Perceived ease of use.
- Performance expectancy.
- Positive affect.
- Negative affect.
- Willingness to use AI.
- Objection to AI use.

## Analyses included

The code reproduces the following analyses:

### 1. Data preparation
- Import of the cleaned dataset.
- Conversion of `LevelOfAutonomy` to a factor.
- Construction of scale scores using row means.
- Rescaling of selected items.
- Collapsing empty response categories for ordinal indicators.

### 2. Measurement models
- One-factor confirmatory factor analyses (CFAs) for each construct.
- Full measurement model CFA across all constructs.
- Configural, metric, and scalar measurement invariance tests across levels of autonomy.
- Reliability, AVE, latent correlations, and R-square output for measurement assessment.

### 3. Multilevel structure checks
- Intraclass correlation coefficient 1 (ICC1) calculations for:
  - PA.
  - Negative affect.
  - Willingness.
  - Objection.
  - PE.
  - EE.
  - SI.
  - HM.

### 4. Random-intercept models
- Null models for:
  - Willingness.
  - Objection.
  - PA.
  - Negative affect.
- Random-intercept models with fixed slopes predicting:
  - Negative affect from SI, HM, PE, and EE.
  - Willingness from PA and negative affect.
  - Objection from PA and negative affect.
  - Objection from PE and EE.
  - Models including autonomy level as a Level-2 predictor.

### 5. Random-slope models
- Random-slope models for:
  - Negative affect.
  - Willingness.
  - Objection.
- Tests of whether slopes vary across autonomy levels.

### 6. Moderation and mediation
- Within-level moderation models.
- Cross-level moderation models.
- 2-1-1 mediation models.
- Full moderated-mediation models.
- Combined models with autonomy-level interactions.
- Group-mean centering and grand-mean centering comparisons.
- Model comparison using AIC and BIC.

## Software

Analyses were conducted in R using packages including:
- `lavaan`
- `semTools`
- `lme4`
- `lmerTest`
- `performance`
- `dplyr`
- `readr`
- `emmeans`

## Reproducibility

To reproduce the analyses:
1. Open the R project or run the scripts from the repository root.
2. Install the required packages.
3. Run the data preparation script.
4. Run the measurement model code.
5. Run the multilevel models and robustness checks.

## Notes on data use

If the repository includes only processed or anonymized data, the files are intended for research transparency and reproducibility. Any restricted raw data should be stored separately and not uploaded unless approved for public sharing.

## Citation

If you use this repository, please cite the associated manuscript and this repository.
