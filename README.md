# Development and validation of EWS systems 🚑

## Installation

Install [pixi](https://pixi.sh/latest/installation) to install the dependencies
necessary to run the project.

Once pixi is installed, clone the repository and run the following inside the
project directory:

```bash
# Install all pixi-friendly dependencies
pixi install
# Install missing R dependencies
pixi run post_install
```

To run the python notebooks, it is recommended to install cuda 12.6. After
installation, verify GPU availability in PyTorch:

```python
import torch
print(torch.cuda.is_available())
```

## Contents

All main scripts can be found in the `pipeline` directory:

- `preprocessing.R`: initial pre-processing of Electronic Health Records
  consisting of early warning score measurements and vital signs for individuals
  residing in Denmark, with a general admission to the hospitals in the region
  of Zealand, Denmark, between 2018-2023.
- `preprocessing_extra.R`: merging of other clinical data, consisting of
  procedures, diagnoses, blood tests, and ITA information.
- `EWS_Modelling.R`: comparison of various models and algorithms for early
  warning systems
  - Implementation of the weighting model (CBPS) for the individuals
  - 🔗
    [NEWS](https://www.england.nhs.uk/ourwork/clinical-policy/sepsis/nationalearlywarningscore)
    (National Early Warning Score)
  - 🔗 NEWS-Light: NEWS2 - Blood Pressure - Temperature
  - 🔗 [DEWS](http://doi.org/10.1097/CCM.0000000000005842) (Demographic Early
    Warning Score): NEWS2-Light + Age + Sex
  - 🔗 XGB-EWS: Age + Sex + Vital Signs + Number of Previous Hospitalizations +
    Embeddings of Previous Medical Procedures and Diagnoses + historical
    averages of blood test values + time-related recording information
  - Grouped Cross-Validation based on hospitals
  - AUC, Brier Score, Calibration, Net Benefit (Differences)
- `M2V_Embeddings.ipynb`: generating the full embeddings + PCA for
  dimensionality reduction
- `Sensitivity_Embeddings.ipynb`: generating the full embeddings + PCA for
  dimensionality reduction using a bigger sentence transformer of 560M
  parameters with instruction (`multilingual-e5-large-instruct`)

### EWS models

- [XGBoost](https://xgboost.readthedocs.io/en/stable)
- [Logistic regression](https://parsnip.tidymodels.org/reference/logistic_reg.html)

### Embedding models

- Static embeddings of medical procedures/diagnoses trajectories using
  [model2vec](https://github.com/MinishLab/model2vec)'s
  [potion-multilingual-128M](https://huggingface.co/minishlab/potion-multilingual-128M)
  model
- Logistic regression for Covariate Balancing Propensity Score (CBPS) using the
  [weightit](https://ngreifer.github.io/WeightIt) R package
- Sensitivity analysis using the
  [e5-large-instruct](multilingual-e5-large-instruct) sentence Transformer

## Summary

- Assessment of NEWS current system based on predictive performance metrics
  using data-splitting techniques ✅.

- De-biasing the dataset with IPW (Inverse Probability Weighting) based on
  intervention scenarios ✅

- Development of alternative early warning score systems and model comparison ✅

- Outcome: 24-hour mortality prediction after initial NEWS score ✅

- Used scores: Initial score at admission ✅

- Assess calibration and net benefit on various strata of target population ✅
