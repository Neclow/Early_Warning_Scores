# Models

* XGBoost (https://xgboost.readthedocs.io/en/stable/) for downstream prediction
* Logistic regression (https://parsnip.tidymodels.org/reference/logistic_reg.html) for downstream prediction
* Static embeddings of medical procedures/diagnoses trajectories using model2vec's (https://github.com/MinishLab/model2vec) most recent multilingual model (https://huggingface.co/minishlab/potion-multilingual-128M)
* Logistic regression for Covariate Balancing Propensity Score (CBPS) using the weightit R package (https://ngreifer.github.io/WeightIt/)
* Sensitivity analysis using a bigger 560M param sentence transformer with instruction: https://huggingface.co/intfloat/multilingual-e5-large-instruct
