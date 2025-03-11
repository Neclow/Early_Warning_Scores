# Models

* XGBoost (https://xgboost.readthedocs.io/en/stable/) for downstream prediction
* Logistic regression (https://parsnip.tidymodels.org/reference/logistic_reg.html) for downstream prediction
* Static embeddings using distillation of sentence transformers models (https://huggingface.co/hs-hf/jina-embeddings-v3-distilled) using model2vec (https://github.com/MinishLab/model2vec)
* Regularized/Ridge logistic regression (https://glmnet.stanford.edu/articles/glmnet.html) for propensity scores creation using embeddings for adjustment
