# Development and validation of EWS systems 🚑

## Contents

-   `preprocessing`

    Contains info on initial pre-processing of Electronic Health Records consisting of blood tests, diagnoses, various procedures, and intensive care data for individuals residing in Denmark, with a general admission to the hospitals in the region of Zealand, Denmark, between 2018-2023.

    -   `Original_Preprocessing.ipynb` contains python code with an initial pre-processing of all datasets with clinical information.

    -   `Intensive_Care.R` contains R code with more thorough analysis of intensive care data.

    -   `Diagnoses.R` contains R code performing categorization/grouping of various ICD-10 diagnoses of patients.

    -   `Blood_Tests.py` contains python code on imputation of blood tests containing string values not suitable for analysis.

    -   `Procedures.R` contains R code on various medical procedures for each individual. Categorization of procedures (SKS-Codes) has been performed + text mining/topic modelling for the characterization of them.

-   `Merging`

    -   `EWS_Blood.py` contains python code on merging of EWS (Early Warning Score) data of individuals with blood tests.

    -   `EWS_ITA.py` contains python code on merging EWS + Blood Tests with Intensive Care data

    -   `EWS_Blood_ITA_Procedures.R` contains R code on merging EWS + Blood Tests + Intensive Care with Procedures data

    -   `EWS_Final_Merging.py` contains python code on the final merging of the datasets (diagnoses included)

-   `modelling`

    -   `EWS_Systems_Evaluation.R` :
        -   Contains R code comparing various models and algorithms with the current NEWS2 system
        -   🔗 NEWS2-Light: NEWS2 - Blood Pressure - Temperature
        -   🔗 IEWS-Light: NEWS2-Light + Age + Sex
        -   🔗 XGBoost: Age + Sex + Vital Signs + Number of Previous Hospitalizations + Embeddings of Previous Medical Procedures, Action-diagnoses, and Blood tests
        -   🔗 Grouped Cross-Validation based on hospitals
        -   🔗 AUC, Brier Score, Calibration, Net Benefit
        -   🔗 Weighted performance metrics
    -   `Sentence_Transformers_DF_Creation.ipynb` :
        -   Contains python code for generating the full embeddings + PCA for dimensionality reduction

-   **Accomplished stuff:**

    -   Assessment of NEWS2 curren system based on predictive performance metrics using data-splitting techniques ✅.

    -   De-biasing the dataset with IPW based on intervention scenarios ✅

        -   **Counterfactual scenario:** Natural predictive ability of EWS for mortality, unconfounded by interventions (Keeping only individuals without major interventions, weighted by their propensity)

    -   Development of alternative early warning score systems and model comparison ✅

    -   Main Outcome: 30-day mortality prediction after admission ✅

    -   Used scores: Initial EWS score at admission ✅

    -   Add calibration plots for the newly developed models ✅

    -   Assess sustained recovery prediction ✅

    -   Assess performance on various strata of target population / Assess fairness ✅

    -   Create a Table 1 ✅
