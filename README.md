# Development and validation of EWS systems 🚑

## Contents

-   `preprocessing`

    Contains info on initial pre-processing of Electronic Health Records consisting of blood tests, diagnoses, various procedures, and intensive care data for individuals residing in Denmark, with a general admission to the hospitals in the region of Zealand, Denmark, between 2018-2023.

    -   `Original_Preprocessing.ipynb` contains python code with an initial pre-processing of all datasets with clinical information.

    -   `Intensive_Care.R` contains R code with initial analysis of intensive care data.

    -   `Diagnoses.R` contains R code performing categorization/grouping of various ICD-10 diagnoses of patients.

    -   `Blood_Tests.py` contains python code on pre-processing and imputation of blood tests.

    -   `Procedures.R` contains R code on various medical procedures for each individual. Categorization of procedures (SKS-Codes) has been performed (<https://sundhedsdatastyrelsen.dk/indberetning/klassifikationer/sks-klassifikationer/hovedgrupper>)

-   `Merging`

    -   `EWS_Blood.py` contains python code on initial merging of EWS (Early Warning Score) data of individuals with blood tests.

    -   `EWS_ITA.py` contains python code on merging EWS + Blood Tests with Intensive Care data

    -   `EWS_Blood_ITA_Procedures.R` contains R code on merging EWS + Blood Tests + Intensive Care with Procedures data

    -   `EWS_Final_Merging.py` contains python code on the final merging of the datasets (diagnoses included)

-   `modelling`

    -   `Final_Analysis.R` :
        -   Contains R code comparing various models and algorithms for early warning systems
        -   Implementation of the weighting model (CBPS) for the individuals
        -   🔗 NEWS2: (<https://www.england.nhs.uk/ourwork/clinical-policy/sepsis/nationalearlywarningscore/>)
        -   🔗 NEWS2-Light: NEWS2 - Blood Pressure - Temperature
        -   🔗 IEWS-Light: NEWS2-Light + Age + Sex (<https://journals.lww.com/ccmjournal/fulltext/2023/07000/development_and_external_validation_of_the.4.aspx>)
        -   🔗 XGBoost: Age + Sex + Vital Signs + Number of Previous Hospitalizations + Embeddings of Previous Medical Procedures, Action-diagnoses + historical averages of blood test values
        -   Grouped Cross-Validation based on hospitals
        -   AUC, Brier Score, Calibration, Net Benefit (Differences) with weights
    -   `Clean_M2V_Sentences.ipynb` :
        -   Contains python code for generating the full embeddings + PCA for dimensionality reduction

-   **Accomplished stuff:**

    -   Assessment of NEWS2 current system based on predictive performance metrics using data-splitting techniques ✅.

    -   De-biasing the dataset with IPW (Inverse Probability Weighting) based on intervention scenarios ✅

    -   Development of alternative early warning score systems and model comparison ✅

    -   Outcome: 24-hour mortality prediction after initial NEWS2 score ✅

    -   Used scores: Initial NEWS2 score at admission ✅

    -   Assess calibration and net benefit on various strata of target population ✅
