# Development and validation of EWS systems 🚑

## Contents

-   `preprocessing`

    Contains info on initial pre-processing of Electronic Health Records consisting of early warning score measurements and vital signs for individuals residing in Denmark, with a general admission to the hospitals in the region of Zealand, Denmark, between 2018-2023.

    -   `Pre_Processing_Latest.R` contains R code with the initial pre-processing of the EWS data

-   `Merging`

    -   `Supplemental_Data_Latest.R` contains R code with merging of other clinical data, consisting of procedures, diagnoses, blood tests, and ITA information.

-   `modelling`

    -   `EWS_Modelling.R` :
        -   Contains R code comparing various models and algorithms for early warning systems
        -   Implementation of the weighting model (CBPS) for the individuals
        -   🔗 NEWS: (<https://www.england.nhs.uk/ourwork/clinical-policy/sepsis/nationalearlywarningscore/>)
        -   🔗 NEWS-Light: NEWS2 - Blood Pressure - Temperature
        -   🔗 DEWS: NEWS2-Light + Age + Sex (<https://journals.lww.com/ccmjournal/fulltext/2023/07000/development_and_external_validation_of_the.4.aspx>)
        -   🔗 XGBoost: Age + Sex + Vital Signs + Number of Previous Hospitalizations + Embeddings of Previous Medical Procedures and Diagnoses + historical averages of blood test values + time-related recording information
        -   Grouped Cross-Validation based on hospitals
        -   AUC, Brier Score, Calibration, Net Benefit (Differences)
    -   `M2V_Embeddings.ipynb` :
        -   Contains python code for generating the full embeddings + PCA for dimensionality reduction

-   **Accomplished stuff:**

    -   Assessment of NEWS current system based on predictive performance metrics using data-splitting techniques ✅.

    -   De-biasing the dataset with IPW (Inverse Probability Weighting) based on intervention scenarios ✅

    -   Development of alternative early warning score systems and model comparison ✅

    -   Outcome: 24-hour mortality prediction after initial NEWS score ✅

    -   Used scores: Initial score at admission ✅

    -   Assess calibration and net benefit on various strata of target population ✅
