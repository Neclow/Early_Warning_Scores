#########################################
############# Packages ##################
#########################################

library(tidyverse) # For data analysis
library(tidymodels) # For modelling if needed
library(doParallel) # Parallel processing
library(tidylog) # Observing pre-processing numbers
library(arrow) # Read parquet files

#######################################################


#################################################################
################ Predicting sustained recovery ##################
#################################################################

# Add the new dataframe

data <- read_parquet("embeddings_df_weights.parquet")

# Make a small modification to hospitals

data <- data |> 
  mutate(Hospital = if_else(Hospital == "HGH, Hervel and Gentofte Hospital", "HGH, Herlev and Gentofte Hospital", Hospital)) |> 
  mutate_at(vars(Hospital),as.factor)


# Create the new outcome : Sustained recovery

result_df <- data %>%
  group_by(PT_ID) %>%
  # Create the sustained recovery variable
  mutate(
    next_admission_date = lead(HOSP_DISCH_TIME),
    days_to_next_admission = as.numeric(next_admission_date - HOSP_DISCH_TIME),
    days_to_death = as.numeric(deathDate - HOSP_DISCH_TIME),
    sustained_recovery = case_when(
      # If there's no next admission and no death date, it's a sustained recovery
      is.na(next_admission_date) & is.na(deathDate) ~ 0,
      # If next admission is more than 30 days later and death is either NA or more than 30 days later
      (is.na(days_to_next_admission) | days_to_next_admission > 30) & 
        (is.na(days_to_death) | days_to_death > 30) ~ 0,
      # Otherwise, it's not a sustained recovery
      TRUE ~ 1
    )
  ) %>%
  # Remove the temporary columns we created
  select(-next_admission_date, -days_to_next_admission, -days_to_death) %>%
  ungroup()

data <- result_df

data <- data |>
  mutate(Status30D = sustained_recovery) |>
  mutate(Status30D = if_else(Status30D == 0, "Sustained_Recovery", "No_Recovery")) |>
  mutate_at(vars(Status30D),as.factor)

data$Status30D <- relevel(data$Status30D,"No_Recovery")

################################################
################ Modelling #####################
################################################

set.seed(234)

data_folds <- rsample::group_vfold_cv(data, group = Hospital)

# Now we will create our model 

model <- logistic_reg(engine = "glm",
                      mode = "classification")

# Let's also create an xgboost model

xgb <- boost_tree() |>
  set_engine("xgboost") |>
  set_mode("classification")

############## Workflows now ###############

# Current model

current_wf <- workflow() |> 
  add_formula(Status30D ~ EWS_score) |>
  add_model(model)


# NEWS2-Light

light_wf <- workflow() |> 
  add_formula(Status30D ~ EWS_light) |>
  add_model(model)

# IEWS

full_wf <- workflow() |> 
  add_formula(Status30D ~ IEWS_Light) |>
  add_model(model)

# XGBoost

xgb_wf <- workflow() |>
  add_formula(Status30D ~ Mean_NEWS + Age + Sex + Respiration_Rate + Temperature + Saturation + Oxygen_Supplement + Blood_Pressure.Sys + Blood_Pressure.Dia + 
                Consciousness + Previous_Hosp_Fac + pca_0 + pca_1 + pca_2 + pca_3 + pca_4 + pca_5 + pca_6 + pca_7 + pca_8 + pca_9 + pca_10 + pca_11 +
                pca_12 + pca_13 + pca_14 + pca_15 + pca_16 + pca_17 + pca_18 + pca_19 + pca_20 + pca_21 + pca_22 + pca_23 + pca_24 + pca_25 + pca_26 + pca_27 + pca_28 + pca_29) |>
  add_model(xgb)

xgb_wf


# Set up parallel processing
doParallel::registerDoParallel(cores = 40)

cntrl <- control_resamples(save_pred = T)


# Internal-External validation of the current EWS (checking demographic parity also)

current_fit <- fit_resamples(current_wf,resamples = data_folds,
                             metrics = metric_set(
                               roc_auc,
                               brier_class,
                               demographic_parity(Age_Group),
                               demographic_parity(Sex),
                               demographic_parity(Department_Name_Fac),
                               demographic_parity(Hospital),
                               demographic_parity(Risk_Groups_EWS),
                               demographic_parity(Previous_Hosp_Fac),
                               demographic_parity(SKS_Category)), 
                             ,control = cntrl)

light_fit <- fit_resamples(light_wf,resamples = data_folds,
                           metrics = metric_set(
                             roc_auc,
                             brier_class,
                             demographic_parity(Age_Group),
                             demographic_parity(Sex),
                             demographic_parity(Department_Name_Fac),
                             demographic_parity(Hospital),
                             demographic_parity(Risk_Groups_EWS),
                             demographic_parity(Previous_Hosp_Fac),
                             demographic_parity(SKS_Category)), 
                           ,control = cntrl)


full_fit <- fit_resamples(full_wf,resamples = data_folds,
                          metrics = metric_set(
                            roc_auc,
                            brier_class,
                            demographic_parity(Age_Group),
                            demographic_parity(Sex),
                            demographic_parity(Department_Name_Fac),
                            demographic_parity(Hospital),
                            demographic_parity(Risk_Groups_EWS),
                            demographic_parity(Previous_Hosp_Fac),
                            demographic_parity(SKS_Category)), 
                          ,control = cntrl)

xgb_fit <- fit_resamples(xgb_wf,resamples = data_folds,
                         metrics = metric_set(
                           roc_auc,
                           brier_class,
                           demographic_parity(Age_Group),
                           demographic_parity(Sex),
                           demographic_parity(Department_Name_Fac),
                           demographic_parity(Hospital),
                           demographic_parity(Risk_Groups_EWS),
                           demographic_parity(Previous_Hosp_Fac),
                           demographic_parity(SKS_Category)), 
                         ,control = cntrl)


# Collect metrics

current_fit |> collect_metrics() 
light_fit |> collect_metrics()
full_fit |> collect_metrics()
xgb_fit |> collect_metrics()

