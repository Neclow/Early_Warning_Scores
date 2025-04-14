# Set up working directory

#########################################
############# Packages ##################
#########################################

library(tidyverse) # For data analysis
library(tidymodels) # For modelling if needed
library(patchwork) # For plot merging
library(dcurves) # For decision curve analysis
library(ggsci) # More palettes
library(probably) # Calibration
library(rms) # Classic statistical modelling
library(doParallel) # Parallel processing
library(tidylog) # Observing pre-processing numbers
library(Publish) # Table 1 creation
library(arrow) # Read parquet files
library(riskRegression) # Risk modelling
library(halfmoon) # Checking propensity
library(geomtextpath) # Text in ggplot2
library(glmnet) # Ridge regression
library(ggdist) # For further plotting
library(tidyposterior) # For posterior distributions
library(bayesplot) # For bayesian related plotting
library(ggridges) # For plotting

#######################################################

# Open procedures data

procedures <- read_parquet("procedures_newest.parquet")

# Further modification

procedures <- procedures |> 
  select(-EnterpriseID,-SKS_Code,-Procedurenavn)

# Count the number of distinct procedures each individual (per hospitalization number, i.e. CSN) has been exposed to

csns_interventions <- procedures |> 
  group_by(CSN) |> 
  distinct(SKS_Group) |> 
  arrange(CSN)

# Individuals that have received Anesthesia or Intensive Care or Surgical Operations

csns_interventions <- csns_interventions |> 
  filter(SKS_Group == "Anesthesia or Intensive Care" | SKS_Group == "Surgical Operations" | SKS_Group == "Treatment Care")

csns_interventions <- na.omit(csns_interventions)


# I want to save the csns_interventions as a parquet file

write_parquet(csns_interventions,"new_intervenetions_group.parquet")


##########################################################################################################
################ Different approach by using a different weight method using embeddings ##################
##########################################################################################################

# Add the new dataframe

data <- read_parquet("embeddings_df_weights.parquet")

# Make a small modification to hospitals

data <- data |> 
  mutate(Hospital = if_else(Hospital == "HGH, Hervel and Gentofte Hospital", "HGH, Herlev and Gentofte Hospital", Hospital)) |> 
  mutate_at(vars(Hospital),as.factor)


# Loading the new dataframe with the new intervention classification
# This includes intensive care, anesthesia, surgical operation and treatment care

new_ints <- read_parquet("new_interventions_group.parquet")

# Now create a variable called interventions 

data <- data |>
  mutate(Interventions = if_else(CSN %in% new_ints$CSN, "Intervention", "No_Intervention")) 

# New propensity model
pca_columns <- grep("^pca", names(data), value = TRUE)

# Prepare the data matrix for glmnet
X <- model.matrix(~ Age_Group + Sex + Hospital + 
                    Blood_Pressure.Sys + Temperature + Saturation + Pulse +
                    Oxygen_Supplement + Consciousness - 1, 
                  data = data)

# Add PCA columns to the matrix
if(length(pca_columns) > 0) {
  pca_matrix <- as.matrix(data[, pca_columns])
  X <- cbind(X, pca_matrix)
}

# Convert Interventions to binary for glmnet
y <- as.numeric(data$Interventions == "Intervention")

# Fit the glmnet model using ridge regression
set.seed(123)

ridge_fit <- glmnet(X, y, family = "binomial", alpha = 0, lambda = 0.005)

intervention_probs <- predict(ridge_fit, newx = X, type = "response")[,1]


# Put them into the data (Inverse probability weighting) with stabilizing weights
data <- data |>
  mutate(preds_net = intervention_probs,
         weights_new = if_else(Interventions == "Intervention", 0.4583109/preds_net, 0.5416891/(1-preds_net)))

plot_ipw <- data |> 
  tidy_smd(c(EWS_score, Age_Group, Sex, Hospital, Blood_Pressure.Sys, Temperature, 
             Saturation, Pulse, Oxygen_Supplement, Consciousness,
             all_of(pca_columns)),
           .group = Interventions,
           .wts = weights_new)

love_plot(plot_ipw) + 
  theme(legend.position = "top") + 
  theme_gray(base_size = 12) +
  labs(x = "Absolute standardized mean differences", y ="", fill = "", color = "") +
  theme(legend.position = "top") +
  scale_fill_discrete(labels = c("Unweighted","Weighted")) + 
  scale_color_discrete(labels = c("Unweighted","Weighted"))

# Mirror histogram of the weights

data |>
  ggplot(aes(preds_net, fill = Interventions)) +
  geom_mirror_histogram(bins = 50,alpha = 0.5) +
  scale_y_continuous(labels = abs) +
  labs(x = "Propensity score", fill = "Received major intervention", y = "") +
  theme_gray(base_size = 12) +
  scale_fill_discrete(labels = c("Yes", "No"))

# Check the summary of preds_net

data |>
  group_by(Interventions) |>
  summarise(mean(preds_net), max(preds_net), min(preds_net))

#########################################################################
###################### Modelling ########################################
#########################################################################

data$imp_weights <- importance_weights(data$weights_new) 

data <- data |> filter(Interventions == "No_Intervention")

# Check the unique individuals in the data

length(unique(data$PT_ID)) # 590,190 individuals

#############################################################

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
  add_model(model) |>
  add_case_weights(imp_weights)


# NEWS2-Light

light_wf <- workflow() |> 
  add_formula(Status30D ~ EWS_light) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# IEWS

full_wf <- workflow() |> 
  add_formula(Status30D ~ IEWS_Light) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# XGBoost

xgb_wf <- workflow() |>
  add_formula(Status30D ~  Age + Sex + Respiration_Rate + Temperature + Saturation + Oxygen_Supplement + Blood_Pressure.Sys + Blood_Pressure.Dia + 
                Consciousness + Previous_Hosp_Fac + pca_0 + pca_1 + pca_2 + pca_3 + pca_4 + pca_5 + pca_6 + pca_7 + pca_8 + pca_9 + pca_10 + pca_11 +
                pca_12 + pca_13 + pca_14 + pca_15 + pca_16 + pca_17 + pca_18 + pca_19 + pca_20 + pca_21 + pca_22 + pca_23 + pca_24 + pca_25 + pca_26 + pca_27 + pca_28 + pca_29) |>
  add_model(xgb) |>
  add_case_weights(imp_weights)

xgb_wf


# Age and sex

age_sex_wf <- workflow() |> 
  add_formula(Status30D ~ Age + Sex) |> 
  add_model(model) |> 
  add_case_weights(imp_weights)

age_sex_wf

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

age_sex_fit <- fit_resamples(age_sex_wf,resamples = data_folds,
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

###########

# Calculate weighted and unweighted metrics using 100 bootstraps

# Function to calculate weighted and unweighted metrics with bootstrap CIs
calculate_metrics_with_bootstrap <- function(fit_object, data, model_name, n_bootstrap = 100) {
  # Get predictions
  preds <- fit_object %>% 
    collect_predictions() %>%
    arrange(.row) %>%
    mutate(
      weights = data$weights_new,
      mort30D = if_else(Status30D == "Deceased", 1, 0)
    )
  
  # Calculate point estimates
  auc_weighted <- MetricsWeighted::AUC(preds$mort30D, preds$.pred_Deceased, w = preds$weights)
  auc_unweighted <- MetricsWeighted::AUC(preds$mort30D, preds$.pred_Deceased)
  brier_weighted <- MetricsWeighted::mse(preds$mort30D, preds$.pred_Deceased, w = preds$weights)
  brier_unweighted <- MetricsWeighted::mse(preds$mort30D, preds$.pred_Deceased)
  
  # Function to calculate metrics for a bootstrap sample
  calculate_bootstrap_metrics <- function(data_sample, indices) {
    # Create bootstrap sample
    boot_sample <- data_sample[indices, ]
    
    # Calculate metrics for this bootstrap sample
    auc_w <- MetricsWeighted::AUC(boot_sample$mort30D, boot_sample$.pred_Deceased, w = boot_sample$weights)
    auc_uw <- MetricsWeighted::AUC(boot_sample$mort30D, boot_sample$.pred_Deceased)
    brier_w <- MetricsWeighted::mse(boot_sample$mort30D, boot_sample$.pred_Deceased, w = boot_sample$weights)
    brier_uw <- MetricsWeighted::mse(boot_sample$mort30D, boot_sample$.pred_Deceased)
    
    return(c(auc_w = auc_w, auc_uw = auc_uw, brier_w = brier_w, brier_uw = brier_uw))
  }
  
  # Perform bootstrap resampling
  set.seed(42 + which(c("NEWS2", "NEWS2-Light", "IEWS-Light", "XGBoost", "Age+Sex") == model_name))
  bootstrap_results <- matrix(NA, nrow = n_bootstrap, ncol = 4)
  colnames(bootstrap_results) <- c("auc_w", "auc_uw", "brier_w", "brier_uw")
  
  # Use parallel processing if available
  if (exists("doParallel::registerDoParallel")) {
    bootstrap_results <- foreach(i = 1:n_bootstrap, .combine = rbind) %dopar% {
      indices <- sample(1:nrow(preds), nrow(preds), replace = TRUE)
      calculate_bootstrap_metrics(preds, indices)
    }
  } else {
    for (i in 1:n_bootstrap) {
      indices <- sample(1:nrow(preds), nrow(preds), replace = TRUE)
      bootstrap_results[i, ] <- calculate_bootstrap_metrics(preds, indices)
    }
  }
  
  # Calculate confidence intervals from bootstrap distribution
  auc_w_ci <- quantile(bootstrap_results[, "auc_w"], probs = c(0.025, 0.975), na.rm = TRUE)
  auc_uw_ci <- quantile(bootstrap_results[, "auc_uw"], probs = c(0.025, 0.975), na.rm = TRUE)
  brier_w_ci <- quantile(bootstrap_results[, "brier_w"], probs = c(0.025, 0.975), na.rm = TRUE)
  brier_uw_ci <- quantile(bootstrap_results[, "brier_uw"], probs = c(0.025, 0.975), na.rm = TRUE)
  
  # Return results
  return(data.frame(
    model = model_name,
    AUC_Weighted = auc_weighted,
    AUC_Weighted_LCI = auc_w_ci[1],
    AUC_Weighted_UCI = auc_w_ci[2],
    AUC_Unweighted = auc_unweighted,
    AUC_Unweighted_LCI = auc_uw_ci[1],
    AUC_Unweighted_UCI = auc_uw_ci[2],
    Brier_Weighted = brier_weighted,
    Brier_Weighted_LCI = brier_w_ci[1],
    Brier_Weighted_UCI = brier_w_ci[2],
    Brier_Unweighted = brier_unweighted,
    Brier_Unweighted_LCI = brier_uw_ci[1],
    Brier_Unweighted_UCI = brier_uw_ci[2]
  ))
}

# Calculate metrics for all models
metrics_current <- calculate_metrics_with_bootstrap(current_fit, data, "NEWS2")
metrics_light <- calculate_metrics_with_bootstrap(light_fit, data, "NEWS2-Light")
metrics_full <- calculate_metrics_with_bootstrap(full_fit, data, "IEWS-Light")
metrics_xgb <- calculate_metrics_with_bootstrap(xgb_fit, data, "XGBoost")
metrics_as <- calculate_metrics_with_bootstrap(age_sex_fit, data, "Age+Sex")

# Combine all metrics
all_metrics_bootstrap <- bind_rows(
  metrics_current,
  metrics_light,
  metrics_full,
  metrics_xgb,
  metrics_as
)

# Create a tidy format for plotting
tidy_metrics_bootstrap <- bind_rows(
  # AUC metrics - weighted
  all_metrics_bootstrap %>% 
    select(model, AUC_Weighted, AUC_Weighted_LCI, AUC_Weighted_UCI) %>%
    mutate(
      metric = "AUC", 
      weighting = "Weighted", 
      estimate = AUC_Weighted, 
      lower_ci = AUC_Weighted_LCI, 
      upper_ci = AUC_Weighted_UCI
    ),
  
  # AUC metrics - unweighted
  all_metrics_bootstrap %>% 
    select(model, AUC_Unweighted, AUC_Unweighted_LCI, AUC_Unweighted_UCI) %>%
    mutate(
      metric = "AUC", 
      weighting = "Unweighted", 
      estimate = AUC_Unweighted, 
      lower_ci = AUC_Unweighted_LCI, 
      upper_ci = AUC_Unweighted_UCI
    ),
  
  # Brier score metrics - weighted
  all_metrics_bootstrap %>% 
    select(model, Brier_Weighted, Brier_Weighted_LCI, Brier_Weighted_UCI) %>%
    mutate(
      metric = "Brier Score", 
      weighting = "Weighted", 
      estimate = Brier_Weighted, 
      lower_ci = Brier_Weighted_LCI, 
      upper_ci = Brier_Weighted_UCI
    ),
  
  # Brier score metrics - unweighted
  all_metrics_bootstrap %>% 
    select(model, Brier_Unweighted, Brier_Unweighted_LCI, Brier_Unweighted_UCI) %>%
    mutate(
      metric = "Brier Score", 
      weighting = "Unweighted", 
      estimate = Brier_Unweighted, 
      lower_ci = Brier_Unweighted_LCI, 
      upper_ci = Brier_Unweighted_UCI
    )
) %>%
  select(model, metric, weighting, estimate, lower_ci, upper_ci)

# Set model factor levels for consistent ordering
tidy_metrics_bootstrap$model <- factor(tidy_metrics_bootstrap$model, 
                                       levels = c("NEWS2", "NEWS2-Light", "IEWS-Light", "XGBoost","Age+Sex"))

# Create a plot with the metrics
overall_performance_plot <- ggplot(tidy_metrics_bootstrap, 
                                   aes(x = model, y = estimate, color = weighting)) +
  geom_point(position = position_dodge(width = 0.5), size = 3) +
  geom_pointinterval(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.5),
    width = 0.2
  ) +
  # Facet by metric with free y scales
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    y = "Value", 
    x = NULL,
    color = "Weighting"
  ) +
  theme_gray(base_size = 14) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# Display the plot
overall_performance_plot


# Compute the metrics for each hospital

# Function to calculate weighted metrics by hospital with bootstrap confidence intervals

calculate_metrics_by_hospital_with_bootstrap_ci <- function(fit_object, data, model_name, n_bootstrap = 100) {
  # Get predictions and arrange by row
  preds <- fit_object %>% 
    collect_predictions() %>%
    arrange(.row) %>%
    mutate(
      weights = data$weights_new,
      mort30D = if_else(Status30D == "Deceased", 1, 0),
      Hospital = data$Hospital
    )
  
  # Function to calculate metrics for a bootstrap sample
  calculate_bootstrap_metrics <- function(hospital_data, indices) {
    # Create bootstrap sample
    boot_sample <- hospital_data[indices, ]
    
    # Calculate metrics for this bootstrap sample
    auc <- MetricsWeighted::AUC(boot_sample$mort30D, boot_sample$.pred_Deceased, w = boot_sample$weights)
    brier <- MetricsWeighted::mse(boot_sample$mort30D, boot_sample$.pred_Deceased, w = boot_sample$weights)
    
    return(c(auc = auc, brier = brier))
  }
  
  # Initialize results dataframe
  hospital_metrics <- data.frame()
  
  # Process each hospital separately
  for (hosp in unique(preds$Hospital)) {
    # Filter data for this hospital
    hosp_data <- preds %>% filter(Hospital == hosp)
    n_patients <- nrow(hosp_data)
    
    # Calculate point estimates
    auc_weighted <- MetricsWeighted::AUC(hosp_data$mort30D, hosp_data$.pred_Deceased, w = hosp_data$weights)
    brier_weighted <- MetricsWeighted::mse(hosp_data$mort30D, hosp_data$.pred_Deceased, w = hosp_data$weights)
    
    # Perform bootstrap resampling
    set.seed(42 + which(unique(preds$Hospital) == hosp)) # Different seed for each hospital
    bootstrap_results <- matrix(NA, nrow = n_bootstrap, ncol = 2)
    colnames(bootstrap_results) <- c("auc", "brier")
    
    for (i in 1:n_bootstrap) {
      # Sample with replacement
      indices <- sample(1:n_patients, n_patients, replace = TRUE)
      bootstrap_results[i, ] <- calculate_bootstrap_metrics(hosp_data, indices)
    }
    
    # Calculate confidence intervals from bootstrap distribution
    auc_ci <- quantile(bootstrap_results[, "auc"], probs = c(0.025, 0.975), na.rm = TRUE)
    brier_ci <- quantile(bootstrap_results[, "brier"], probs = c(0.025, 0.975), na.rm = TRUE)
    
    # Add to results
    hospital_metrics <- bind_rows(
      hospital_metrics,
      data.frame(
        Hospital = hosp,
        AUC_Weighted = auc_weighted,
        Brier_Score_Weighted = brier_weighted,
        AUC_LCI = auc_ci[1],
        AUC_UCI = auc_ci[2],
        Brier_LCI = brier_ci[1],
        Brier_UCI = brier_ci[2],
        n_patients = n_patients,
        model = model_name
      )
    )
  }
  
  return(hospital_metrics %>% arrange(desc(n_patients)))
}

# Calculate metrics by hospital for each model using bootstrap
hospital_metrics_current <- calculate_metrics_by_hospital_with_bootstrap_ci(current_fit, data, "NEWS2")
hospital_metrics_light <- calculate_metrics_by_hospital_with_bootstrap_ci(light_fit, data, "NEWS2-Light")
hospital_metrics_full <- calculate_metrics_by_hospital_with_bootstrap_ci(full_fit, data, "IEWS-Light")
hospital_metrics_xgb <- calculate_metrics_by_hospital_with_bootstrap_ci(xgb_fit, data, "XGBoost")
hospital_metrics_as <- calculate_metrics_by_hospital_with_bootstrap_ci(age_sex_fit, data, "Age+Sex")

# Combine all hospital metrics
all_hospital_metrics <- bind_rows(
  hospital_metrics_current,
  hospital_metrics_light,
  hospital_metrics_full,
  hospital_metrics_xgb,
  hospital_metrics_as
)

# Create enhanced heatmap for AUC with bootstrap confidence intervals
auc_heatmap_with_ci <- ggplot(all_hospital_metrics, 
                              aes(x = model, y = reorder(Hospital, n_patients))) +
  # Add tiles colored by AUC value
  geom_tile(aes(fill = AUC_Weighted), color = "white") +
  # Add text showing AUC value and CI
  geom_text(aes(label = sprintf("%.3f\n[%.3f-%.3f]", 
                                AUC_Weighted, AUC_LCI, AUC_UCI)),
            size = 5) +
  # Add color scale
  scale_fill_gradient(low = "white", high = "steelblue") +
  # Add labels
  labs(
    x = "",
    y = "Hospital",
    fill = "Weighted AUC\n(higher better)"
  ) +
  # Customize theme
  theme_gray(base_size = 14) +
  theme(
    axis.text.x = element_blank(),
    legend.position = "right",
    panel.grid = element_blank()
  )

# Create enhanced heatmap for Brier Score with bootstrap confidence intervals
brier_heatmap_with_ci <- ggplot(all_hospital_metrics, 
                                aes(x = model, y = reorder(Hospital, n_patients))) +
  # Add tiles colored by Brier score value (lower is better)
  geom_tile(aes(fill = Brier_Score_Weighted), color = "white") +
  # Add text showing Brier score value and CI
  geom_text(aes(label = sprintf("%.3f\n[%.3f-%.3f]", 
                                Brier_Score_Weighted, Brier_LCI, Brier_UCI)),
            size = 5) +
  # Add color scale (reversed so darker is worse)
  scale_fill_gradient(low = "white", high = "coral", trans = "reverse") +
  # Add labels
  labs(
    x = "Model",
    y = "",
    fill = "Weighted Brier\n(lower is better)"
  ) +
  # Customize theme
  theme_gray(base_size = 14) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid = element_blank()
  )

# Display the enhanced heatmaps
auc_heatmap_with_ci /brier_heatmap_with_ci

##################################################################
########## Compare the Net Benefit of various models #############
##################################################################

# First, let's create a function to extract net benefit at specific thresholds
extract_net_benefit <- function(fit_object, data, thresholds = c(0.05, 0.1, 0.15, 0.2)) {
  # Get predictions
  preds <- fit_object |> 
    collect_predictions() |>
    arrange(.row) |>
    mutate(mort30D = if_else(Status30D == "Deceased", 1, 0),
           weights = data$weights_new)
  
  # Calculate net benefit for each fold and threshold
  nb_results <- data.frame()
  
  for (fold_id in unique(preds$id)) {
    fold_preds <- preds |> filter(id == fold_id)
    
    for (t in thresholds) {
      # Classify patients based on threshold
      tp <- fold_preds$mort30D == 1 & fold_preds$.pred_Deceased >= t
      fp <- fold_preds$mort30D == 0 & fold_preds$.pred_Deceased >= t
      
      # Calculate weighted true positives and false positives
      wtp <- sum(fold_preds$weights[tp])
      wfp <- sum(fold_preds$weights[fp])
      
      # Total weighted population
      total_w <- sum(fold_preds$weights)
      
      # Net benefit formula
      nb <- (wtp/total_w) - (wfp/total_w) * (t/(1-t))
      
      # Store results
      nb_results <- bind_rows(nb_results, 
                              data.frame(id = fold_id, 
                                         threshold = t, 
                                         net_benefit = nb))
    }
  }
  
  return(nb_results)
}

# Define thresholds of interest
thresholds_of_interest <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)

# Extract net benefit for each model
nb_current <- extract_net_benefit(current_fit, data, thresholds_of_interest) |> 
  mutate(model = "NEWS2")
nb_light <- extract_net_benefit(light_fit, data, thresholds_of_interest) |> 
  mutate(model = "NEWS2-Light")
nb_full <- extract_net_benefit(full_fit, data, thresholds_of_interest) |> 
  mutate(model = "IEWS-Light")
nb_xgb <- extract_net_benefit(xgb_fit, data, thresholds_of_interest) |> 
  mutate(model = "XGBoost")
nb_as <- extract_net_benefit(age_sex_fit, data, thresholds_of_interest) |> 
  mutate(model = "Age+Sex")

# Combine all net benefit results
all_nb <- bind_rows(nb_current, nb_light, nb_full, nb_xgb, nb_as)

# Create a boxplot to visualize the distribution of net benefit across folds
ggplot(all_nb, aes(x = model, y = net_benefit, fill = model)) +
  geom_boxplot() +
  facet_wrap(~ threshold, labeller = labeller(threshold = function(x) paste0("Threshold = ", x))) +
  labs(x = "",
       y = "Net Benefit") +
  theme_gray(base_size = 14) +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 45, hjust = 1))

# Now let's perform Bayesian analysis for each threshold
prepare_nb_for_tidyposterior <- function(all_nb_data, threshold_value) {
  nb_data <- all_nb_data |> 
    filter(threshold == threshold_value) |> 
    select(id, model, net_benefit) |>
    rename(.estimate = net_benefit) |>
    pivot_wider(
      id_cols = id,
      names_from = model,
      values_from = .estimate
    )
  
  return(nb_data)
}

# Perform Bayesian analysis for each threshold
nb_post_results <- list()
nb_diffs_results <- list()
nb_dens_results <- list()

for (t in thresholds_of_interest) {
  # Prepare data
  nb_for_post <- prepare_nb_for_tidyposterior(all_nb, t)
  
  # Fit Bayesian model
  set.seed(123)
  nb_post <- perf_mod(
    nb_for_post,
    seed = 1234,
    iter = 4000,
    chains = 4,
    cores = 40,
    refresh = 0
  )
  
  # Calculate pairwise differences
  nb_diffs <- contrast_models(nb_post)
  
  # Extract posterior distributions
  nb_dens <- tidy(nb_post)
  
  # Store results
  nb_post_results[[as.character(t)]] <- nb_post
  nb_diffs_results[[as.character(t)]] <- nb_diffs
  nb_dens_results[[as.character(t)]] <- nb_dens
}

# Plot the results

# Create an empty dataframe to store the posterior samples
posterior_samples <- data.frame()

# Loop through each threshold in nb_diffs_results
for (threshold in names(nb_diffs_results)) {
  # Get the contrast results for this threshold
  diff_result <- nb_diffs_results[[threshold]]
  
  # Extract the difference column for NEWS2 vs NEWS2-Light
  diff_data <- as.data.frame(diff_result) %>%
    filter(contrast == "NEWS2 vs. Age+Sex") %>%
    select(difference) %>%
    mutate(threshold = as.numeric(threshold))
  
  # Append to results dataframe
  posterior_samples <- bind_rows(posterior_samples, diff_data)
}

# Make all possible comparisons

# Function to combine all model comparison plots into one figure
create_comprehensive_model_comparison <- function(nb_diffs_results, models_to_compare) {
  # Create an empty dataframe for all posterior samples
  all_posterior_samples <- data.frame()
  
  # Process each model comparison
  for (model_comparison in models_to_compare) {
    # Parse the comparison string to get model names
    models <- strsplit(model_comparison, " vs ")[[1]]
    model1 <- models[1]
    model2 <- models[2]
    
    cat("Processing comparison:", model_comparison, "\n")
    
    # Loop through each threshold in nb_diffs_results
    for (threshold in names(nb_diffs_results)) {
      # Get the contrast results for this threshold
      diff_result <- nb_diffs_results[[threshold]]
      
      # Convert to data frame for easier manipulation
      diff_df <- as.data.frame(diff_result)
      
      # Check all contrast strings in this diff_result to debug
      all_contrasts <- unique(diff_df$contrast)
      cat("Contrasts available for threshold", threshold, ":", paste(all_contrasts, collapse=", "), "\n")
      
      # Try different variants of the contrast string
      variants <- c(
        model_comparison,                  # "NEWS2 vs XGBoost"
        paste0(model1, " vs. ", model2),   # "NEWS2 vs. XGBoost"
        paste0(model1, " versus ", model2) # "NEWS2 versus XGBoost"
      )
      
      # Find matching rows
      matching_rows <- NULL
      for (variant in variants) {
        matching_rows <- diff_df[diff_df$contrast == variant, ]
        if (nrow(matching_rows) > 0) {
          cat("Found match with variant:", variant, "\n")
          break
        }
      }
      
      # If we found matching rows, process them
      if (!is.null(matching_rows) && nrow(matching_rows) > 0) {
        diff_data <- matching_rows %>%
          select(difference) %>%
          mutate(
            threshold = as.numeric(threshold),
            comparison = model_comparison
          )
        
        # Append to results dataframe
        all_posterior_samples <- bind_rows(all_posterior_samples, diff_data)
      } else {
        cat("No matching contrast found for", model_comparison, "at threshold", threshold, "\n")
      }
    }
  }
  
  # Check if we have data to plot
  if (nrow(all_posterior_samples) == 0) {
    stop("No matching data found for any of the specified model comparisons")
  }
  
  # Ensure comparison is a factor with the right levels for consistent ordering
  all_posterior_samples$comparison <- factor(
    all_posterior_samples$comparison,
    levels = models_to_compare
  )
  
  # Create a density plot of the posterior distributions by threshold and comparison
  comparison_plot <- ggplot(all_posterior_samples, 
                            aes(x = difference, y = factor(threshold), 
                                fill = factor(threshold))) +
    # Add density ridges
    geom_density_ridges(alpha = 0.7, scale = 0.9, rel_min_height = 0.01) +
    # Add a vertical line at x=0
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    # Facet by model comparison
    facet_wrap(~ comparison, scales = "free_x", ncol = 2) +
    # Add labels
    labs(
      x = "Difference in Net Benefit",
      y = "Decision Threshold",
      fill = "Threshold"
    ) +
    # Customize theme
    theme_gray(base_size = 14) +
    theme(
      legend.position = "right",
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold"),
      axis.text.x = element_text(angle = 45, hjust = 1)
    )
  
  return(comparison_plot)
}

# Define all possible model pairs to include
all_models <- c("NEWS2", "NEWS2-Light", "IEWS-Light", "XGBoost", "Age+Sex")
models_to_compare <- c()

# Generate all pairwise comparisons
for (i in 1:(length(all_models)-1)) {
  for (j in (i+1):length(all_models)) {
    models_to_compare <- c(models_to_compare, 
                           paste(all_models[i], "vs", all_models[j]))
  }
}

# Display generated comparisons
print(models_to_compare)

# Create the comprehensive comparison plot
comprehensive_model_comparison <- create_comprehensive_model_comparison(nb_diffs_results, models_to_compare)

# Display the plot with a larger size
print(comprehensive_model_comparison)


########### Create the net benefit comparisons per age group ###############

# Function to extract net benefit by group and threshold
extract_net_benefit_by_group <- function(fit_object, data, thresholds, group_var) {
  # Get predictions and add group variable
  preds <- fit_object |> 
    collect_predictions() |>
    arrange(.row) |>
    mutate(mort30D = if_else(Status30D == "Deceased", 1, 0),
           weights = data$weights_new,
           group = data[[group_var]]) # Add the grouping variable
  
  # Calculate net benefit for each fold, threshold, and group
  nb_results <- data.frame()
  
  for (fold_id in unique(preds$id)) {
    fold_preds <- preds |> filter(id == fold_id)
    
    for (grp in unique(fold_preds$group)) {
      group_preds <- fold_preds |> filter(group == grp)
      
      # Skip if group has no data in this fold
      if (nrow(group_preds) == 0) next
      
      for (t in thresholds) {
        # Classify patients based on threshold
        tp <- group_preds$mort30D == 1 & group_preds$.pred_Deceased >= t
        fp <- group_preds$mort30D == 0 & group_preds$.pred_Deceased >= t
        
        # Calculate weighted true positives and false positives
        wtp <- sum(group_preds$weights[tp])
        wfp <- sum(group_preds$weights[fp])
        
        # Total weighted population in this group and fold
        total_w <- sum(group_preds$weights)
        
        # Handle cases where total_w might be zero or t is 1
        if (total_w == 0 || t == 1) {
          nb <- 0
        } else {
          # Net benefit formula
          nb <- (wtp/total_w) - (wfp/total_w) * (t/(1-t))
        }
        
        # Store results
        nb_results <- bind_rows(nb_results, 
                                data.frame(id = fold_id, 
                                           threshold = t, 
                                           group = grp,
                                           net_benefit = nb))
      }
    }
  }
  
  return(nb_results)
}

# Extract net benefit by age group for each model
thresholds_of_interest <- c(0.05, 0.1, 0.15, 0.2, 0.25, 0.3)

nb_current_age <- extract_net_benefit_by_group(current_fit, data, thresholds_of_interest, "Age_Group") |> 
  mutate(model = "NEWS2")
nb_light_age <- extract_net_benefit_by_group(light_fit, data, thresholds_of_interest, "Age_Group") |> 
  mutate(model = "NEWS2-Light")
nb_full_age <- extract_net_benefit_by_group(full_fit, data, thresholds_of_interest, "Age_Group") |> 
  mutate(model = "IEWS-Light")
nb_xgb_age <- extract_net_benefit_by_group(xgb_fit, data, thresholds_of_interest, "Age_Group") |> 
  mutate(model = "XGBoost")
nb_as_age <- extract_net_benefit_by_group(age_sex_fit, data, thresholds_of_interest, "Age_Group") |> 
  mutate(model = "Age+Sex")

# Combine all net benefit results by age
all_nb_by_age <- bind_rows(nb_current_age, nb_light_age, nb_full_age, nb_xgb_age, nb_as_age)

# Function to prepare data for tidyposterior by group and threshold
prepare_nb_by_group_for_tidyposterior <- function(all_nb_data, threshold_value, group_value) {
  nb_data <- all_nb_data |> 
    filter(threshold == threshold_value, group == group_value) |> 
    select(id, model, net_benefit) |>
    rename(.estimate = net_benefit) |>
    pivot_wider(
      id_cols = id,
      names_from = model,
      values_from = .estimate
    ) |>
    # Remove rows with NA estimates, as perf_mod can't handle them
    na.omit() 
  
  return(nb_data)
}

# Perform Bayesian analysis for each age group and threshold
nb_diffs_by_age_results <- list()
age_groups <- unique(data$Age_Group)

for (ag in age_groups) {
  nb_diffs_by_age_results[[ag]] <- list()
  for (t in thresholds_of_interest) {
    cat("Processing Age Group:", ag, "- Threshold:", t, "\n")
    
    # Prepare data
    nb_for_post_age <- prepare_nb_by_group_for_tidyposterior(all_nb_by_age, t, ag)
    
    # Check if we have enough data (at least 2 folds with non-NA estimates for all models)
    if (nrow(nb_for_post_age) < 2 || any(sapply(nb_for_post_age, function(x) sum(!is.na(x))) < 2)) {
      cat("Skipping due to insufficient data.\n")
      nb_diffs_by_age_results[[ag]][[as.character(t)]] <- NULL # Store NULL if skipped
      next
    }
    
    # Fit Bayesian model
    set.seed(123)
    nb_post_age <- tryCatch({
      perf_mod(
        nb_for_post_age,
        seed = 1234,
        iter = 4000,
        chains = 4,
        cores = 4, # Use fewer cores to avoid potential issues
        refresh = 0
      )
    }, error = function(e) {
      cat("Error fitting perf_mod:", conditionMessage(e), "\n")
      return(NULL) # Return NULL if perf_mod fails
    })
    
    # Skip if perf_mod failed
    if (is.null(nb_post_age)) {
      cat("Skipping contrast calculation due to perf_mod error.\n")
      nb_diffs_by_age_results[[ag]][[as.character(t)]] <- NULL
      next
    }
    
    # Calculate pairwise differences
    nb_diffs_age <- contrast_models(nb_post_age)
    
    # Store results
    nb_diffs_by_age_results[[ag]][[as.character(t)]] <- nb_diffs_age
  }
}

nb_diffs_by_age_results


# --- Helper Function 1: Tidying Contrast Results ---

tidy_contrast_results <- function(nb_diffs_by_group_results) {
  purrr::imap_dfr(nb_diffs_by_group_results, ~{ # Iterate over groups (.y = group name)
    purrr::imap_dfr(.x, ~{ # Iterate over thresholds within a group (.y = threshold name)
      if (is.null(.x)) return(NULL) # Skip if threshold result is NULL
      # Convert contrast results to DF and add numeric threshold directly
      as.data.frame(.x) %>% 
        dplyr::mutate(threshold = as.numeric(.y)) 
    }) # Removed .id = "threshold_chr"
  }, .id = "group") %>% # Add group column
    # The threshold column should exist and be numeric now
    dplyr::select(group, threshold, contrast, difference) # Keep relevant columns
}

# --- Helper Function 2: Preparing Data for Comparison Plot (Corrected Separator) ---
prepare_comparison_data <- function(tidy_diffs, all_models = c("NEWS2", "NEWS2-Light", "IEWS-Light", "XGBoost", "Age+Sex")) {
  
  # Generate target comparison strings (forward and backward) using " vs. " separator
  target_comparisons_df <- expand.grid(model1 = all_models, model2 = all_models, stringsAsFactors = FALSE) %>%
    dplyr::filter(model1 < model2) %>% # Keep unique pairs
    dplyr::mutate(
      comparison_label = paste(model1, "vs", model2), # Keep this for final plot label
      # Use the correct separator found in the contrast_models output
      contrast_forward = paste(model1, "vs.", model2), 
      contrast_backward = paste(model2, "vs.", model1) # Also check reverse order with correct separator
    ) %>%
    # Select only the columns needed for joining
    dplyr::select(comparison_label, contrast_forward, contrast_backward)
  
  # Join, filter, and handle contrast directions
  prepared_data <- tidy_diffs %>%
    # Match forward contrasts (e.g., "NEWS2 vs. XGBoost")
    dplyr::left_join(target_comparisons_df, by = c("contrast" = "contrast_forward")) %>%
    # Match backward contrasts (e.g., "XGBoost vs. NEWS2") where forward didn't match
    dplyr::left_join(
      # Select the comparison label and the backward contrast name
      target_comparisons_df %>% dplyr::select(comparison_label_rev = comparison_label, contrast_backward), 
      by = c("contrast" = "contrast_backward")
    ) %>%
    dplyr::mutate(
      # Use the comparison label from whichever join succeeded
      comparison = dplyr::coalesce(comparison_label, comparison_label_rev),
      # Negate difference if the backward contrast was matched (i.e., comparison_label is NA)
      difference = ifelse(is.na(comparison_label) & !is.na(comparison_label_rev), -difference, difference) 
    ) %>%
    # Keep only rows that successfully matched one of the target comparison formats
    dplyr::filter(!is.na(comparison)) %>%
    # Select the final columns needed for plotting
    dplyr::select(group, threshold, comparison, difference) 
  
  # Check if any rows remain after joining and filtering
  if (nrow(prepared_data) == 0) {
    stop("No matching contrasts found even after correcting the separator. Check contrast names and model names.")
  }
  
  return(prepared_data)
}

# --- Helper Function 3: Plotting Comparison Ridges ---
plot_comparison_ridges <- function(comparison_data, models_to_compare_labels, group_to_plot) {
  
  # Filter data for the specific group
  plot_data_group <- comparison_data %>%
    dplyr::filter(group == group_to_plot)
  
  # Check if data exists for this group
  if (nrow(plot_data_group) == 0) {
    warning(paste("No data found for group:", group_to_plot))
    return(NULL) # Return NULL if no data for this group
  }
  
  # Ensure factors for consistent plotting order within the group's data
  plot_data <- plot_data_group %>%
    dplyr::mutate(
      # Order comparisons based on the predefined list of labels ("A vs B" format)
      comparison = factor(comparison, levels = intersect(models_to_compare_labels, unique(comparison))),
      # Group is now constant, but keep it if needed for consistency
      group = factor(group, levels = group_to_plot) 
    ) %>%
    # Remove any comparison levels not present in this group's data
    dplyr::filter(!is.na(comparison))
  
  if (nrow(plot_data) == 0) {
    warning(paste("No valid comparison data remaining after factoring for group:", group_to_plot))
    return(NULL) 
  }
  
  # Create the plot (Removed facet_grid, added ggtitle)
  ggplot(plot_data, 
         aes(x = difference, y = factor(threshold), 
             fill = factor(threshold))) +
    geom_density_ridges(alpha = 0.7, scale = 0.9, rel_min_height = 0.01) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    facet_wrap(~ comparison, scales = "free_x", ncol = 2) + # Facet only by comparison now
    labs(
      title = paste("Net Benefit Comparison for Age Group:", group_to_plot), # Add title
      x = "Difference in Net Benefit",
      y = "Decision Threshold",
      fill = "Threshold"
    ) +
    theme_gray(base_size = 11) + 
    theme(
      legend.position = "right",
      plot.title = element_text(hjust = 0.5, face = "bold"), # Center title
      panel.grid.major.x = element_line(color = "gray90"),
      panel.grid.minor = element_blank(),
      strip.text = element_text(face = "bold", size = 8), # Adjusted strip text for comparison
      axis.text.x = element_text(angle = 45, hjust = 1, size = 7) 
    )
}


# --- Code that calls the functions (Modified to create separate plots) ---
# Define necessary inputs
all_models <- c("NEWS2", "NEWS2-Light", "IEWS-Light", "XGBoost", "Age+Sex")

# Generate the comparison labels in "A vs B" format for plot ordering
comparison_pairs <- expand.grid(model1 = all_models, model2 = all_models, stringsAsFactors = FALSE) %>%
  dplyr::filter(model1 < model2) # Get unique pairs where model1 comes before model2 alphabetically/factor levels

models_to_compare_labels <- paste(comparison_pairs$model1, "vs", comparison_pairs$model2)


# Get the list of groups for which data was processed successfully
# Ensure the raw 'nb_diffs_by_age_results' exists before trying to access it
if(exists("nb_diffs_by_age_results") && !is.null(nb_diffs_by_age_results) && length(nb_diffs_by_age_results) > 0) {
  
  processed_age_groups <- names(which(sapply(nb_diffs_by_age_results, function(threshold_list) length(purrr::compact(threshold_list)) > 0)))
  
  # Define the specific groups you want plots for
  target_age_groups <- c("18-65", "66-80", "80+") 
  
  # Filter processed_age_groups to only include target groups that actually have data
  groups_to_plot <- intersect(processed_age_groups, target_age_groups)
  
  if(length(groups_to_plot) > 0) {
    
    # 1. Tidy the results (only needs to be done once)
    tidy_results <- tidy_contrast_results(nb_diffs_by_age_results)
    
    # 2. Prepare the data for plotting (only needs to be done once)
    comparison_plot_data <- prepare_comparison_data(tidy_results, all_models)
    
    # 3. Create a plot for each target group
    plot_list <- list() # Optional: Store plots in a list
    
    for(age_group in groups_to_plot) {
      cat("Generating plot for age group:", age_group, "\n")
      
      # Create the plot for the current age group
      current_plot <- plot_comparison_ridges(
        comparison_plot_data, 
        models_to_compare_labels, 
        age_group # Pass only the current group
      )
      
      # Assign the plot to a dynamically named variable
      plot_name <- paste0("plot_age_", gsub("[^a-zA-Z0-9_]", "_", age_group, perl=TRUE)) # Sanitize name
      assign(plot_name, current_plot, envir = .GlobalEnv) # Assign to global environment
      
      # Optional: store in list and/or print immediately
      # plot_list[[plot_name]] <- current_plot
      # print(current_plot) 
      
      cat("Created plot object:", plot_name, "\n")
    }
    
  } else {
    print("None of the target age groups had sufficient data to plot.") 
  }
  
} else {
  print("No data available (nb_diffs_by_age_results is missing or empty) to create comparison plots by age group.") 
}

plot_age_18_65
plot_age_66_80
plot_age_80_
