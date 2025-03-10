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


##########################################################################################################
################ Creation of IPWs using embeddings of sentence transformers ##############################
##########################################################################################################

# Add the new dataframe

data <- read_parquet("embeddings_df_weights.parquet")

# Make a small modification to hospitals

data <- data |> 
  mutate(Hospital = if_else(Hospital == "HGH, Hervel and Gentofte Hospital", "HGH, Herlev and Gentofte Hospital", Hospital)) |> 
  mutate_at(vars(Hospital),as.factor)

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

ridge_fit <- glmnet(X, y, family = "binomial", alpha = 0, lambda = 0.001)

intervention_probs <- predict(ridge_fit, newx = X, type = "response")[,1]


# Put them into the data (Inverse probability weighting) with stabilizing weights
data <- data |>
  mutate(preds_net = intervention_probs,
         weights_new = if_else(Interventions == "Intervention", 0.2707588/preds_net, 0.7292412/(1-preds_net)))

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

###########

# Calculate weighted and unweighted metrics for all models using normal approximation

calculate_metrics <- function(fit_object, data) {
  fit_object |> 
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights_new,
           mort30D = if_else(Status30D == "Deceased",1,0)) |>
    group_by(id) |>
    summarise(AUC_Weighted = MetricsWeighted::AUC(mort30D, .pred_Deceased, w = weights),
              AUC_Unweighted = MetricsWeighted::AUC(mort30D, .pred_Deceased),
              Brier_Score_Weighted = MetricsWeighted::mse(mort30D,.pred_Deceased,w = weights),
              Brier_Score_Unweighted = MetricsWeighted::mse(mort30D,.pred_Deceased)) |>
    ungroup() |>
    summarise(Mean_AUC = mean(AUC_Weighted), Mean_Brier = mean(Brier_Score_Weighted), 
              SD_AUC = sd(AUC_Weighted), SD_Brier = sd(Brier_Score_Weighted),
              Mean_AUC_Unweighted = mean(AUC_Unweighted), Mean_Brier_Unweighted = mean(Brier_Score_Unweighted), 
              SD_AUC_Unweighted = sd(AUC_Unweighted), SD_Brier_Unweighted = sd(Brier_Score_Unweighted)) |>
    mutate(Lower_AUC = Mean_AUC - 1.96*(SD_AUC/sqrt(10)), 
           Upper_AUC = Mean_AUC + 1.96*(SD_AUC/sqrt(10)), 
           Lower_Brier = Mean_Brier - 1.96*(SD_Brier/sqrt(10)), 
           Upper_Brier = Mean_Brier + 1.96*(SD_Brier/sqrt(10)),
           Lower_AUC_Unweighted = Mean_AUC_Unweighted - 1.96*(SD_AUC_Unweighted/sqrt(10)), 
           Upper_AUC_Unweighted = Mean_AUC_Unweighted + 1.96*(SD_AUC_Unweighted/sqrt(10)), 
           Lower_Brier_Unweighted = Mean_Brier_Unweighted - 1.96*(SD_Brier_Unweighted/sqrt(10)), 
           Upper_Brier_Unweighted = Mean_Brier_Unweighted + 1.96*(SD_Brier_Unweighted/sqrt(10)))
}

# Calculate metrics for all models
current_metrics <- calculate_metrics(current_fit, data) %>% mutate(model = "NEWS2")
light_metrics <- calculate_metrics(light_fit, data) %>% mutate(model = "NEWS2-Light")
full_metrics <- calculate_metrics(full_fit, data) %>% mutate(model = "IEWS-Light")
xgb_metrics <- calculate_metrics(xgb_fit, data) %>% mutate(model = "XGBoost")

# Combine all metrics
all_metrics <- bind_rows(current_metrics, light_metrics, full_metrics, xgb_metrics)

# Create a tidy format for plotting
tidy_all_metrics <- bind_rows(
  # AUC metrics
  all_metrics %>% select(model, Mean_AUC, Lower_AUC, Upper_AUC, SD_AUC) %>%
    mutate(metric = "AUC", weighting = "Weighted", 
           mean = Mean_AUC, lower_ci = Lower_AUC, upper_ci = Upper_AUC, sd = SD_AUC),
  
  all_metrics %>% select(model, Mean_AUC_Unweighted, Lower_AUC_Unweighted, Upper_AUC_Unweighted, SD_AUC_Unweighted) %>%
    mutate(metric = "AUC", weighting = "Unweighted", 
           mean = Mean_AUC_Unweighted, lower_ci = Lower_AUC_Unweighted, 
           upper_ci = Upper_AUC_Unweighted, sd = SD_AUC_Unweighted),
  
  # Brier score metrics
  all_metrics %>% select(model, Mean_Brier, Lower_Brier, Upper_Brier, SD_Brier) %>%
    mutate(metric = "Brier Score", weighting = "Weighted", 
           mean = Mean_Brier, lower_ci = Lower_Brier, upper_ci = Upper_Brier, sd = SD_Brier),
  
  all_metrics %>% select(model, Mean_Brier_Unweighted, Lower_Brier_Unweighted, Upper_Brier_Unweighted, SD_Brier_Unweighted) %>%
    mutate(metric = "Brier Score", weighting = "Unweighted", 
           mean = Mean_Brier_Unweighted, lower_ci = Lower_Brier_Unweighted, 
           upper_ci = Upper_Brier_Unweighted, sd = SD_Brier_Unweighted)
) %>%
  select(model, metric, weighting, mean, lower_ci, upper_ci, sd)

# Set model factor levels for consistent ordering
tidy_all_metrics$model <- factor(tidy_all_metrics$model, 
                                 levels = c("NEWS2", "NEWS2-Light", "IEWS-Light", "XGBoost"))

# Create a plot with the metrics
ggplot(tidy_all_metrics, aes(x = model, y = mean, color = weighting)) +
  geom_pointinterval(
    aes(ymin = lower_ci, ymax = upper_ci),
    position = position_dodge(width = 0.5),
    size = 1,
    point_size = 3
  ) +
  # Facet by metric with free y scales
  facet_wrap(~ metric, scales = "free_y") +
  labs(
    y = "Value", 
    x = NULL,
    color = "Weighting"
  ) +
  theme_gray(base_size = 12) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold"),
    axis.text.x = element_text(angle = 45, hjust = 1)
  )


# Calculate weighted and unweighted metrics using 100 bootstraps (alternative approach)

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
  set.seed(42 + which(c("NEWS2", "NEWS2-Light", "IEWS-Light", "XGBoost") == model_name))
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

# Combine all metrics
all_metrics_bootstrap <- bind_rows(
  metrics_current,
  metrics_light,
  metrics_full,
  metrics_xgb
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
                                       levels = c("NEWS2", "NEWS2-Light", "IEWS-Light", "XGBoost"))

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
  theme_gray(base_size = 12) +
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

# Combine all hospital metrics
all_hospital_metrics <- bind_rows(
  hospital_metrics_current,
  hospital_metrics_light,
  hospital_metrics_full,
  hospital_metrics_xgb
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
  theme_gray(base_size = 12) +
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
  theme_gray(base_size = 12) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right",
    panel.grid = element_blank()
  )

# Display the enhanced heatmaps
auc_heatmap_with_ci /brier_heatmap_with_ci



########################################################
#### Use posterior distributions for the delta AUCs ####
########################################################

# Create a function to prepare data for tidyposterior analysis
prepare_for_tidyposterior <- function(fit_object, data, metric_name) {
  fit_object %>% 
    collect_predictions() %>%
    arrange(.row) %>%
    mutate(weights = data$weights_new,
           mort30D = if_else(Status30D == "Deceased", 1, 0)) %>%
    group_by(id) %>%
    summarise(
      .estimate = if(metric_name == "roc_auc") {
        MetricsWeighted::AUC(mort30D, .pred_Deceased, w = weights)
      } else {
        MetricsWeighted::mse(mort30D, .pred_Deceased, w = weights)
      }
    )
}

# Prepare data for all models and metrics
auc_current <- prepare_for_tidyposterior(current_fit, data, "roc_auc") %>% mutate(model = "NEWS2")
auc_light <- prepare_for_tidyposterior(light_fit, data, "roc_auc") %>% mutate(model = "NEWS2-Light")
auc_full <- prepare_for_tidyposterior(full_fit, data, "roc_auc") %>% mutate(model = "IEWS-Light")
auc_xgb <- prepare_for_tidyposterior(xgb_fit, data, "roc_auc") %>% mutate(model = "XGBoost")

brier_current <- prepare_for_tidyposterior(current_fit, data, "brier_score") %>% mutate(model = "NEWS2")
brier_light <- prepare_for_tidyposterior(light_fit, data, "brier_score") %>% mutate(model = "NEWS2-Light")
brier_full <- prepare_for_tidyposterior(full_fit, data, "brier_score") %>% mutate(model = "IEWS-Light")
brier_xgb <- prepare_for_tidyposterior(xgb_fit, data, "brier_score") %>% mutate(model = "XGBoost")

# Combine all metrics
auc_metrics <- bind_rows(auc_current, auc_light, auc_full, auc_xgb)
brier_metrics <- bind_rows(brier_current, brier_light, brier_full, brier_xgb)

# Use a simpler approach - directly use the metrics data frame
auc_for_post <- auc_metrics %>%
  pivot_wider(
    id_cols = id,
    names_from = model,
    values_from = .estimate
  )

# Similarly for Brier score
brier_for_post <- brier_metrics %>%
  pivot_wider(
    id_cols = id,
    names_from = model,
    values_from = .estimate
  )

# Fit Bayesian models using the perf_mod function with the metrics directly
set.seed(123)

# For AUC (higher is better)
auc_post <- perf_mod(
  auc_for_post,
  seed = 1234,
  iter = 4000,
  chains = 4,
  cores = 40,  # Reduced to avoid memory issues
  refresh = 0
)

# For Brier score (lower is better)
brier_post <- perf_mod(
  brier_for_post,
  seed = 1234,
  iter = 4000,
  chains = 4,
  cores = 40,  # Reduced to avoid memory issues
  refresh = 0
)

# Check model convergence
summary(auc_post)
summary(brier_post)

# Extract posterior distributions
auc_dens <- tidy(auc_post)
brier_dens <- tidy(brier_post)

# Create density plots of the posterior distributions
distribution_auc <- ggplot(auc_dens, aes(x = posterior, fill = model)) +
  geom_density(alpha = 0.5) +
  labs(
    x = "AUC",
    y = "Density") +
  theme_gray(base_size = 12) +
  theme(legend.position = "top")

distribution_brier <- ggplot(brier_dens, aes(x = posterior, fill = model)) +
  geom_density(alpha = 0.5) +
  labs(
    x = "Brier Score",
    y = "Density") +
  theme_gray(base_size = 12) +
  theme(legend.position = "")

# Merge them now

distribution_auc / distribution_brier


# Calculate pairwise differences
auc_diffs <- contrast_models(auc_post)

brier_diffs <- contrast_models(brier_post)

summary(auc_diffs)
summary(brier_diffs)

diffs_brier <- brier_diffs |> 
  ggplot(aes(x = difference, fill = contrast)) + 
  geom_density(alpha = 0.5) +
  theme(legend.position = "") +
  labs(x = "Brier score difference", y = "") +
  facet_wrap(vars(contrast))

diffs_auc <- auc_diffs |> 
  ggplot(aes(x = difference, fill = contrast)) + 
  geom_density(alpha = 0.5) +
  theme(legend.position = "") +
  labs(x = "AUC difference", y = "") +
  facet_wrap(vars(contrast))

diffs_auc / diffs_brier


# Calibration curves

##############################################
########### Calibration Curves ###############
##############################################

# Get the overall calibration curves

# Function to create weighted calibration curves for a model
create_weighted_calibration <- function(fit_object, data, n_bins = 20, model_name = "Model") {
  # Bin the predicted probabilities
  cals <- fit_object |>
    collect_predictions() |>
    arrange(.row) |>
    mutate(weights = data$weights_new) |>
    mutate(bin = cut(.pred_Deceased, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE)) |>
    mutate(mort30D = if_else(Status30D == "Deceased", 1, 0))
  
  # Calculate weighted fraction of positives for each resample
  resample_calibration <- cals |>
    group_by(id, bin) |>
    summarize(
      mean_pred_prob = mean(.pred_Deceased),
      weighted_frac_pos = sum(weights * mort30D) / sum(weights),
      .groups = 'drop'
    )
  
  # Calculate overall mean and confidence intervals for each bin
  calibration_summary <- resample_calibration |>
    group_by(bin) |>
    summarize(
      mean_pred_prob = mean(mean_pred_prob),
      weighted_frac_pos_mean = mean(weighted_frac_pos),
      lower_ci = quantile(weighted_frac_pos, probs = 0.025, na.rm = TRUE),
      upper_ci = quantile(weighted_frac_pos, probs = 0.975, na.rm = TRUE),
      n_resamples = n(),
      .groups = 'drop'
    ) |>
    mutate(model = model_name)
  
  return(calibration_summary)
}

# Create calibration data for all models
cal_data_current <- create_weighted_calibration(current_fit, data, model_name = "NEWS2")
cal_data_light <- create_weighted_calibration(light_fit, data, model_name = "NEWS2-Light")
cal_data_full <- create_weighted_calibration(full_fit, data, model_name = "IEWS-Light")
cal_data_xgb <- create_weighted_calibration(xgb_fit, data, model_name = "XGBoost")

# Combine all calibration data
all_cal_data <- bind_rows(cal_data_current, cal_data_light, cal_data_full, cal_data_xgb)

# Plot individual calibration curves
plot_calibration <- function(cal_data, title) {
  ggplot(cal_data, aes(x = mean_pred_prob, y = weighted_frac_pos_mean)) +
    geom_point(size = 3) +
    geom_line() + 
    geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.01) + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    coord_equal() +
    scale_x_continuous(limits = c(0, max(cal_data$mean_pred_prob) * 1.1), 
                       breaks = seq(0, 1, 0.05)) +
    scale_y_continuous(limits = c(0, max(cal_data$upper_ci, na.rm = TRUE) * 1.1), 
                       breaks = seq(0, 1, 0.05)) +
    labs(x = "Mean predicted probability", 
         y = "Weighted fraction of mortality cases",
         title = "") +
    theme_gray(base_size = 12)
}

# Create individual plots
cal_plot_current <- plot_calibration(cal_data_current, "NEWS2 Weighted Calibration")
cal_plot_light <- plot_calibration(cal_data_light, "NEWS2-Light Weighted Calibration")
cal_plot_full <- plot_calibration(cal_data_full, "IEWS-Light Weighted Calibration")
cal_plot_xgb <- plot_calibration(cal_data_xgb, "XGBoost Weighted Calibration")

# Combined plot of all models
cal_plot_combined <- ggplot(all_cal_data, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, 
                                              color = model, shape = model)) +
  geom_point(size = 3) +
  geom_line() + 
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
  scale_x_continuous(limits = c(0, max(all_cal_data$mean_pred_prob) * 1.1), 
                     breaks = seq(0, 1, 0.05)) +
  scale_y_continuous(limits = c(0, max(all_cal_data$upper_ci, na.rm = TRUE) * 1.1), 
                     breaks = seq(0, 1, 0.05)) +
  labs(x = "Mean predicted probability", 
       y = "Weighted fraction of mortality cases",
       color = "Model",
       shape = "Model",
       title = "") +
  theme_gray(base_size = 12) +
  theme(legend.position = "top")

# Display the plots
cal_plot_current
cal_plot_light
cal_plot_full
cal_plot_xgb
cal_plot_combined


################################################################################
########################## Calibration Curves by subgroups ####################
################################################################################

# Function to create stratified calibration curves
create_stratified_calibration <- function(fit_object, data, strat_var, n_bins = 20, model_name = "Model") {
  # Get the stratification variable and shorten specific SKS category
  strat_values <- if_else(
    data[[strat_var]] == "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
    "Diseases of the blood and certain disorders involving the immune mechanism",
    data[[strat_var]]
  )
  
  # Bin the predicted probabilities
  cals <- fit_object |>
    collect_predictions() |>
    arrange(.row) |>
    mutate(
      weights = data$weights_new,
      strat_group = strat_values,
      bin = cut(.pred_Deceased, breaks = seq(0, 1, length.out = n_bins + 1), include.lowest = TRUE),
      mort30D = if_else(Status30D == "Deceased", 1, 0)
    )
  
  # Calculate weighted fraction of positives for each resample and stratification group
  resample_calibration <- cals |>
    group_by(id, strat_group, bin) |>
    summarize(
      mean_pred_prob = mean(.pred_Deceased),
      weighted_frac_pos = sum(weights * mort30D) / sum(weights),
      .groups = 'drop'
    )
  
  # Calculate overall mean and confidence intervals for each bin and stratification group
  calibration_summary <- resample_calibration |>
    group_by(strat_group, bin) |>
    summarize(
      mean_pred_prob = mean(mean_pred_prob),
      weighted_frac_pos_mean = mean(weighted_frac_pos, na.rm = TRUE),
      lower_ci = quantile(weighted_frac_pos, probs = 0.025, na.rm = TRUE),
      upper_ci = quantile(weighted_frac_pos, probs = 0.975, na.rm = TRUE),
      n_resamples = n(),
      .groups = 'drop'
    ) |>
    mutate(model = model_name)
  
  return(calibration_summary)
}


create_all_stratified_calibrations <- function(data, strat_var, n_bins = 20) {
  # Create stratified calibration data for each model
  cal_data_current <- create_stratified_calibration(current_fit, data, strat_var, n_bins, "NEWS2")
  cal_data_light <- create_stratified_calibration(light_fit, data, strat_var, n_bins, "NEWS2-Light")
  cal_data_full <- create_stratified_calibration(full_fit, data, strat_var, n_bins, "IEWS-Light")
  cal_data_xgb <- create_stratified_calibration(xgb_fit, data, strat_var, n_bins, "XGBoost")
  
  # Create individual plots
  plot_strat_cal <- function(cal_data, title) {
    ggplot(cal_data, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, color = strat_group)) +
      geom_point(size = 3) +
      geom_line() + 
      geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
      facet_wrap(~ strat_group) +
      labs(x = "Mean predicted probability", 
           y = "Weighted fraction of mortality cases",
           color = strat_var) +
      theme_gray(base_size = 12) +
      theme(legend.position = "none")
  }
  
  # Create plots for each model
  cal_plot_current <- plot_strat_cal(cal_data_current, paste("NEWS2 Weighted Calibration by", strat_var))
  cal_plot_light <- plot_strat_cal(cal_data_light, paste("NEWS2-Light Weighted Calibration by", strat_var))
  cal_plot_full <- plot_strat_cal(cal_data_full, paste("IEWS-Light Weighted Calibration by", strat_var))
  cal_plot_xgb <- plot_strat_cal(cal_data_xgb, paste("XGBoost Weighted Calibration by", strat_var))
  
  # Return list of plots
  return(list(
    NEWS2 = cal_plot_current,
    NEWS2_Light = cal_plot_light,
    IEWS_Light = cal_plot_full,
    XGBoost = cal_plot_xgb
  ))
}


# Function to create model comparison plots for each stratification group
create_model_comparison_by_strat <- function(data, strat_var, n_bins = 20) {
  # Create stratified calibration data for each model
  cal_data_current <- create_stratified_calibration(current_fit, data, strat_var, n_bins, "NEWS2")
  cal_data_light <- create_stratified_calibration(light_fit, data, strat_var, n_bins, "NEWS2-Light")
  cal_data_full <- create_stratified_calibration(full_fit, data, strat_var, n_bins, "IEWS-Light")
  cal_data_xgb <- create_stratified_calibration(xgb_fit, data, strat_var, n_bins, "XGBoost")
  
  # Combine all calibration data
  all_cal_data <- bind_rows(cal_data_current, cal_data_light, cal_data_full, cal_data_xgb)
  
  # Create comparison plot
  ggplot(all_cal_data, aes(x = mean_pred_prob, y = weighted_frac_pos_mean, 
                           color = model)) +
    geom_point(size = 3) +
    geom_line() + 
    geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey") +
    facet_wrap(~ strat_group) +
    labs(x = "Mean predicted probability", 
         y = "Weighted fraction of mortality cases",
         color = "Model",
         shape = "Model") +
    theme_gray(base_size = 12) +
    theme(legend.position = "top") 
}

# Create model comparison plots for each stratification variable
age_model_comparison <- create_model_comparison_by_strat(data, "Age_Group")
sex_model_comparison <- create_model_comparison_by_strat(data, "Sex")
hospital_model_comparison <- create_model_comparison_by_strat(data, "Hospital")
sks_model_comparison <- create_model_comparison_by_strat(data, "SKS_Category")

# Display the model comparison plots
age_model_comparison
sex_model_comparison
hospital_model_comparison
sks_model_comparison




################################################################################
########################## Decision curve analysis #############################
################################################################################

##############################################
########### Weighted Decision Curves #########
##############################################

# Function to create weighted decision curves
create_weighted_dca <- function(fit_object, data, thresholds = seq(0, 0.4, by = 0.01), 
                                label = "Model", subset_data = NULL) {
  
  # Get predictions
  preds <- fit_object |> 
    collect_predictions() |>
    arrange(.row) |>
    mutate(mort30D = if_else(Status30D == "Deceased", 1, 0),
           weights = data$weights_new)
  
  # Apply subset if provided
  if (!is.null(subset_data)) {
    subset_indices <- which(subset_data)
    preds <- preds[subset_indices, ]
  }
  
  # Calculate net benefit for each threshold
  nb_results <- data.frame(threshold = thresholds)
  
  # Calculate weighted net benefit for the model
  nb_model <- sapply(thresholds, function(t) {
    # Classify patients based on threshold
    tp <- preds$mort30D == 1 & preds$.pred_Deceased >= t
    fp <- preds$mort30D == 0 & preds$.pred_Deceased >= t
    
    # Calculate weighted true positives and false positives
    wtp <- sum(preds$weights[tp])
    wfp <- sum(preds$weights[fp])
    
    # Total weighted population
    total_w <- sum(preds$weights)
    
    # Net benefit formula
    nb <- (wtp/total_w) - (wfp/total_w) * (t/(1-t))
    return(nb)
  })
  
  # Calculate weighted net benefit for treat all
  nb_all <- sapply(thresholds, function(t) {
    # All patients with the outcome
    tp <- preds$mort30D == 1
    fp <- preds$mort30D == 0
    
    # Calculate weighted true positives and false positives
    wtp <- sum(preds$weights[tp])
    wfp <- sum(preds$weights[fp])
    
    # Total weighted population
    total_w <- sum(preds$weights)
    
    # Net benefit formula for treat all
    nb <- (wtp/total_w) - (wfp/total_w) * (t/(1-t))
    return(nb)
  })
  
  # Calculate net benefit for treat none (always 0)
  nb_none <- rep(0, length(thresholds))
  
  # Combine results
  nb_results$nb_model <- nb_model
  nb_results$nb_all <- nb_all
  nb_results$nb_none <- nb_none
  nb_results$model <- label
  
  return(nb_results)
}

# Create weighted DCA for all models
dca_current <- create_weighted_dca(current_fit, data, thresholds = seq(0, 0.4, by = 0.01), label = "NEWS2")
dca_light <- create_weighted_dca(light_fit, data, thresholds = seq(0, 0.4, by = 0.01), label = "NEWS2-Light")
dca_full <- create_weighted_dca(full_fit, data, thresholds = seq(0, 0.4, by = 0.01), label = "IEWS-Light")
dca_xgb <- create_weighted_dca(xgb_fit, data, thresholds = seq(0, 0.4, by = 0.01), label = "XGBoost")

# Combine all DCA results
all_dca <- bind_rows(dca_current, dca_light, dca_full, dca_xgb)

# Plot the decision curves
plot_dca <- function(dca_data) {
  # Reshape data for plotting
  dca_long <- dca_data %>%
    pivot_longer(
      cols = c(nb_model, nb_all, nb_none),
      names_to = "strategy",
      values_to = "net_benefit"
    ) %>%
    mutate(strategy_label = case_when(
      strategy == "nb_model" ~ model,
      strategy == "nb_all" ~ "Treat All",
      strategy == "nb_none" ~ "Treat None"
    ))
  
  # Create plot
  ggplot(dca_long, aes(x = threshold, y = net_benefit, color = strategy_label, linetype = strategy_label)) +
    geom_line(size = 1) +
    scale_color_manual(values = c("Treat All" = "darkgray", "Treat None" = "black", 
                                  "NEWS2" = "#E69F00", "NEWS2-Light" = "#56B4E9", 
                                  "IEWS-Light" = "#009E73", "XGBoost" = "#CC79A7")) +
    scale_linetype_manual(values = c("Treat All" = "dashed", "Treat None" = "dotted",
                                     "NEWS2" = "solid", "NEWS2-Light" = "solid", 
                                     "IEWS-Light" = "solid", "XGBoost" = "solid")) +
    labs(
      x = "Threshold Probability",
      y = "Net Benefit",
      color = "Strategy",
      linetype = "Strategy"
    ) +
    theme_gray(base_size = 12) +
    theme(legend.position = "top") +
    coord_cartesian(ylim = c(-0.01, max(dca_long$net_benefit) * 1.05))
}

# Plot overall weighted DCA
overall_dca_plot <- plot_dca(all_dca)
overall_dca_plot

# Add vertical lines for risk groups

# Calculate risk group thresholds from your data
current_preds <- current_fit %>%
  collect_predictions() %>%
  arrange(.row) %>%
  mutate(Risk_Groups_EWS = data$Risk_Groups_EWS)  # Add risk groups from original data

# Calculate mean predicted risk for each risk group
risk_thresholds <- current_preds %>%
  group_by(Risk_Groups_EWS) %>%
  summarize(mean_risk = mean(.pred_Deceased)) %>%
  arrange(mean_risk)

# Extract threshold values
low_threshold <- risk_thresholds$mean_risk[risk_thresholds$Risk_Groups_EWS == "Low"]
low_med_threshold <- risk_thresholds$mean_risk[risk_thresholds$Risk_Groups_EWS == "Low-Medium"]
med_threshold <- risk_thresholds$mean_risk[risk_thresholds$Risk_Groups_EWS == "Medium"]
high_threshold <- risk_thresholds$mean_risk[risk_thresholds$Risk_Groups_EWS == "High"]


# Now use these values in your plot
overall_dca_plot_with_groups <- overall_dca_plot +
  geom_vline(xintercept = low_threshold, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = low_med_threshold, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = med_threshold, linetype = 2, alpha = 0.5) +
  geom_vline(xintercept = high_threshold, linetype = 2, alpha = 0.5) +
  annotate("text", x = low_threshold, y = max(all_dca$nb_model) * 0.9, 
           label = "Low", angle = 90, hjust = 1.2, vjust = 1.28,size = 3) +
  annotate("text", x = low_med_threshold, y = max(all_dca$nb_model) * 0.9, 
           label = "Low-Medium", angle = 90, hjust = 1.2,vjust = 1.28, size = 3) +
  annotate("text", x = med_threshold, y = max(all_dca$nb_model) * 0.9, 
           label = "Medium", angle = 90, hjust = 1.2,vjust = 1.28, size = 3) +
  annotate("text", x = high_threshold, y = max(all_dca$nb_model) * 0.9, 
           label = "High", angle = 90,vjust = 1.2, hjust = 1.28, size = 3)

overall_dca_plot_with_groups


# Create weighted DCA for subgroups

create_subgroup_dca <- function(data, subgroup_var, subgroup_value) {
  subset_condition <- data[[subgroup_var]] == subgroup_value
  
  dca_current <- create_weighted_dca(current_fit, data, 
                                     thresholds = seq(0, 0.2, by = 0.01), 
                                     label = "NEWS2", 
                                     subset_data = subset_condition)
  
  dca_light <- create_weighted_dca(light_fit, data, 
                                   thresholds = seq(0, 0.2, by = 0.01), 
                                   label = "NEWS2-Light", 
                                   subset_data = subset_condition)
  
  dca_full <- create_weighted_dca(full_fit, data, 
                                  thresholds = seq(0, 0.2, by = 0.01), 
                                  label = "IEWS-Light", 
                                  subset_data = subset_condition)
  
  dca_xgb <- create_weighted_dca(xgb_fit, data, 
                                 thresholds = seq(0, 0.2, by = 0.01), 
                                 label = "XGBoost", 
                                 subset_data = subset_condition)
  
  all_dca <- bind_rows(dca_current, dca_light, dca_full, dca_xgb)
  
  # Call plot_dca without any title argument
  plot_dca(all_dca)
}

# Create DCA for age groups
age_18_65_dca <- create_subgroup_dca(data, "Age_Group", "18-65")
age_66_80_dca <- create_subgroup_dca(data, "Age_Group", "66-80")
age_80plus_dca <- create_subgroup_dca(data, "Age_Group", "80+")



# Create DCA by sex
male_dca <- create_subgroup_dca(data, "Sex", "Male")
female_dca <- create_subgroup_dca(data, "Sex", "Female")

# Create grid layouts for related DCA plots

# Age group DCA grid
age_dca_grid <- (age_18_65_dca + age_66_80_dca + age_80plus_dca) +
  theme(legend.position = "top")

# Sex DCA grid
sex_dca_grid <- male_dca + female_dca +
  theme(legend.position = "top")


# Display the DCA grids
age_dca_grid
sex_dca_grid


# Compare the Net Benefit of various models

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

# Combine all net benefit results
all_nb <- bind_rows(nb_current, nb_light, nb_full, nb_xgb)

# Create a boxplot to visualize the distribution of net benefit across folds
ggplot(all_nb, aes(x = model, y = net_benefit, fill = model)) +
  geom_boxplot() +
  facet_wrap(~ threshold, labeller = labeller(threshold = function(x) paste0("Threshold = ", x))) +
  labs(x = "",
       y = "Net Benefit") +
  theme_gray(base_size = 12) +
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

# Create an empty dataframe to store the results
news2_vs_xgboost_results <- data.frame()

# Loop through each threshold in nb_diffs_results
for (threshold in names(nb_diffs_results)) {
  # Get the contrast results for this threshold
  diff_result <- nb_diffs_results[[threshold]]
  
  # Convert to tidy format
  tidy_result <- summary(diff_result)
  
  # Filter for "NEWS2 vs. XGBoost" contrast
  news2_vs_xgb <- tidy_result %>% 
    filter(contrast == "NEWS2 vs XGBoost")
  
  # Add threshold column
  news2_vs_xgb <- news2_vs_xgb %>%
    mutate(threshold = as.numeric(threshold))
  
  # Append to results dataframe
  news2_vs_xgboost_results <- bind_rows(news2_vs_xgboost_results, news2_vs_xgb)
}

# Sort by threshold for better readability
news2_vs_xgboost_results <- news2_vs_xgboost_results %>%
  arrange(threshold)

# Print the results
print(news2_vs_xgboost_results)


# Plot the results

# Create an empty dataframe to store the posterior samples
posterior_samples <- data.frame()

# Loop through each threshold in nb_diffs_results
for (threshold in names(nb_diffs_results)) {
  # Get the contrast results for this threshold
  diff_result <- nb_diffs_results[[threshold]]
  
  # Extract the difference column for NEWS2 vs NEWS2-Light
  diff_data <- as.data.frame(diff_result) %>%
    filter(contrast == "NEWS2 vs. XGBoost") %>%
    select(difference) %>%
    mutate(threshold = as.numeric(threshold))
  
  # Append to results dataframe
  posterior_samples <- bind_rows(posterior_samples, diff_data)
}

# Create a density plot of the posterior distributions by threshold
ggplot(posterior_samples, aes(x = difference, y = factor(threshold), fill = factor(threshold))) +
  # Add density ridges
  geom_density_ridges(alpha = 0.7, scale = 0.9, rel_min_height = 0.01) +
  # Add a vertical line at x=0
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  # Add labels
  labs(
    x = "Difference in Net Benefit (NEWS2 vs XGBoost)",
    y = "Decision Threshold",
    fill = "Threshold"
  ) +
  # Customize theme
  theme_gray(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.major.x = element_line(color = "gray90"),
    panel.grid.minor = element_blank()
  )