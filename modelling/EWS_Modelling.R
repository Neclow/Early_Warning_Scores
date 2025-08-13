# ================================
# LANCET STYLE GGPLOT SYSTEM
# ================================
# Source this file to apply Lancet formatting to all your ggplot figures

library(ggplot2)
library(stringr)
library(scales)

# --------------------------------
# 1. NUMBER FORMATTING FUNCTIONS
# --------------------------------

#' Format numbers with Lancet conventions (mid point for decimals)
#' @param x numeric vector
#' @param digits number of decimal places
#' @param big_mark separator for thousands (default: space)
#' @return formatted character vector
format_lancet <- function(x, digits = 0, big_mark = " ") {
  format(
    round(x, digits),
    big.mark = big_mark,
    decimal.mark = "·",
    scientific = FALSE,
    trim = TRUE
  )
}

#' Convert any text with decimal points to mid points
#' @param text character vector
#' @return character vector with mid points
convert_to_midpoints <- function(text) {
  str_replace_all(text, "\\.", "·")
}

# --------------------------------
# 2. LANCET THEME FUNCTION
# --------------------------------

#' Lancet journal style theme
#' @param base_size base font size
#' @param base_family base font family
#' @return ggplot theme
theme_lancet <- function(base_size = 14, base_family = "") {
  theme_minimal(base_size = base_size, base_family = base_family) +
    theme(
      # Plot titles and subtitles
      plot.title = element_text(
        size = base_size + 2,
        face = "bold",
        margin = margin(b = 10)
      ),
      plot.subtitle = element_text(
        size = base_size - 1,
        color = "gray40",
        margin = margin(b = 15)
      ),

      # Axis text and titles
      axis.text.y = element_text(size = base_size - 2, color = "gray20"),
      axis.text.x = element_text(size = base_size - 3, color = "gray20"),
      axis.title.x = element_text(
        size = base_size - 2,
        color = "gray20",
        margin = margin(t = 10)
      ),
      axis.title.y = element_text(
        size = base_size - 2,
        color = "gray20",
        margin = margin(r = 10)
      ),

      # Grid lines - minimal and subtle
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),

      # Legend
      legend.text = element_text(size = base_size - 3),
      legend.title = element_text(size = base_size - 2, face = "bold"),
      legend.margin = margin(5, 5, 5, 5),

      # Plot margins
      plot.margin = margin(15, 15, 15, 15),

      # Strip text for facets
      strip.text = element_text(size = base_size - 2, face = "bold"),
      strip.background = element_rect(fill = "gray95", color = NA)
    )
}

# --------------------------------
# 3. LANCET COLOR PALETTES
# --------------------------------

# Professional color palette suitable for medical journals
lancet_colors <- c(
  "#2E86AB", # Blue
  "#A23B72", # Purple/Magenta
  "#F18F01", # Orange
  "#C73E1D", # Red
  "#5D737E", # Gray-blue
  "#64A6BD", # Light blue
  "#F4B942", # Yellow
  "#8B5A2B" # Brown
)

# Color palette function
scale_fill_lancet <- function(...) {
  scale_fill_manual(values = lancet_colors, ...)
}

scale_color_lancet <- function(...) {
  scale_color_manual(values = lancet_colors, ...)
}

# --------------------------------
# 4. AXIS FORMATTING FUNCTIONS
# --------------------------------

#' Lancet-style continuous scale with mid points
scale_y_lancet <- function(digits = 1, ...) {
  scale_y_continuous(
    labels = function(x) format_lancet(x, digits = digits),
    ...
  )
}

scale_x_lancet <- function(digits = 1, ...) {
  scale_x_continuous(
    labels = function(x) format_lancet(x, digits = digits),
    ...
  )
}


# Working directory for the rest should be found in the S-drive
setwd("/home/alex/ews/NEWS2_Evaluation/Modelling")

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
library(ggdist) # For further plotting
library(ggridges) # For plotting
library(scales) # GGplot modification
library(WeightIt) # Weighting methods
library(cobalt) # Balance checking
library(gtsummary) # For summary tables
library(survey) # For weights
library(gt) # For summary tables
library(gtsummary) # For tables
library(survey) # For weights
library(janitor) # For quick tabyls
library(pmsampsize) # For sample size requirements

###############################################################
################ Start of the pre-processing ##################
###############################################################

data <- read_parquet(
  "/home/alex/ews/NEWS2_Evaluation/Additional_Data/df_with_embeddings.parquet"
)

levels(data$Hospital)

data$Status24H <- relevel(data$Status24H, "Deceased")

data <- data |>
  relocate(Hospital, .after = CSN) |>
  relocate(Department_Name, .after = Hospital)

#############################################

data |> count(Department_Name, sort = T)

levels(data$Department_Name)


######## Create a more refined mapping of specialties to broader categories ###########
specialty_categories <- list(
  "Surgical" = c(
    "Brystkirurgi",
    "Gynækologi",
    "Gynækologi-obstetrik",
    "Karkirurgi",
    "Kirurgi",
    "Neurokirurgi",
    "Ortopædkirurgi",
    "Plastikkirurgi",
    "Tand- mund- og kæbekirurgi",
    "Thoraxkirurgi",
    "Urologi",
    "Øre-næse-hals"
  ),

  "Cardiopulmonary" = c("Kardiologi", "Lungesygdomme"),

  "Gastroenterology" = c("Medicinsk gastroenterologi", "Hepatologi"),

  "Neurology" = c("Neurologi"),

  "Oncology & Hematology" = c(
    "Onkologi",
    "Hæmatologi",
    "Hæmatologi-onkologi pædiatrisk"
  ),

  "Endocrinology & Metabolism" = c("Endokrinologi"),

  "Nephrology" = c("Nefrologi"),

  "Infectious Disease" = c("Infektionsmedicin"),

  "Dermatology" = c("Dermatologi"),

  "Rheumatology" = c("Reumatologi"),

  "Geriatrics & Palliative" = c("Geriatri", "Palliation"),

  "Pediatrics" = c("Pædiatri"),

  "Internal & Acute Medicine" = c("Intern medicin", "Akutmedicin"),

  "Critical Care" = c("Anæstesiologi", "Intensiv terapi"),

  "Psychiatry" = c("Psykiatri"),

  "Rehabilitation" = c("Rehabilitering"),

  "Ophthalmology" = c("Oftalmologi"),

  "Other" = c("Ikke-klinisk speciale", "NULL")
)

# Function to categorize a specialty
get_category <- function(specialty) {
  for (category in names(specialty_categories)) {
    if (specialty %in% specialty_categories[[category]]) {
      return(category)
    }
  }
  return("Uncategorized")
}

# Apply the categorization to your dataframe
data <- data %>%
  mutate(Specialty_Category = sapply(Department_Name, get_category))

# Convert to factor with a logical order
data <- data %>%
  mutate(
    Specialty_Category = factor(
      Specialty_Category,
      levels = c(
        "Internal & Acute Medicine",
        "Cardiopulmonary",
        "Gastroenterology",
        "Neurology",
        "Oncology & Hematology",
        "Endocrinology & Metabolism",
        "Nephrology",
        "Infectious Disease",
        "Dermatology",
        "Rheumatology",
        "Geriatrics & Palliative",
        "Pediatrics",
        "Critical Care",
        "Surgical",
        "Psychiatry",
        "Rehabilitation",
        "Ophthalmology",
        "Other"
      )
    )
  )

data <- data |>
  mutate(
    Department_Name_Fac = if_else(
      is.na(Specialty_Category),
      "Other",
      Specialty_Category
    )
  ) |>
  mutate_at(vars(Department_Name_Fac), as.factor) |>
  select(-Specialty_Category)


# Lumping the 10 most frequent departments

data <- data %>%
  mutate(Department_Name_Fac = fct_lump_n(Department_Name_Fac, n = 9))

# Check the result
data %>% count(Department_Name_Fac, sort = TRUE)

# Defining the propensity score model

# Grab all the pca columns
pca_columns <- grep("^pc", names(data), value = TRUE)

my_formula_pca <- reformulate(termlabels = pca_columns)

# Grab all the blood tests
median_columns <- grep("^median", names(data), value = TRUE)

my_formula_med <- reformulate(termlabels = median_columns)

# Create a composite variable (average of all PCA columns)
data <- data |>
  mutate(pca_composite = rowMeans(across(all_of(pca_columns)), na.rm = TRUE))

# Factorize variable regarding previous icu and respiratory
data <- data |>
  mutate(
    previous_icu_respiratory = if_else(
      previous_icu_respiratory == 0,
      "No history of ICU or respiratory support",
      "Previous history of ICU or respiratory support"
    )
  ) |>
  mutate_at(vars(previous_icu_respiratory), as.factor)

data <- data |>
  mutate_at(
    vars(Sex, Age_Group, Hospital, Oxygen_Supplement, Consciousness),
    as.factor
  )

# Use propensity score modelling for the probability of interventions at 24 hours

weight_model <- weightit(
  Interventions_24 ~
    Age_Group +
      Sex +
      Hospital +
      Blood_Pressure.Sys +
      Temperature +
      Saturation +
      Pulse +
      Previous_Hosp +
      previous_icu_respiratory +
      Oxygen_Supplement +
      Consciousness +
      pca_composite,
  data = data,
  method = "cbps",
  estimand = "ATE"
)

summary(weight_model)

# Store the weights

w_raw <- weight_model$weights

w_df <- as.data.frame(w_raw)

colnames(w_df) <- c("weights_new")

# Save the weights in a dataframe

write_parquet(w_df, "weights_cbps_24.parquet")

# Attach to the data
data <- data %>%
  mutate(weights_new = w_raw)

# Absolute standardized mean differences

plot_ipw <- data |>
  tidy_smd(
    c(
      Age_Group,
      Sex,
      Hospital,
      Blood_Pressure.Sys,
      Temperature,
      Saturation,
      Pulse,
      Oxygen_Supplement,
      Consciousness,
      pca_composite,
      Previous_Hosp,
      previous_icu_respiratory
    ),
    .group = Interventions_24,
    .wts = weights_new
  )

# Love plot of absolute standardized mean differences

plot_love <- love_plot(plot_ipw) +
  theme(legend.position = "top") +
  theme_lancet() +
  labs(
    x = "Absolute standardized mean differences",
    y = "",
    fill = "",
    color = ""
  ) +
  scale_fill_lancet(labels = c("Unweighted", "Weighted")) +
  scale_color_lancet(labels = c("Unweighted", "Weighted")) +
  scale_x_lancet() +
  scale_y_discrete(
    labels = c(
      "Age group",
      "Systolic blood pressure",
      "Consciousness",
      "Hospital",
      "Oxygen Supplement",
      "Embedding score",
      "Number of previous hospitalizations",
      "History of previous ICU or respiratory support",
      "Pulse",
      "Saturation",
      "Sex",
      "Temperature"
    )
  )


plot_love

# Check the distributions of the propensity scores amongs groups

bal_plot <- bal.plot(
  weight_model,
  var.name = "prop.score",
  which = "both",
  type = "histogram",
  mirror = TRUE
) +
  theme_lancet() +
  labs(x = "Propensity Score", y = "Proportion", title = "") +
  scale_color_lancet(
    name = "Received major intervention",
    labels = c("No", "Yes")
  ) +
  scale_fill_lancet(
    name = "Received major intervention",
    labels = c("No", "Yes")
  ) +
  scale_x_lancet() +
  scale_y_lancet()
theme(legend.position = "top")

bal_plot

#########################################################################
###################### Modelling ########################################
#########################################################################

data$imp_weights <- importance_weights(data$weights_new)

# Keep only the weighted individuals without interventions

data <- data |> filter(Interventions_24 == 0)

data <- data |>
  mutate(
    day_type = if_else(wday(recorded_time) %in% 2:6, "Weekday", "Weekend")
  ) |>
  mutate(hour = hour(recorded_time)) |>
  mutate(
    time_of_day = case_when(
      hour >= 6 & hour < 12 ~ "Morning",
      hour >= 12 & hour < 22 ~ "Afternoon/Evening",
      TRUE ~ "Night"
    )
  ) |>
  mutate(month = month(recorded_time, label = TRUE, abbr = TRUE)) |>
  select(-hour) |>
  mutate_at(vars(day_type, time_of_day), as.factor)

# Check the unique individuals in the data

length(unique(data$PT_ID)) # 829,610 individuals

# Check number of unique hospital encounters

length(unique(data$Identifier)) # 2,161,689 unique encounters

#############################################################
############## Create table 1 with weights ##################
#############################################################

data1 <- data |> select(-imp_weights)

svy_design <- svydesign(ids = ~1, weights = ~weights_new, data = data1)

# Create a weighted table with gtsummary
# First, set the theme to The Lancet's style
gtsummary::set_gtsummary_theme(gtsummary::theme_gtsummary_journal("lancet"))

# Create the weighted table.
# Define the text for the title and subtitle for clarity
table_title <- "Table 1: Baseline characteristics of untreated weighted patients by 24-Hour mortality status"

# Create, modify, and redact the gtsummary table
tbl_summary_weighted <-
  tbl_svysummary(
    svy_design,
    by = Status24H,
    include = c(
      "Age_Group",
      "Sex",
      "Previous_Hosp",
      "Hospital",
      "Department_Name_Fac",
      "Consciousness",
      "Oxygen_Supplement",
      "Respiration_Rate",
      "Pulse",
      "Temperature",
      "Saturation",
      "Blood_Pressure.Sys",
      "Blood_Pressure.Dia",
      "previous_icu_respiratory",
      "EWS_score",
      "median_Hemoglobin",
      "median_Leukocytter",
      "median_Trombocytter",
      "median_Kreatinin",
      "median_ALAT",
      "median_LDH",
      "median_Albumin",
      "median_CRP",
      "median_Laktat_ab",
      "median_Troponin_T",
      "median_Laktat_vb"
    ),
    missing = "no",
    label = list(
      Age_Group = "Age group",
      Department_Name_Fac = "Clinical department",
      Respiration_Rate = "Respiratory rate (breaths/min)",
      Pulse = "Pulse (beats/min)",
      Temperature = "Temperature (°C)",
      Saturation = "Oxygen saturation (%)",
      Oxygen_Supplement = "Supplemental oxygen",
      Blood_Pressure.Dia = "Diastolic blood pressure (mm Hg)",
      Previous_Hosp_Fac = "Number of previous hospitalizations",
      Blood_Pressure.Sys = "Systolic blood pressure (mm Hg)",
      previous_icu_respiratory = "Previous ICU/respiratory support",
      EWS_score = "NEWS2 score",
      # Biomarker labels with Danish units
      median_Hemoglobin = "Hemoglobin (g/L)",
      median_Leukocytter = "Leukocytes (×10⁹/L)",
      median_Trombocytter = "Platelets (×10⁹/L)",
      median_Kreatinin = "Creatinine (μmol/L)",
      median_ALAT = "Alanine aminotransferase (U/L)",
      median_LDH = "Lactate dehydrogenase (U/L)",
      median_Albumin = "Albumin (g/L)",
      median_CRP = "C-reactive protein (mg/L)",
      median_Laktat_ab = "Lactate, arterial blood (mmol/L)",
      median_Troponin_T = "Troponin T (ng/L)",
      median_Laktat_vb = "Lactate, venous blood (mmol/L)"
    )
  ) |>
  add_overall() |>
  modify_header(all_stat_cols() ~ "**{level}**<br>N = {n}") |>
  as_gt() |>
  tab_header(
    title = md(table_title)
  )

# Print the table
tbl_summary_weighted

# Reset the theme to the default after creating your table
gtsummary::reset_gtsummary_theme()

# And save the table

gtsave(
  data = tbl_summary_weighted,
  filename = "Weighted_Summary_Table.docx"
)

#############################################
################### Modelling ###############
#############################################

set.seed(234)

data_folds <- rsample::group_vfold_cv(data, group = Hospital)

# Now we will create our model

model <- logistic_reg(engine = "glm", mode = "classification")

# Let's also create an xgboost model

xgb <- boost_tree(trees = 100, mtry = 30) |>
  set_engine("xgboost") |>
  set_mode("classification")

############## Workflows now ###############

# Current model

current_wf <- workflow() |>
  add_formula(Status24H ~ EWS_score) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# NEWS2-Light

light_wf <- workflow() |>
  add_formula(Status24H ~ EWS_light) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# IEWS

full_wf <- workflow() |>
  add_formula(Status24H ~ IEWS_Light) |>
  add_model(model) |>
  add_case_weights(imp_weights)

# XGBoost

xgb_wf <- workflow() |>
  add_formula(
    Status24H ~
      Age +
        Sex +
        Respiration_Rate +
        Temperature +
        Saturation +
        Oxygen_Supplement +
        Blood_Pressure.Sys +
        Blood_Pressure.Dia +
        Consciousness +
        Previous_Hosp +
        day_type +
        time_of_day +
        month +
        previous_icu_respiratory +
        median_Hemoglobin +
        median_Leukocytter +
        median_Trombocytter +
        median_Kreatinin +
        median_ALAT +
        median_LDH +
        median_Albumin +
        median_CRP +
        median_Laktat_ab +
        median_Troponin_T +
        median_Laktat_vb +
        pca_0 +
        pca_1 +
        pca_2 +
        pca_3 +
        pca_4 +
        pca_5 +
        pca_6 +
        pca_7 +
        pca_8 +
        pca_9 +
        pca_10 +
        pca_11 +
        pca_12 +
        pca_13 +
        pca_14 +
        pca_15 +
        pca_16 +
        pca_17 +
        pca_18 +
        pca_19 +
        pca_20 +
        pca_21 +
        pca_22 +
        pca_23 +
        pca_24 +
        pca_25 +
        pca_26 +
        pca_27 +
        pca_28 +
        pca_29 +
        pca_30 +
        pca_31 +
        pca_32 +
        pca_33 +
        pca_34 +
        pca_35 +
        pca_36 +
        pca_37 +
        pca_38 +
        pca_39 +
        pca_40 +
        pca_41 +
        pca_42 +
        pca_43 +
        pca_44 +
        pca_45 +
        pca_46 +
        pca_47 +
        pca_48 +
        pca_49 +
        pca_50 +
        pca_51 +
        pca_52 +
        pca_53 +
        pca_54 +
        pca_55 +
        pca_56 +
        pca_57 +
        pca_58 +
        pca_59
  ) |>
  add_model(xgb) |>
  add_case_weights(imp_weights)

xgb_wf


# Set up parallel processing
doParallel::registerDoParallel(cores = 40)

cntrl <- control_resamples(save_pred = T)


# Grouped cross validations using hospitals as folds

current_fit <- fit_resamples(
  current_wf,
  resamples = data_folds,
  metrics = metric_set(
    roc_auc,
    brier_class
  ),
  control = cntrl
)

light_fit <- fit_resamples(
  light_wf,
  resamples = data_folds,
  metrics = metric_set(
    roc_auc,
    brier_class
  ),
  control = cntrl
)


full_fit <- fit_resamples(
  full_wf,
  resamples = data_folds,
  metrics = metric_set(
    roc_auc,
    brier_class
  ),
  control = cntrl
)

xgb_fit <- fit_resamples(
  xgb_wf,
  resamples = data_folds,
  metrics = metric_set(
    roc_auc,
    brier_class
  ),
  control = cntrl
)

# Generate bootstrapped predictions

calculate_overall_bootstrap_ci <- function(
  fit_object,
  original_data,
  model_name,
  n_bootstrap = 200
) {
  preds_df <- fit_object %>%
    collect_predictions() %>%
    arrange(.row) %>%
    mutate(
      Status24H = original_data$Status24H,
      weights_new = original_data$weights_new,
      mort24H = if_else(Status24H == "Deceased", 1, 0)
    )

  # Perform Bootstrapping
  set.seed(42)

  boot_results <- map_dfr(
    1:n_bootstrap,
    ~ {
      boot_indices <- sample(1:nrow(preds_df), nrow(preds_df), replace = TRUE)
      boot_sample <- preds_df[boot_indices, ]

      auc_val <- MetricsWeighted::AUC(
        actual = boot_sample$mort24H,
        predicted = boot_sample$.pred_Deceased,
        w = boot_sample$weights_new
      )
      brier_val <- MetricsWeighted::mse(
        actual = boot_sample$mort24H,
        predicted = boot_sample$.pred_Deceased,
        w = boot_sample$weights_new
      )

      # Return a one-row tibble for this iteration.
      tibble(
        bootstrap_id = .x,
        auc = auc_val,
        brier = brier_val
      )
    }
  )

  # Summarize Bootstrap Results
  summary_metrics <- boot_results %>%
    # Pivot to a long format for easy summarization by metric.
    pivot_longer(
      cols = c(auc, brier),
      names_to = ".metric",
      values_to = "estimate"
    ) %>%
    group_by(.metric) %>%
    summarise(
      .estimate = mean(estimate, na.rm = TRUE),
      .lower = quantile(estimate, 0.025, na.rm = TRUE),
      .upper = quantile(estimate, 0.975, na.rm = TRUE),
      .n = n()
    ) %>%
    mutate(model = model_name)

  return(summary_metrics)
}

# Run the Analysis and Generate Plots
n_boot <- 200

# Create a "safe" version of the function that returns NULL on error.
possibly_bootstrap <- possibly(calculate_overall_bootstrap_ci, otherwise = NULL)

# Calculate bootstrapped performance metrics for all four models.
all_model_performance <- bind_rows(
  possibly_bootstrap(current_fit, data, "NEWS2", n_bootstrap = n_boot),
  possibly_bootstrap(light_fit, data, "NEWS2-Light", n_bootstrap = n_boot),
  possibly_bootstrap(full_fit, data, "IEWS-Light", n_bootstrap = n_boot),
  possibly_bootstrap(xgb_fit, data, "XGBoost", n_bootstrap = n_boot)
)

# Create the Dot-Interval Plot
auc_p <- all_model_performance %>%
  filter(.metric == "auc") %>%
  ggplot(aes(
    x = fct_reorder(model, .estimate, .desc = TRUE),
    y = .estimate,
    color = model
  )) +
  geom_pointinterval(
    aes(ymin = .lower, ymax = .upper),
    point_size = 4,
    interval_size = 1.2,
    fatten_point = 1.5
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  labs(
    x = NULL,
    y = "Area Under the Curve (AUC)"
  ) +
  scale_fill_lancet() +
  scale_color_lancet() +
  scale_y_lancet(digits = 5) +
  coord_cartesian(
    ylim = c(
      min(all_model_performance$.lower[
        all_model_performance$.metric == 'auc'
      ]) -
        0.005,
      NA
    )
  )

# Brier Score
# For Brier Score, lower is better, so we reorder in ascending order (default).
brier_p <- all_model_performance %>%
  filter(.metric == "brier") %>%
  ggplot(aes(x = fct_reorder(model, .estimate), y = .estimate, color = model)) +
  geom_pointinterval(
    aes(ymin = .lower, ymax = .upper),
    point_size = 4,
    interval_size = 1.2,
    fatten_point = 1.5
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "none",
    axis.text.x = element_text(angle = 45, hjust = 1),
    plot.title = element_text(hjust = 0.5, face = "bold")
  ) +
  scale_fill_lancet() +
  scale_color_lancet() +
  scale_y_lancet(digits = 5) +
  labs(
    x = NULL,
    y = "Brier Score"
  )

# Combine the two plots into a single figure
final_plot <- auc_p +
  brier_p +
  plot_annotation(
    theme = theme(
      plot.title = element_text(hjust = 0.5, size = 18),
      plot.subtitle = element_text(hjust = 0.5, size = 12)
    )
  )

final_plot

# Compute the metrics for each hospital

calculate_metrics_by_hospital_with_bootstrap_ci_refactored <- function(
  fit_object,
  data,
  model_name,
  n_bootstrap = 200
) {
  calculate_metrics_on_sample <- function(df) {
    tryCatch(
      {
        tibble(
          auc = MetricsWeighted::AUC(
            actual = df$mort24H,
            predicted = df$.pred_Deceased,
            w = df$weights_new
          ),
          brier = MetricsWeighted::mse(
            actual = df$mort24H,
            predicted = df$.pred_Deceased,
            w = df$weights_new
          )
        )
      },
      error = function(e) {
        # Return NA if metric calculation fails on a bootstrap sample
        tibble(auc = NA_real_, brier = NA_real_)
      }
    )
  }

  # Set a single seed for reproducibility of the entire process
  set.seed(42)

  # Prepare the initial predictions data
  preds_data <- fit_object %>%
    collect_predictions() %>%
    arrange(.row) %>%
    mutate(
      mort24H = if_else(data$Status24H == "Deceased", 1, 0),
      Hospital = data$Hospital,
      weights_new = data$weights_new
    )

  metrics_by_hospital <- preds_data %>%
    group_by(Hospital) %>%
    nest() %>% # Creates a list-column 'data' containing the data for each hospital
    mutate(
      n_patients = map_int(data, nrow),

      # Calculate point estimates
      point_estimates = map(data, calculate_metrics_on_sample),

      # Generate bootstrap resamples using rsample
      boot_samples = map(data, ~ rsample::bootstraps(.x, times = n_bootstrap)),

      # Calculate metrics on each bootstrap sample
      boot_metrics = map(
        boot_samples,
        ~ {
          .x %>%
            mutate(
              metrics = map(
                splits,
                ~ calculate_metrics_on_sample(rsample::analysis(.))
              )
            ) %>%
            select(id, metrics) %>%
            unnest(metrics)
        }
      ),

      # Summarize bootstrap results to get percentile confidence intervals
      conf_intervals = map(
        boot_metrics,
        ~ {
          summarise(
            .x,
            across(
              c(auc, brier),
              list(
                lci = ~ quantile(.x, 0.025, na.rm = TRUE),
                uci = ~ quantile(.x, 0.975, na.rm = TRUE)
              ),
              .names = "{.col}_{.fn}"
            )
          )
        }
      )
    ) %>%

    # Unnest and format the final results table
    select(Hospital, n_patients, point_estimates, conf_intervals) %>%
    unnest(c(point_estimates, conf_intervals)) %>%
    rename(
      AUC = auc,
      Brier_Score = brier,
      AUC_LCI = auc_lci,
      AUC_UCI = auc_uci,
      Brier_LCI = brier_lci,
      Brier_UCI = brier_uci
    ) %>%
    mutate(model = model_name) %>%
    arrange(desc(n_patients))

  return(metrics_by_hospital)
}


# Calculate metrics by hospital for each model using the bootstrap function
hospital_metrics_current <- calculate_metrics_by_hospital_with_bootstrap_ci_refactored(
  current_fit,
  data,
  "NEWS2"
)
hospital_metrics_light <- calculate_metrics_by_hospital_with_bootstrap_ci_refactored(
  light_fit,
  data,
  "NEWS2-Light"
)
hospital_metrics_full <- calculate_metrics_by_hospital_with_bootstrap_ci_refactored(
  full_fit,
  data,
  "IEWS-Light"
)
hospital_metrics_xgb <- calculate_metrics_by_hospital_with_bootstrap_ci_refactored(
  xgb_fit,
  data,
  "XGBoost"
)

# Combine all hospital metrics
all_hospital_metrics <- bind_rows(
  hospital_metrics_current,
  hospital_metrics_light,
  hospital_metrics_full,
  hospital_metrics_xgb
)

all_hospital_metrics <- all_hospital_metrics |>
  mutate(
    Hospital = if_else(
      Hospital == "HGH, Herlev and Gentofte Hospital",
      "Herlev and Gentofte Hospital",
      Hospital
    )
  ) |>
  mutate(
    Hospital = if_else(
      Hospital == "NOH, Hospital of North Zealand",
      "Hospital of North Zealand",
      Hospital
    )
  ) |>
  mutate(
    Hospital = if_else(
      Hospital == "Nykøbing Sygehus",
      "Nykøbing Hospital",
      Hospital
    )
  ) |>
  mutate(
    Hospital = if_else(
      Hospital == "Holbæk Sygehus",
      "Holbæk Hospital",
      Hospital
    )
  ) |>
  mutate_at(vars(Hospital), as.factor)

##### Create a dotplot for the hospital metrics ######

hospital_order <- all_hospital_metrics %>%
  ungroup() %>%
  distinct(Hospital, n_patients) %>%
  arrange(desc(n_patients)) %>%
  mutate(y_level = row_number())

shading_data <- hospital_order %>%
  filter(y_level %% 2 == 1) %>%
  mutate(ymin = y_level - 0.5, ymax = y_level + 0.5)

all_hospital_metrics <- all_hospital_metrics %>%
  ungroup() %>%
  left_join(hospital_order %>% select(Hospital, y_level), by = "Hospital") %>%
  mutate(Hospital_Ordered = fct_reorder(Hospital, y_level, .desc = FALSE))


# Plot of the AUC for hospitals

auc_dotplot_ci_refined <- ggplot(all_hospital_metrics) +
  geom_rect(
    data = shading_data,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
    fill = "grey90",
    alpha = 0.5
  ) +
  geom_hline(
    yintercept = hospital_order$y_level,
    color = "grey80",
    linetype = "dotted",
    linewidth = 0.3
  ) +
  geom_errorbarh(
    aes(xmin = AUC_LCI, xmax = AUC_UCI, y = Hospital_Ordered, color = model),
    height = 0,
    position = position_dodge(width = 0.7),
    alpha = 0.7,
    linewidth = 0.6
  ) +
  geom_point(
    aes(x = AUC, y = Hospital_Ordered, color = model),
    position = position_dodge(width = 0.7),
    size = 2.5
  ) +
  geom_vline(xintercept = 0.5, linetype = "dashed", color = "grey50") +
  scale_color_lancet() +
  scale_x_lancet(digits = 3) +
  coord_cartesian(
    xlim = c(
      min(all_hospital_metrics$AUC_LCI, na.rm = TRUE) - 0.02,
      max(all_hospital_metrics$AUC_UCI, na.rm = TRUE) + 0.02
    )
  ) +
  labs(
    x = "AUC (Higher is Better)",
    y = "",
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = rel(0.85))
  )


# Define dodging width for consistency
dodge_width <- 0.7

# Plot for Brier Score for the hospitals
brier_dotplot_ci_refined <- ggplot(
  all_hospital_metrics,
  aes(x = Brier_Score, y = Hospital_Ordered, color = model)
) +
  geom_rect(
    data = shading_data,
    aes(xmin = -Inf, xmax = Inf, ymin = ymin, ymax = ymax),
    fill = "grey90",
    alpha = 0.5,
    inherit.aes = FALSE
  ) +
  geom_hline(
    yintercept = hospital_order$y_level,
    color = "grey80",
    linetype = "dotted",
    linewidth = 0.3
  ) +
  geom_errorbarh(
    aes(xmin = Brier_LCI, xmax = Brier_UCI),
    height = 0,
    position = position_dodge(width = dodge_width),
    alpha = 0.7,
    linewidth = 0.6
  ) +
  geom_point(position = position_dodge(width = dodge_width), size = 2.5) +
  scale_color_lancet() +
  scale_x_lancet(digits = 5) +
  coord_cartesian(
    xlim = c(
      max(0, min(all_hospital_metrics$Brier_LCI, na.rm = TRUE) - 0.001),
      max(all_hospital_metrics$Brier_UCI, na.rm = TRUE) + 0.001
    )
  ) +
  labs(
    x = "Brier Score (Lower is Better)",
    y = NULL,
    color = "Model"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    panel.grid.major.x = element_line(color = "grey85"),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    axis.text.y = element_text(size = rel(0.85))
  )
##############################################
########### Calibration Curves ###############
##############################################

# First check the prevalence of mortality in the dataset

data |>
  group_by(Status24H) |>
  summarise(weighted_n = sum(weights_new, na.rm = TRUE)) |>
  ungroup() |>
  mutate(
    prevalence_percent = (weighted_n / sum(weighted_n)) * 100
  )

# Get the predicted probabilities for all models

current_preds <- current_fit |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

light_preds <- light_fit |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

full_preds <- full_fit |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

xgb_preds <- xgb_fit |>
  collect_predictions() |>
  arrange(.row) |>
  pull(.pred_Deceased)

# Then I need to put them in the dataframe called data

data <- data |>
  mutate(
    pred_current = current_preds,
    pred_light = light_preds,
    pred_full = full_preds,
    pred_xgb = xgb_preds
  )

# Now compute the distribution of the predicted probabilities for all models in separate plots

summary(data$pred_current)
summary(data$pred_light)
summary(data$pred_full)
summary(data$pred_xgb)

# Now I want to compute the risk distribution for each risk group

data |>
  group_by(Risk_Groups_EWS) |>
  summarise(
    mean_current = mean(pred_current),
    mean_xgb = mean(pred_xgb),
    mean_light = mean(pred_light),
    mean_full = mean(pred_full),
    death = mean(Status24H == "Deceased")
  )


# Function for weighted calibration

create_weighted_decile_data <- function(data, truth, estimate, wt) {
  # Use {{}} to handle unquoted column names
  truth_col <- enquo(truth)
  estimate_col <- enquo(estimate)
  wt_col <- enquo(wt)

  # Prepare the data by creating the outcome and decile groups
  binned_data <- data %>%
    mutate(
      truth_numeric = if_else(!!truth_col == "Deceased", 1, 0),
      decile = ntile({{ estimate }}, 10) # Create 10 decile groups
    ) %>%
    group_by(decile)

  # Calculate the straightforward weighted mean of the predictions in each decile
  mean_predictions <- binned_data %>%
    summarise(
      mean_predicted = weighted.mean(
        {{ estimate }},
        w = {{ wt }},
        na.rm = TRUE
      ),
      .groups = 'drop'
    )

  # Second, calculate the weighted observed frequency and its confidence interval
  observed_frequencies <- binned_data %>%
    group_modify(
      ~ {
        design <- svydesign(
          ids = ~1,
          weights = ~ .x[[quo_name(wt_col)]],
          data = .x
        )
        # Calculate the weighted mean of the outcome
        mean_obj <- svymean(~truth_numeric, design, na.rm = TRUE)
        # Calculate the confidence interval for that mean
        ci_obj <- confint(mean_obj)
        # Return a one-row tibble with the results
        tibble(
          observed_frequency = coef(mean_obj),
          conf.low = ci_obj[1],
          conf.high = ci_obj[2]
        )
      }
    )

  # Join the two summaries back together
  left_join(mean_predictions, observed_frequencies, by = "decile")
}

# Generate weighted calibration data for each model
decile_data_current <- create_weighted_decile_data(
  data,
  Status24H,
  pred_current,
  weights_new
) %>%
  mutate(model = "NEWS2")

decile_data_light <- create_weighted_decile_data(
  data,
  Status24H,
  pred_light,
  weights_new
) %>%
  mutate(model = "NEWS2-Light")

decile_data_full <- create_weighted_decile_data(
  data,
  Status24H,
  pred_full,
  weights_new
) %>%
  mutate(model = "IEWS-Light")

decile_data_xgb <- create_weighted_decile_data(
  data,
  Status24H,
  pred_xgb,
  weights_new
) %>%
  mutate(model = "XGBoost")

# Combine all data into a single tibble
all_decile_data <- bind_rows(
  decile_data_current,
  decile_data_light,
  decile_data_full,
  decile_data_xgb
) %>%
  mutate(
    model = factor(
      model,
      levels = c("NEWS2", "NEWS2-Light", "IEWS-Light", "XGBoost")
    )
  )


long_cal_data <- all_decile_data %>%
  # We only need the decile, predicted, and observed values for the plot
  select(decile, model, mean_predicted, observed_frequency) %>%
  pivot_longer(
    cols = c(mean_predicted, observed_frequency), # The columns to pivot
    names_to = "group", # New column for the group names
    values_to = "value" # New column for their values
  ) %>%
  # Make the group names more readable for the plot legend
  mutate(
    group = factor(
      case_when(
        group == "mean_predicted" ~ "Predicted",
        group == "observed_frequency" ~ "Observed"
      ),
      levels = c("Predicted", "Observed")
    )
  )

# Create the grouped bar chart
calibration_bar_plot <- ggplot(
  long_cal_data,
  aes(x = decile, y = value, fill = group)
) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  facet_wrap(~model, ncol = 2) +
  scale_fill_lancet() +
  scale_x_continuous(breaks = 1:10) +
  scale_y_lancet(digits = 4) +
  labs(
    x = "Deciles of Predicted Risk",
    y = "Frequency",
    fill = "Group" # This is the title for the legend
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    strip.text = element_text(face = "bold", size = 12),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank()
  )

# Print the final plot
calibration_bar_plot

################################################################################
########################## Calibration Curves by subgroups ####################
################################################################################

create_grouped_decile_data <- function(data, truth, estimate, wt, ...) {
  # Capture the additional grouping variables
  grouping_vars <- enquos(...)
  truth_col <- enquo(truth)
  estimate_col <- enquo(estimate)
  wt_col <- enquo(wt)

  binned_data <- data %>%
    mutate(truth_numeric = if_else(!!truth_col == "Deceased", 1, 0)) %>%
    group_by(!!!grouping_vars) %>%
    mutate(decile = ntile(!!estimate_col, 10)) %>%
    group_by(!!!grouping_vars, decile) # Regroup with deciles for summarization

  # Calculate the weighted mean of predictions
  mean_predictions <- binned_data %>%
    summarise(
      mean_predicted = weighted.mean(
        !!estimate_col,
        w = !!wt_col,
        na.rm = TRUE
      ),
      .groups = 'drop'
    )

  # Calculate weighted observed frequency and CI using the survey package
  observed_frequencies <- binned_data %>%
    group_modify(
      ~ {
        design <- svydesign(
          ids = ~1,
          weights = ~ .x[[quo_name(wt_col)]],
          data = .x
        )
        mean_obj <- svymean(~truth_numeric, design, na.rm = TRUE)
        ci_obj <- confint(mean_obj)
        tibble(
          observed_frequency = coef(mean_obj),
          conf.low = ci_obj[1],
          conf.high = ci_obj[2]
        )
      },
      .keep = TRUE
    ) # .keep = TRUE retains grouping variables

  # Join the summaries
  left_join(
    mean_predictions,
    observed_frequencies,
    by = c(sapply(grouping_vars, quo_name), "decile")
  )
}

# Generate the age-stratified decile data for all models
age_decile_current <- create_grouped_decile_data(
  data,
  Status24H,
  pred_current,
  weights_new,
  Age_Group
) %>%
  mutate(model = "NEWS2")

age_decile_light <- create_grouped_decile_data(
  data,
  Status24H,
  pred_light,
  weights_new,
  Age_Group
) %>%
  mutate(model = "NEWS2-Light")

age_decile_full <- create_grouped_decile_data(
  data,
  Status24H,
  pred_full,
  weights_new,
  Age_Group
) %>%
  mutate(model = "IEWS-Light")

age_decile_xgb <- create_grouped_decile_data(
  data,
  Status24H,
  pred_xgb,
  weights_new,
  Age_Group
) %>%
  mutate(model = "XGBoost")

# Combine all results
all_age_decile_data <- bind_rows(
  age_decile_current,
  age_decile_light,
  age_decile_full,
  age_decile_xgb
) %>%
  mutate(
    model = factor(
      model,
      levels = c("NEWS2", "NEWS2-Light", "IEWS-Light", "XGBoost")
    )
  )


# Pivot the data into a long format suitable for ggplot
long_age_cal_data <- all_age_decile_data %>%
  select(Age_Group, decile, model, mean_predicted, observed_frequency) %>%
  pivot_longer(
    cols = c(mean_predicted, observed_frequency),
    names_to = "group",
    values_to = "value"
  ) %>%
  mutate(
    group = factor(
      case_when(
        group == "mean_predicted" ~ "Predicted",
        group == "observed_frequency" ~ "Observed"
      ),
      levels = c("Predicted", "Observed")
    )
  )

# Create the final faceted bar chart
age_cal_bar_plot <- ggplot(
  long_age_cal_data,
  aes(x = decile, y = value, fill = group)
) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.85),
    width = 0.75
  ) +
  facet_grid(Age_Group ~ model, scales = "free_y") +
  scale_fill_lancet() +
  scale_x_continuous(breaks = 1:10) +
  scale_y_lancet(digits = 5) +
  labs(
    x = "Deciles of Predicted Risk (within Age Group)",
    y = "Frequency",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"), # Make facet titles bold
    strip.background = element_rect(fill = "gray92", color = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing.y = unit(2, "lines") # Add vertical spacing between age groups
  )

# Print the final plot
age_cal_bar_plot

# Calibration Plots for sex groups

# Generate the age-stratified decile data for all models
sex_decile_current <- create_grouped_decile_data(
  data,
  Status24H,
  pred_current,
  weights_new,
  Sex
) %>%
  mutate(model = "NEWS2")

sex_decile_light <- create_grouped_decile_data(
  data,
  Status24H,
  pred_light,
  weights_new,
  Sex
) %>%
  mutate(model = "NEWS2-Light")

sex_decile_full <- create_grouped_decile_data(
  data,
  Status24H,
  pred_full,
  weights_new,
  Sex
) %>%
  mutate(model = "IEWS-Light")

sex_decile_xgb <- create_grouped_decile_data(
  data,
  Status24H,
  pred_xgb,
  weights_new,
  Sex
) %>%
  mutate(model = "XGBoost")

# Combine all results
all_sex_decile_data <- bind_rows(
  sex_decile_current,
  sex_decile_light,
  sex_decile_full,
  sex_decile_xgb
) %>%
  mutate(
    model = factor(
      model,
      levels = c("NEWS2", "NEWS2-Light", "IEWS-Light", "XGBoost")
    )
  )


# Pivot the data into a long format suitable for ggplot
long_sex_cal_data <- all_sex_decile_data %>%
  select(Sex, decile, model, mean_predicted, observed_frequency) %>%
  pivot_longer(
    cols = c(mean_predicted, observed_frequency),
    names_to = "group",
    values_to = "value"
  ) %>%
  mutate(
    group = factor(
      case_when(
        group == "mean_predicted" ~ "Predicted",
        group == "observed_frequency" ~ "Observed"
      ),
      levels = c("Predicted", "Observed")
    )
  )

# Create the final faceted bar chart
sex_cal_bar_plot <- ggplot(
  long_sex_cal_data,
  aes(x = decile, y = value, fill = group)
) +
  geom_bar(
    stat = "identity",
    position = position_dodge(width = 0.85),
    width = 0.75
  ) +
  facet_grid(Sex ~ model, scales = "free_y") +
  scale_fill_lancet() +
  scale_x_continuous(breaks = 1:10) +
  scale_y_lancet(digits = 5) +
  labs(
    x = "Deciles of Predicted Risk (within Sex Group)",
    y = "Frequency",
    fill = "Group"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    legend.position = "top",
    strip.text = element_text(face = "bold"), # Make facet titles bold
    strip.background = element_rect(fill = "gray92", color = "white"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.spacing.y = unit(2, "lines") # Add vertical spacing between sex groups
  )

# Print the final plot
sex_cal_bar_plot

################################################################################
########################## Decision curve analysis #############################
################################################################################

# First compute the weighted prevalence of my data

weighted_prevalence <- weighted.mean(
  x = if_else(data$Status24H == "Deceased", 1, 0),
  w = data$weights_new,
  na.rm = TRUE
)

# Compute the multiples of the prevalence to use as thresholds
cut_points <- weighted_prevalence * c(1, 2, 4, 8, 10, 20, 30)

#  Filter out any cut points that are beyond our plot's x-axis limit
plot_xlim <- 0.05
cut_points_to_plot <- cut_points[cut_points <= plot_xlim]

# Print the calculated values to see what they are
print("Weighted Prevalence of Mortality:")
print(weighted_prevalence)
print("Cut Points for Vertical Lines:")
print(cut_points_to_plot)


calculate_weighted_nb <- function(data, estimate, truth, wt, thresholds) {
  # Use {{}} to handle unquoted column names
  estimate_col <- enquo(estimate)
  truth_col <- enquo(truth)
  wt_col <- enquo(wt)

  # Prepare the data once
  df <- data %>%
    select(!!estimate_col, !!truth_col, !!wt_col) %>%
    mutate(truth_numeric = if_else(!!truth_col == "Deceased", 1, 0))

  # Get the total sum of weights
  total_weight <- sum(df[[quo_name(wt_col)]], na.rm = TRUE)

  # Use map_dfr to iterate over each threshold and calculate net benefit
  map_dfr(
    thresholds,
    ~ {
      pt <- .x # Current threshold

      # Calculate weighted counts of TP and FP at the current threshold
      summary_df <- df %>%
        mutate(
          is_positive = if_else(!!estimate_col >= pt, 1, 0)
        ) %>%
        group_by(truth_numeric, is_positive) %>%
        summarise(sum_w = sum(!!wt_col, na.rm = TRUE), .groups = 'drop')

      # Extract weighted TP and FP
      tp_w <- summary_df$sum_w[
        summary_df$truth_numeric == 1 & summary_df$is_positive == 1
      ]
      fp_w <- summary_df$sum_w[
        summary_df$truth_numeric == 0 & summary_df$is_positive == 1
      ]

      # Handle cases where there are no TPs or FPs
      if (length(tp_w) == 0) {
        tp_w <- 0
      }
      if (length(fp_w) == 0) {
        fp_w <- 0
      }

      # Calculate net benefit
      net_benefit <- (tp_w / total_weight) -
        (fp_w / total_weight) * (pt / (1 - pt))

      tibble(
        threshold = pt,
        net_benefit = net_benefit
      )
    }
  )
}

# Define the thresholds to evaluate, matching your original code
thresholds <- seq(0, 0.05, by = 0.0005)

# Calculate Net Benefit for each model
nb_current <- calculate_weighted_nb(
  data,
  pred_current,
  Status24H,
  weights_new,
  thresholds
) %>%
  mutate(model = "NEWS2")
nb_light <- calculate_weighted_nb(
  data,
  pred_light,
  Status24H,
  weights_new,
  thresholds
) %>%
  mutate(model = "NEWS2-Light")
nb_full <- calculate_weighted_nb(
  data,
  pred_full,
  Status24H,
  weights_new,
  thresholds
) %>%
  mutate(model = "IEWS-Light")
nb_xgb <- calculate_weighted_nb(
  data,
  pred_xgb,
  Status24H,
  weights_new,
  thresholds
) %>%
  mutate(model = "XGBoost")

# Calculate Net Benefit for "Treat All"

# First, find the weighted prevalence of the event
weighted_prevalence <- weighted.mean(
  if_else(data$Status24H == "Deceased", 1, 0),
  w = data$weights_new
)
nb_all <- tibble(
  threshold = thresholds,
  net_benefit = weighted_prevalence -
    (1 - weighted_prevalence) * (thresholds / (1 - thresholds)),
  model = "Treat All"
)

# Create the "Treat None" curve
nb_none <- tibble(
  threshold = thresholds,
  net_benefit = 0,
  model = "Treat None"
)

# Combine all data for plotting
all_nb_data <- bind_rows(
  nb_current,
  nb_light,
  nb_full,
  nb_xgb,
  nb_all,
  nb_none
) %>%
  mutate(
    model = factor(
      model,
      levels = c(
        "XGBoost",
        "IEWS-Light",
        "NEWS2",
        "NEWS2-Light",
        "Treat All",
        "Treat None"
      )
    )
  )

# Create the weighted Decision Curve Analysis plot
plot_data_stopped <- all_nb_data %>%
  mutate(
    net_benefit = if_else(
      model == "Treat All" & net_benefit < -0.005,
      NA_real_,
      net_benefit
    )
  )

# Create the weighted Decision Curve Analysis plot
weighted_dca_final_with_lines <- ggplot(
  plot_data_stopped,
  aes(x = threshold, y = net_benefit, color = model)
) +
  geom_line(linewidth = 1.1) +
  coord_cartesian(
    xlim = c(0, plot_xlim),
    ylim = c(-0.001, max(all_nb_data$net_benefit, na.rm = TRUE))
  ) +
  scale_x_lancet(digits = 4) +
  scale_y_lancet(digits = 4, breaks = scales::pretty_breaks(n = 7)) +
  labs(
    x = "Probability Threshold",
    y = "Net Benefit",
    color = "Model"
  ) +
  scale_color_lancet() +
  theme_minimal(base_size = 14) +
  theme(legend.position = "top")

# Print the final plot
weighted_dca_final_with_lines


# Creating the comparison of net benefit differences

# For models
nb_at_cuts_current <- calculate_weighted_nb(
  data,
  pred_current,
  Status24H,
  weights_new,
  cut_points
) %>%
  mutate(model = "NEWS2")
nb_at_cuts_light <- calculate_weighted_nb(
  data,
  pred_light,
  Status24H,
  weights_new,
  cut_points
) %>%
  mutate(model = "NEWS2-Light")
nb_at_cuts_full <- calculate_weighted_nb(
  data,
  pred_full,
  Status24H,
  weights_new,
  cut_points
) %>%
  mutate(model = "IEWS-Light")
nb_at_cuts_xgb <- calculate_weighted_nb(
  data,
  pred_xgb,
  Status24H,
  weights_new,
  cut_points
) %>%
  mutate(model = "XGBoost")

# For "Treat All" strategy
nb_at_cuts_all <- tibble(
  threshold = cut_points,
  net_benefit = weighted_prevalence -
    (1 - weighted_prevalence) * (cut_points / (1 - cut_points)),
  model = "Treat All"
)

# For "Treat None" strategy
nb_at_cuts_none <- tibble(
  threshold = cut_points,
  net_benefit = 0,
  model = "Treat None"
)

# Combine these precise calculations into a single data frame
nb_at_exact_cuts <- bind_rows(
  nb_at_cuts_current,
  nb_at_cuts_light,
  nb_at_cuts_full,
  nb_at_cuts_xgb,
  nb_at_cuts_all,
  nb_at_cuts_none
)

# Pivot, Scale, and Calculate Differences

comparison_data_consistent <- nb_at_exact_cuts %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%

  # Scale by 10,000 to get values per 10,000 patients
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(
    across(
      .cols = c(
        NEWS2,
        `NEWS2-Light`,
        XGBoost,
        `IEWS-Light`,
        `Treat All`,
        `Treat None`
      ),
      .fns = ~ round(.x, digits = 1)
    )
  ) %>%
  mutate(
    difference1 = NEWS2 - XGBoost,
    difference2 = NEWS2 - `NEWS2-Light`
  ) %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS2,
    `NEWS2-Light`,
    `IEWS-Light`,
    XGBoost,
    difference1,
    difference2
  ) %>%
  arrange(threshold)


# Create the Final gt Table

# Calculate the color domain robustly before creating the table
domain_range_consistent <- range(
  c(
    comparison_data_consistent$difference1,
    comparison_data_consistent$difference2
  ),
  na.rm = TRUE
)

final_comparison_table <- comparison_data_consistent %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS2,
    `NEWS2-Light`,
    XGBoost,
    difference1,
    difference2
  ) %>%
  gt() %>%
  tab_header(
    title = "Decision Curve Analysis: Net Benefit Comparison",
    subtitle = "Comparing models at exact prevalence-based thresholds. Values are per 10,000 patients."
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS2 = "NEWS2",
    `NEWS2-Light` = "NEWS2-Light",
    XGBoost = "XGBoost",
    difference1 = "NEWS2 vs XGBoost",
    difference2 = "NEWS2 vs NEWS2-Light"
  ) %>%
  # Format numbers with middle dot and space as thousands separator
  fmt_number(
    columns = threshold,
    decimals = 4,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  fmt_number(
    columns = where(is.numeric) & !matches("threshold"),
    decimals = 1,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  tab_spanner(
    label = "Net Benefit per 10 000 Patients",
    columns = c(`Treat All`, `Treat None`, NEWS2, `NEWS2-Light`, XGBoost)
  ) %>%
  tab_spanner(
    label = "Net Benefit Difference per 10 000",
    columns = c(difference1, difference2)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(difference1, difference2))
  ) %>%
  tab_footnote(
    footnote = "Positive (blue) indicates NEWS2 has higher net benefit; Negative (red) indicates the comparator is better.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

# Display the final, numerically consistent table
final_comparison_table

final_comparison_table |> gtsave("weights_final_comparison_table_lanc.docx")

################################################################################
########################## Create DCA for subgroups ############################
################################################################################

# Get the unique age groups from your data
age_groups <- unique(data$Age_Group)

# Loop over each age group to perform DCA at exact thresholds
age_stratified_nb_exact <- map_dfr(
  age_groups,
  ~ {
    current_age_group <- .x

    # Filter data for the current age group
    data_subset <- data %>% filter(Age_Group == current_age_group)

    # Calculate the weighted prevalence and exact cut points for this age group
    prevalence <- weighted.mean(
      if_else(data_subset$Status24H == "Deceased", 1, 0),
      w = data_subset$weights_new,
      na.rm = TRUE
    )
    if (is.nan(prevalence) || prevalence == 0) {
      return(NULL)
    } # Skip if no events in subgroup

    cut_points <- prevalence * c(1, 2, 4, 8, 10, 20, 30)

    # Calculate Net Benefit for each model ONLY at the exact cut_points
    nb_current <- calculate_weighted_nb(
      data_subset,
      pred_current,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS2")
    nb_light <- calculate_weighted_nb(
      data_subset,
      pred_light,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS2-Light")
    nb_full <- calculate_weighted_nb(
      data_subset,
      pred_full,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "IEWS-Light")
    nb_xgb <- calculate_weighted_nb(
      data_subset,
      pred_xgb,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "XGBoost")

    nb_all <- tibble(
      threshold = cut_points,
      net_benefit = prevalence -
        (1 - prevalence) * (cut_points / (1 - cut_points)),
      model = "Treat All"
    )
    nb_none <- tibble(
      threshold = cut_points,
      net_benefit = 0,
      model = "Treat None"
    )

    # Combine results for this age group and add the identifier
    bind_rows(nb_current, nb_light, nb_full, nb_xgb, nb_all, nb_none) %>%
      mutate(Age_Group = current_age_group)
  },
  .id = NULL
)


# Create the final table

final_age_stratified_table <- age_stratified_nb_exact %>%
  group_by(Age_Group) %>%
  arrange(threshold, .by_group = TRUE) %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%

  # Scale by 10,000 to get values per 10,000 patients
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(
    across(
      .cols = c(
        NEWS2,
        `NEWS2-Light`,
        XGBoost,
        `IEWS-Light`,
        `Treat All`,
        `Treat None`
      ),
      .fns = ~ round(.x, digits = 1)
    )
  ) %>%
  mutate(
    difference1 = NEWS2 - XGBoost,
    difference2 = NEWS2 - `NEWS2-Light`
  )

# Calculate the color domain robustly before creating the table
domain_range <- range(
  c(
    final_age_stratified_table$difference1,
    final_age_stratified_table$difference2
  ),
  na.rm = TRUE
)

final_age_stratified_table_weights <- final_age_stratified_table %>%
  select(
    threshold,
    `Treat All`,
    `Treat None`,
    NEWS2,
    `NEWS2-Light`,
    XGBoost,
    difference1,
    difference2
  ) %>%
  gt(groupname_col = "Age_Group") %>%
  tab_header(
    title = "Weighted Age-Stratified Decision Curve Analysis",
    subtitle = md(
      "Net benefit differences at **exact** prevalence-based thresholds. **Values are per 10,000 patients.**"
    )
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS2 = "NEWS2",
    `NEWS2-Light` = "NEWS2-Light",
    XGBoost = "XGBoost",
    difference1 = "NEWS2 vs. XGBoost",
    difference2 = "NEWS2 vs. NEWS2-Light"
  ) %>%
  # Format numbers with middle dot and space as thousands separator
  fmt_number(
    columns = threshold,
    decimals = 4,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  fmt_number(
    columns = where(is.numeric) & !matches("threshold"),
    decimals = 1,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  tab_spanner(
    label = "Net Benefit per 10 000 Patients",
    columns = c(`Treat All`, `Treat None`, NEWS2, `NEWS2-Light`, XGBoost)
  ) %>%
  tab_spanner(
    label = "Net Benefit Difference per 10 000",
    columns = c(difference1, difference2)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(difference1, difference2))
  ) %>%
  tab_options(row_group.font.weight = "bold") %>%
  tab_footnote(
    footnote = "Positive (blue) indicates NEWS2 has higher net benefit; Negative (red) indicates the comparator is better.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

# Display the final, numerically consistent table
final_age_stratified_table_weights

final_age_stratified_table_weights |>
  gtsave("weights_final_age_stratified_table.docx")

# Save all the metrics

all_model_performance |>
  write_parquet("weights_all_bootstrapped_metrics.parquet")


# Evaluate the net benefit differences across departments

# We will exclude any levels that might be "Other"
department_levels <- levels(data$Department_Name_Fac)
department_levels <- department_levels[department_levels != "Other"]


# Loop over each department to perform DCA at exact thresholds
all_department_nb <- map_dfr(
  department_levels,
  ~ {
    current_department <- .x

    # Filter data for the current department
    data_subset <- data %>% filter(Department_Name_Fac == current_department)

    # Calculate the weighted prevalence and cut points for this department
    prevalence <- weighted.mean(
      if_else(data_subset$Status24H == "Deceased", 1, 0),
      w = data_subset$weights_new,
      na.rm = TRUE
    )

    # Skip this department if there are no events (mortality)
    if (is.nan(prevalence) || prevalence == 0) {
      return(NULL)
    }

    cut_points <- prevalence * c(1, 2, 4, 8, 10, 20, 30)

    # c. Calculate Net Benefit for each model at the exact cut_points
    nb_current <- calculate_weighted_nb(
      data_subset,
      pred_current,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS2")
    nb_light <- calculate_weighted_nb(
      data_subset,
      pred_light,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS2-Light")
    nb_xgb <- calculate_weighted_nb(
      data_subset,
      pred_xgb,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "XGBoost")

    nb_all <- tibble(
      threshold = cut_points,
      net_benefit = prevalence -
        (1 - prevalence) * (cut_points / (1 - cut_points)),
      model = "Treat All"
    )
    nb_none <- tibble(
      threshold = cut_points,
      net_benefit = 0,
      model = "Treat None"
    )

    # Combine results for this department and add the identifier
    bind_rows(nb_current, nb_light, nb_xgb, nb_all, nb_none) %>%
      mutate(Department_Name_Fac = current_department) # Use the correct column name
  },
  .id = NULL
)


# Pivot, Scale, Round, and create the final table

# First, process the combined data
final_department_table_data <- all_department_nb %>%
  group_by(Department_Name_Fac) %>%
  arrange(threshold, .by_group = TRUE) %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(across(
    c(NEWS2, `NEWS2-Light`, XGBoost, `Treat All`, `Treat None`),
    ~ round(.x, digits = 1),
    .names = "{.col}"
  )) %>%
  mutate(
    difference1 = NEWS2 - XGBoost,
    difference2 = NEWS2 - `NEWS2-Light`
  )

# Calculate the color domain robustly before creating the table
domain_range_dept <- range(
  c(
    final_department_table_data$difference1,
    final_department_table_data$difference2
  ),
  na.rm = TRUE
)

# Generate the gt object
final_department_table <- final_department_table_data %>%
  gt(groupname_col = "Department_Name_Fac") %>%
  tab_header(
    title = "Department-Stratified Decision Curve Analysis",
    subtitle = md(
      "Net benefit differences at prevalence-based thresholds. **Values are per 10,000 patients.**"
    )
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS2 = "NEWS2",
    `NEWS2-Light` = "NEWS2-Light",
    XGBoost = "XGBoost",
    difference1 = "NEWS2 vs. XGBoost",
    difference2 = "NEWS2 vs. NEWS2-Light"
  ) %>%
  fmt_number(
    columns = threshold,
    decimals = 4,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  fmt_number(
    columns = where(is.numeric) & !matches("threshold"),
    decimals = 1,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  tab_spanner(
    label = "Net Benefit per 10 000 Patients",
    columns = c(`Treat All`, `Treat None`, NEWS2, `NEWS2-Light`, XGBoost)
  ) %>%
  tab_spanner(
    label = "Net Benefit Difference per 10 000",
    columns = c(difference1, difference2)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(difference1, difference2))
  ) %>%
  tab_options(row_group.font.weight = "bold") %>%
  tab_footnote(
    footnote = "Positive (blue) indicates NEWS2 has higher net benefit; Negative (red) indicates the comparator is better.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

# Display the final stacked table
final_department_table

final_department_table |> gtsave("weights_dep_stratified_table.docx")


# Now repeat this for males and females

# Get the unique levels for Sex from your data ---
sex_levels <- unique(data$Sex)


# Loop over each sex to perform DCA at exact thresholds
sex_stratified_nb_exact <- map_dfr(
  sex_levels,
  ~ {
    current_sex <- .x

    # Filter data for the current sex
    data_subset <- data %>% filter(Sex == current_sex)

    # Calculate the weighted prevalence and cut points for this subgroup
    prevalence <- weighted.mean(
      if_else(data_subset$Status24H == "Deceased", 1, 0),
      w = data_subset$weights_new,
      na.rm = TRUE
    )

    # Skip this group if there are no events (mortality)
    if (is.nan(prevalence) || prevalence == 0) {
      return(NULL)
    }

    cut_points <- prevalence * c(1, 2, 4, 8, 10, 20, 30)

    # Calculate Net Benefit for each model at the exact cut_points
    nb_current <- calculate_weighted_nb(
      data_subset,
      pred_current,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS2")
    nb_light <- calculate_weighted_nb(
      data_subset,
      pred_light,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS2-Light")
    nb_xgb <- calculate_weighted_nb(
      data_subset,
      pred_xgb,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "XGBoost")

    nb_all <- tibble(
      threshold = cut_points,
      net_benefit = prevalence -
        (1 - prevalence) * (cut_points / (1 - cut_points)),
      model = "Treat All"
    )
    nb_none <- tibble(
      threshold = cut_points,
      net_benefit = 0,
      model = "Treat None"
    )

    # Combine results for this sex and add the identifier
    bind_rows(nb_current, nb_light, nb_xgb, nb_all, nb_none) %>%
      mutate(Sex = current_sex) # Add the Sex identifier
  },
  .id = NULL
)


# Pivot, Scale, Round, and Create the Final gt Table ---

# First, process the combined data
final_sex_table_data <- sex_stratified_nb_exact %>%
  group_by(Sex) %>%
  arrange(threshold, .by_group = TRUE) %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(across(
    c(NEWS2, `NEWS2-Light`, XGBoost, `Treat All`, `Treat None`),
    ~ round(.x, digits = 1)
  )) %>%
  mutate(
    difference1 = NEWS2 - XGBoost,
    difference2 = NEWS2 - `NEWS2-Light`
  )

# Calculate the color domain robustly before creating the table
domain_range_sex <- range(
  c(final_sex_table_data$difference1, final_sex_table_data$difference2),
  na.rm = TRUE
)

# Generate the gt object
final_sex_table <- final_sex_table_data %>%
  gt(groupname_col = "Sex") %>%
  tab_header(
    title = "Sex-Stratified Decision Curve Analysis",
    subtitle = md(
      "Net benefit differences at prevalence-based thresholds. **Values are per 10,000 patients.**"
    )
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS2 = "NEWS2",
    `NEWS2-Light` = "NEWS2-Light",
    XGBoost = "XGBoost",
    difference1 = "NEWS2 vs. XGBoost",
    difference2 = "NEWS2 vs. NEWS2-Light"
  ) %>%
  fmt_number(
    columns = threshold,
    decimals = 4,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  fmt_number(
    columns = where(is.numeric) & !matches("threshold"),
    decimals = 1,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  tab_spanner(
    label = "Net Benefit per 10 000 Patients",
    columns = c(`Treat All`, `Treat None`, NEWS2, `NEWS2-Light`, XGBoost)
  ) %>%
  tab_spanner(
    label = "Net Benefit Difference per 10 000",
    columns = c(difference1, difference2)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(difference1, difference2))
  ) %>%
  tab_options(row_group.font.weight = "bold") %>%
  tab_footnote(
    footnote = "Positive (blue) indicates NEWS2 has higher net benefit; Negative (red) indicates the comparator is better.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

# Display the final stacked table
final_sex_table

final_sex_table |> gtsave("weights_sex_stratified_table.docx")


#### For diagnosis categories

# Get the unique levels for Diagnosis_Category, filtering out any NA values
diagnosis_levels <- unique(data$Diagnosis_Category[
  !is.na(data$Diagnosis_Category)
])

# Loop over each diagnosis category to perform DCA at exact thresholds
diagnosis_stratified_nb_exact <- map_dfr(
  diagnosis_levels,
  ~ {
    current_category <- .x

    # Filter data for the current diagnosis category
    data_subset <- data %>%
      filter(
        Diagnosis_Category == current_category & !is.na(Diagnosis_Category)
      )

    # Calculate the weighted prevalence and cut points for this subgroup
    prevalence <- weighted.mean(
      if_else(data_subset$Status24H == "Deceased", 1, 0),
      w = data_subset$weights_new,
      na.rm = TRUE
    )

    # Skip this group if there are no events (mortality)
    if (is.nan(prevalence) || prevalence == 0) {
      return(NULL)
    }

    cut_points <- prevalence * c(1, 2, 4, 8, 10, 20, 30)

    # Calculate Net Benefit for each model at the exact cut_points
    nb_current <- calculate_weighted_nb(
      data_subset,
      pred_current,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS2")
    nb_light <- calculate_weighted_nb(
      data_subset,
      pred_light,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "NEWS2-Light")
    nb_xgb <- calculate_weighted_nb(
      data_subset,
      pred_xgb,
      Status24H,
      weights_new,
      cut_points
    ) %>%
      mutate(model = "XGBoost")

    nb_all <- tibble(
      threshold = cut_points,
      net_benefit = prevalence -
        (1 - prevalence) * (cut_points / (1 - cut_points)),
      model = "Treat All"
    )
    nb_none <- tibble(
      threshold = cut_points,
      net_benefit = 0,
      model = "Treat None"
    )

    # Combine results for this category and add the identifier
    bind_rows(nb_current, nb_light, nb_xgb, nb_all, nb_none) %>%
      mutate(Diagnosis_Category = current_category)
  },
  .id = NULL
)

# --- Pivot, Scale, Round, and Create the Final gt Table ---

# First, process the combined data
final_diagnosis_table_data <- diagnosis_stratified_nb_exact %>%
  group_by(Diagnosis_Category) %>%
  arrange(threshold, .by_group = TRUE) %>%
  select(threshold, model, net_benefit) %>%
  pivot_wider(names_from = model, values_from = net_benefit) %>%
  mutate(across(where(is.numeric) & !matches("threshold"), ~ .x * 10000)) %>%
  mutate(across(
    c(NEWS2, `NEWS2-Light`, XGBoost, `Treat All`, `Treat None`),
    ~ round(.x, digits = 1)
  )) %>%
  mutate(
    difference1 = NEWS2 - XGBoost,
    difference2 = NEWS2 - `NEWS2-Light`
  )

# Generate the gt object
final_diagnosis_table <- final_diagnosis_table_data %>%
  gt(groupname_col = "Diagnosis_Category") %>%
  tab_header(
    title = "Diagnosis-Stratified Decision Curve Analysis",
    subtitle = md(
      "Net benefit differences at prevalence-based thresholds. **Values are per 10,000 patients.**"
    )
  ) %>%
  cols_label(
    threshold = "Risk Threshold",
    `Treat All` = "Treat All",
    `Treat None` = "Treat None",
    NEWS2 = "NEWS2",
    `NEWS2-Light` = "NEWS2-Light",
    XGBoost = "XGBoost",
    difference1 = "NEWS2 vs. XGBoost",
    difference2 = "NEWS2 vs. NEWS2-Light"
  ) %>%
  fmt_number(
    columns = threshold,
    decimals = 4,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  fmt_number(
    columns = where(is.numeric) & !matches("threshold"),
    decimals = 1,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  tab_spanner(
    label = "Net Benefit per 10,000 Patients",
    columns = c(`Treat All`, `Treat None`, NEWS2, `NEWS2-Light`, XGBoost)
  ) %>%
  tab_spanner(
    label = "Net Benefit Difference per 10,000",
    columns = c(difference1, difference2)
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_body(columns = c(difference1, difference2))
  ) %>%
  tab_options(row_group.font.weight = "bold") %>%
  tab_footnote(
    footnote = "Positive indicates NEWS2 has higher net benefit; Negative indicates the comparator is better.",
    locations = cells_column_spanners(spanners = contains("Difference"))
  )

# Display the final stacked table

final_diagnosis_table

final_diagnosis_table |> gtsave("weights_diagnoses_stratified_table.docx")


##################

#### Creation of table with missingness (we need the unimputed data)

df <- read_parquet("/home/alex/ews/NEWS2_Evaluation/single_ews.parquet")

# Now I need to keep only those individuals who were in my modelling workflow

df <- df |> filter(CSN %in% data$CSN)

df <- df |>
  mutate(
    day_type = if_else(wday(recorded_time) %in% 2:6, "Weekday", "Weekend")
  ) |>
  mutate(hour = hour(recorded_time)) |>
  mutate(
    time_of_day = case_when(
      hour >= 6 & hour < 12 ~ "Morning",
      hour >= 12 & hour < 22 ~ "Afternoon/Evening",
      TRUE ~ "Night"
    )
  ) |>
  mutate(month = month(recorded_time, label = TRUE, abbr = TRUE)) |>
  select(-hour) |>
  mutate_at(vars(day_type, time_of_day), as.factor)

# And now for the missingness percentage

df <- df |>
  left_join(
    data |>
      select(PT_ID, CSN, starts_with("median"), previous_icu_respiratory),
    by = c("PT_ID", "CSN")
  )


missingness_table <- df %>% # Replace with your actual dataset name
  select(
    Age,
    Sex,
    Previous_Hosp,
    Hospital,
    Consciousness,
    Oxygen_Supplement,
    Respiration_Rate,
    Pulse,
    Temperature,
    Saturation,
    Blood_Pressure.Sys,
    Blood_Pressure.Dia,
    previous_icu_respiratory,
    EWS_score,
    median_Hemoglobin,
    median_Leukocytter,
    median_Trombocytter,
    median_Kreatinin,
    median_ALAT,
    median_LDH,
    median_Albumin,
    median_CRP,
    median_Laktat_ab,
    median_Troponin_T,
    median_Laktat_vb
  ) %>%
  summarise_all(~ sum(is.na(.))) %>%
  pivot_longer(
    everything(),
    names_to = "Variable",
    values_to = "Missing_Count"
  ) %>%
  mutate(
    Total_N = nrow(df), # Replace with your actual dataset name
    Missing_Percentage = (Missing_Count / Total_N) * 100,
    # Add polished variable names
    Variable_Label = case_when(
      Variable == "Age" ~ "Age",
      Variable == "Sex" ~ "Sex",
      Variable == "Previous_Hosp" ~ "Previous hospitalizations",
      Variable == "Hospital" ~ "Hospital",
      Variable == "Consciousness" ~ "Consciousness",
      Variable == "Oxygen_Supplement" ~ "Supplemental oxygen",
      Variable == "Respiration_Rate" ~ "Respiratory rate (breaths/min)",
      Variable == "Pulse" ~ "Pulse (beats/min)",
      Variable == "Temperature" ~ "Temperature (°C)",
      Variable == "Saturation" ~ "Oxygen saturation (%)",
      Variable == "Blood_Pressure.Sys" ~ "Systolic blood pressure (mm Hg)",
      Variable == "Blood_Pressure.Dia" ~ "Diastolic blood pressure (mm Hg)",
      Variable == "previous_icu_respiratory" ~
        "Previous ICU/respiratory support",
      Variable == "EWS_score" ~ "NEWS2 score",
      Variable == "median_Hemoglobin" ~ "Hemoglobin (g/L)",
      Variable == "median_Leukocytter" ~ "Leukocytes (×10⁹/L)",
      Variable == "median_Trombocytter" ~ "Platelets (×10⁹/L)",
      Variable == "median_Kreatinin" ~ "Creatinine (μmol/L)",
      Variable == "median_ALAT" ~ "Alanine aminotransferase (U/L)",
      Variable == "median_LDH" ~ "Lactate dehydrogenase (U/L)",
      Variable == "median_Albumin" ~ "Albumin (g/L)",
      Variable == "median_CRP" ~ "C-reactive protein (mg/L)",
      Variable == "median_Laktat_ab" ~ "Lactate, arterial blood (mmol/L)",
      Variable == "median_Troponin_T" ~ "Troponin T (ng/L)",
      Variable == "median_Laktat_vb" ~ "Lactate, venous blood (mmol/L)",
      TRUE ~ Variable
    )
  ) %>%
  select(Variable_Label, Missing_Count, Missing_Percentage, Total_N) %>%
  gt() %>%
  tab_header(
    title = "Missing Data Summary",
    subtitle = "Absolute numbers and percentages of missing values by variable"
  ) %>%
  cols_label(
    Variable_Label = "Variable",
    Missing_Count = "Missing (N)",
    Missing_Percentage = "Missing (%)",
    Total_N = "Total N"
  ) %>%
  fmt_number(
    columns = Missing_Percentage,
    decimals = 1,
    dec_mark = "·",
    sep_mark = " "
  ) %>%
  fmt_number(
    columns = c(Missing_Count, Total_N),
    decimals = 0,
    sep_mark = " "
  ) %>%
  cols_align(
    align = "center",
    columns = c(Missing_Count, Missing_Percentage, Total_N)
  ) %>%
  cols_align(align = "left", columns = Variable_Label)

# Display the table

missingness_table

gtsave(missingness_table, "missingness_table.docx")


#################################################

#### FTE Analysis

mults <- read_parquet(
  "/home/alex/ews/NEWS2_Evaluation/full_ews_18m_imp.parquet"
)


# Annual Assessment Analysis and Time Savings Calculation
# Exact period: October 16, 2018 to October 16, 2023 (5 years)

# EXPLORATORY ANALYSIS: Annual breakdown by calendar year for trend visualization
annual_breakdown <- mults %>%
  mutate(year = year(recorded_time)) %>%
  count(year, name = "assessments_per_year") %>%
  arrange(year)

annual_breakdown

# Calendar year summary statistics (for exploratory analysis)
summary_stats_calendar <- annual_breakdown %>%
  summarise(
    total_years = n(),
    total_assessments = sum(assessments_per_year),
    mean_annual_assessments = mean(assessments_per_year),
    median_annual_assessments = median(assessments_per_year),
    min_annual = min(assessments_per_year),
    max_annual = max(assessments_per_year),
    .groups = 'drop'
  )

summary_stats_calendar

# Visualization of annual trends
annual_plot <- annual_breakdown %>%
  ggplot(aes(x = year, y = assessments_per_year)) +
  geom_col(fill = "steelblue", alpha = 0.7) +
  geom_text(
    aes(label = format(assessments_per_year, big.mark = ",")),
    vjust = -0.5,
    size = 3.5
  ) +
  labs(
    title = "Annual EWS Assessments",
    subtitle = paste(
      "Total:",
      format(summary_stats_calendar$total_assessments, big.mark = ","),
      "assessments over",
      summary_stats_calendar$total_years,
      "calendar years"
    ),
    x = "Year",
    y = "Number of EWS Assessments",
    caption = "Note: 2018 and 2023 are partial years (Oct 16 - Oct 16)"
  ) +
  scale_x_continuous(n.breaks = 6) +
  scale_y_continuous(labels = scales::comma_format()) +
  theme_minimal()

annual_plot

# MAIN ANALYSIS: Exact 5-year period calculation
total_assessments <- nrow(mults)
exact_years <- 5 # Exactly 5 years from Oct 16, 2018 to Oct 16, 2023
mean_annual_assessments <- total_assessments / exact_years

# Summary statistics for main analysis
summary_stats <- tibble(
  total_years = exact_years,
  total_assessments = total_assessments,
  mean_annual_assessments = mean_annual_assessments,
  data_start = "2018-10-16",
  data_end = "2023-10-16"
)

print("=== MAIN ANALYSIS SUMMARY STATISTICS ===")
print(paste("Exact time period: October 16, 2018 to October 16, 2023"))
print(paste("Total assessments:", format(total_assessments, big.mark = ",")))
print(paste("Exact years covered:", exact_years))
print(paste(
  "Annual assessments:",
  format(round(mean_annual_assessments, 0), big.mark = ",")
))
print(paste(
  "Calendar year average:",
  format(
    round(summary_stats_calendar$mean_annual_assessments, 0),
    big.mark = ","
  )
))
print(paste(
  "Difference:",
  format(
    round(
      mean_annual_assessments - summary_stats_calendar$mean_annual_assessments,
      0
    ),
    big.mark = ","
  )
))

# Time and monetary savings calculation
time_saved_per_assessment <- 3 # minutes (conservative estimate)
annual_cost_per_fte_dkk <- 631601 # DKK per year
dkk_to_eur_rate <- 0.134 # Current exchange rate: 1 DKK = 0.134 EUR
annual_cost_per_fte_eur <- annual_cost_per_fte_dkk * dkk_to_eur_rate

annual_minutes_saved <- mean_annual_assessments * time_saved_per_assessment
annual_hours_saved <- annual_minutes_saved / 60
danish_FTE_saved <- annual_hours_saved / 1924 # Danish 37h/week
annual_money_saved_eur <- danish_FTE_saved * annual_cost_per_fte_eur

print("=== TIME AND MONETARY SAVINGS ANALYSIS ===")
print(paste(
  "Minutes saved annually:",
  format(round(annual_minutes_saved, 0), big.mark = ",")
))
print(paste(
  "Hours saved annually:",
  format(round(annual_hours_saved, 0), big.mark = ",")
))
print(paste("Danish FTE saved annually:", round(danish_FTE_saved, 1)))
print(paste(
  "Money saved annually:",
  format(round(annual_money_saved_eur, 0), big.mark = ","),
  "EUR"
))
print(paste(
  "Money saved annually:",
  format(round(annual_money_saved_eur / 1000000, 1), big.mark = ","),
  "million EUR"
))

# Hospital-specific analysis
hospital_time_savings <- mults %>%
  count(Hospital, name = "total_assessments_hospital") %>%
  mutate(
    # Annual assessments per hospital over exact 5-year period
    mean_annual_assessments = total_assessments_hospital / exact_years,

    # Annual time savings calculations
    annual_minutes_saved = mean_annual_assessments * time_saved_per_assessment,
    annual_hours_saved = annual_minutes_saved / 60,

    # Danish FTE savings (37h/week = 1924h/year)
    danish_FTE_saved = annual_hours_saved / 1924,

    # Monetary savings in EUR
    annual_money_saved_eur = danish_FTE_saved * annual_cost_per_fte_eur,
    annual_money_saved_million = annual_money_saved_eur / 1000000,

    # Flag as individual hospital
    category = "Individual Hospital"
  ) %>%
  arrange(desc(danish_FTE_saved))

# System-wide totals
system_totals <- hospital_time_savings %>%
  summarise(
    total_hospitals = n(),
    total_annual_assessments = sum(mean_annual_assessments),
    total_annual_hours_saved = sum(annual_hours_saved),
    total_FTE_saved = sum(danish_FTE_saved),
    total_money_saved_eur = sum(annual_money_saved_eur),
    total_money_saved_million = total_money_saved_eur / 1000000
  )

print("=== SYSTEM-WIDE SUMMARY ===")
print(paste("Total hospitals:", system_totals$total_hospitals))
print(paste(
  "Total annual assessments:",
  format(round(system_totals$total_annual_assessments, 0), big.mark = ",")
))
print(paste("Total FTE saved:", round(system_totals$total_FTE_saved, 1)))
print(paste(
  "Total money saved:",
  format(round(system_totals$total_money_saved_eur, 0), big.mark = ","),
  "EUR"
))
print(paste(
  "Total money saved:",
  round(system_totals$total_money_saved_million, 1),
  "million EUR"
))

# Prepare data for visualization
plot_data <- hospital_time_savings %>%
  select(
    Hospital,
    mean_annual_assessments,
    danish_FTE_saved,
    annual_money_saved_million,
    category
  ) %>%
  bind_rows(
    tibble(
      Hospital = "Regional Hospital System",
      mean_annual_assessments = system_totals$total_annual_assessments,
      danish_FTE_saved = system_totals$total_FTE_saved,
      annual_money_saved_million = system_totals$total_money_saved_million,
      category = "System Total"
    )
  ) %>%
  mutate(
    # Create display labels
    Hospital_display = case_when(
      Hospital == "Regional Hospital System" ~ "Regional Hospital System",
      TRUE ~ str_wrap(Hospital, 30)
    ),

    # Create labels for the bars
    bar_label = case_when(
      Hospital == "Regional Hospital System" ~
        paste0(
          round(danish_FTE_saved, 1),
          " FTE\n",
          round(annual_money_saved_million, 1),
          "M EUR"
        ),
      TRUE ~
        paste0(
          round(danish_FTE_saved, 1),
          " FTE\n",
          round(annual_money_saved_million, 2),
          "M EUR"
        )
    ),

    # Order hospitals by FTE savings, but put Regional Hospital System at the top
    Hospital_ordered = case_when(
      Hospital == "Regional Hospital System" ~ paste0("0_", Hospital),
      TRUE ~ paste0("1_", Hospital)
    )
  ) %>%
  arrange(desc(Hospital_ordered), desc(danish_FTE_saved)) %>%
  mutate(
    Hospital_display = factor(Hospital_display, levels = Hospital_display)
  )

# Lancet-style formatting function for consistent number display
format_lancet <- function(x, digits = 0) {
  format(
    round(x, digits),
    big.mark = " ",
    decimal.mark = "·",
    scientific = FALSE
  )
}

# Clean hospital labels and reorder by FTE saved (descending)
plot_data_clean <- plot_data %>%
  mutate(
    Hospital_display = case_when(
      str_detect(Hospital_display, "HGH.*Herlev.*Gentofte") ~
        "Herlev and Gentofte Hospital",
      str_detect(Hospital_display, "NOH.*North Zealand") ~
        "Hospital of North Zealand",
      TRUE ~ str_replace_all(Hospital_display, "Sygehus", "Hospital")
    )
  ) %>%
  arrange(danish_FTE_saved) %>%
  mutate(Hospital_display = factor(Hospital_display, levels = Hospital_display))

combined_plot <- plot_data_clean %>%
  ggplot(aes(x = Hospital_display, y = danish_FTE_saved, fill = category)) +
  geom_col(alpha = 0.8, width = 0.7) +
  geom_text(
    aes(label = str_replace_all(bar_label, "\\.", "·")),
    hjust = -0.05,
    size = 3.2,
    fontface = "bold",
    color = "black"
  ) +
  geom_hline(yintercept = 0, color = "black", linewidth = 0.5) +
  geom_hline(
    aes(yintercept = ifelse(Hospital == "Regional Hospital System", -0.5, NA)),
    color = "gray60",
    linewidth = 0.8,
    alpha = 0.7
  ) +
  coord_flip() +
  scale_fill_manual(
    values = c("Individual Hospital" = "#A23B72", "System Total" = "#2E86AB"),
    guide = "none"
  ) +
  labs(
    x = NULL,
    y = "FTE Saved Annually"
  ) +
  theme_minimal(base_size = 14) +
  theme(
    plot.title = element_text(
      size = 16,
      face = "bold",
      margin = margin(b = 10)
    ),
    plot.subtitle = element_text(
      size = 13,
      color = "gray40",
      margin = margin(b = 15)
    ),
    axis.text.y = element_text(size = 12, color = "gray20"),
    axis.text.x = element_text(size = 11, color = "gray20"),
    axis.title.x = element_text(
      size = 12,
      color = "gray20",
      margin = margin(t = 10)
    ),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_line(color = "gray90", linewidth = 0.3),
    plot.margin = margin(15, 15, 15, 15)
  ) +
  scale_y_continuous(
    expand = expansion(mult = c(0, 0.15)),
    breaks = scales::pretty_breaks(n = 6),
    labels = function(x) format_lancet(x, digits = 1)
  )

combined_plot

# Using the pmsampsize package to showcase the sample size requirements

sample_size_cstat <- pmsampsize(
  type = "b",
  cstatistic = 0.8,
  parameters = 100,
  prevalence = 0.0022
)

print(sample_size_cstat)
