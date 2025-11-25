# Load required libraries

library(tidyverse)
library(arrow)
library(tidylog)
library(tidymodels)

# Set the working directory

setwd("/home/alex/ews/NEWS2_Evaluation")

# Load the initial data provided from the EHR system

load("scoreDataEnriched.Rdata")

###############################################################
############### Start the initial pre-processing ##############
###############################################################

# Now order by ptID (Patient Identifier)

scoreData <- scoreData |>
  arrange(ptID) |>
  rename(PT_ID = ptID)

n_distinct(scoreData$PT_ID) # 899,100 unique patients

##############################################
########## Adding CSN data FIRST #############
##############################################

# Load the dataframe that has the full information on CSNs
full_pats <- read_parquet("PAT_CSN_Full.parquet")

# Rename columns to match
full_pats <- full_pats |>
  rename(PT_ID = PAT_ID) |>
  rename(CSN = PAT_ENC_CSN_ID) |>
  arrange(PT_ID)

n_distinct(full_pats$PT_ID) # 905,012 unique Patients (That's the start)
n_distinct(full_pats$CSN) # 2,373,143 unique hospitalization contacts

# Filter full_pats to include only patients who have EWS measurements
# Removes patients without EWS (removes <1%, removed 39,073 rows, 18,991,889 rem)
full_pats <- full_pats |>
  filter(PT_ID %in% scoreData$PT_ID)

# Again let's check

n_distinct(full_pats$PT_ID) # 899,100 unique Patients with EWS measurements
n_distinct(full_pats$CSN) # 2,366,631 unique hospitalization contacts

# How Many did we discard after we kept only those with an EWS measurement

# We discarded 5912 unique patients without an EWS measurement (905,012 - 899,100)

# We also discarded 6,512 unique hospital contacts (2,373,143 - 2,366,631)

# Create observation numbers for merging
df <- scoreData |>
  group_by(PT_ID) |>
  mutate(obs = row_number()) |>
  ungroup()

full_pats <- full_pats |>
  group_by(PT_ID) |>
  mutate(obs = row_number()) |>
  ungroup()

# Make the CSN character and join
full_pats <- full_pats |>
  mutate(CSN = as.character(CSN))

# Merge CSN information
df <- df |>
  left_join(
    full_pats |> select(PT_ID, obs, CSN, hospital_area_id, AFSNIT_SPECIALE),
    by = c("PT_ID", "obs")
  ) |>
  select(-obs)

# Relocate new columns
df <- df |>
  relocate(CSN, .after = PT_ID) |>
  relocate(hospital_area_id, .after = CSN)

# Check how many individuals and encounters I have

length(unique(df$PT_ID)) # 899,100 individuals
length(unique(df$CSN)) # 2,355,905 unique encounters

# So we further excluded 10,726 (2,366,631 - 2,355,905 ) hospital contacts without EWS measurement
# Total assessments of EWS is 18,877,954

df <- df |>
  mutate(
    Hosp_admsn_time = ymd_hms(Hosp_admsn_time),
    HOSP_DISCH_TIME = ymd_hms(HOSP_DISCH_TIME),
    Hosp_spent_time = HOSP_DISCH_TIME - Hosp_admsn_time
  )

# Check

length(unique(df$PT_ID)) # 899,100 unique patients
length(unique(df$CSN)) # 2,355,905 unique encounters

# Filter out short hospital stays

df <- df |>
  filter(as.numeric(Hosp_spent_time, units = "secs") > 15 * 60)

# 898,692 unique patients (899,100 - 898,692 = 408 patients excluded)
length(unique(df$PT_ID))
# 2,351,861 unique encounters (2,355,905 - 2,351,861 = 4044 encounters excluded)
length(unique(df$CSN))

# Filter missing demographics

df <- df |>
  filter(sex != "Ukendt" | is.na(sex))

# 898,662 unique patients (899,692 - 898,662 = 30 patients excluded)
length(unique(df$PT_ID))
# 2,351,828 unique encounters (2,351,861 - 2,351,828 = 33 encounters excluded)
length(unique(df$CSN))


# Let's now work with the columns

###############################################################
############### PHASE 2: CLEANING & STANDARDIZATION ###########
###############################################################

# 1. DATA TRANSFER: Use .Num columns to overwrite text columns
cat("Transferring numeric values from .Num columns...\n")

# Transfer numeric values from .Num columns
num_cols <- names(df)[str_detect(names(df), "\\.Num$")]

for (col in num_cols) {
  base_col <- str_remove(col, "\\.Num$")
  if (base_col %in% names(df)) df[[base_col]] <- df[[col]]
}
df <- df |> select(-ends_with(".Num"))

# Fill Missing Oxygen
df <- df |> mutate(Ilttilskud = replace_na(Ilttilskud, 0))

# Convert Temperature (Fahrenheit -> Celsius)
df <- df |>
  mutate(
    Temperature = if_else(
      !is.na(Temperature),
      (Temperature - 32) * 5 / 9,
      Temperature
    )
  )

# Apply Physiological Bounds
VITAL_BOUNDS <- list(
  Resp_frekvens = data.frame(lower = 6, upper = 60),
  Saturation = data.frame(lower = 60, upper = 100),
  Blood_Pressure.Sys = data.frame(lower = 30, upper = 305),
  Blood_Pressure.Dia = data.frame(lower = 20, upper = 180),
  Puls = data.frame(lower = 30, upper = 250),
  Temperature = data.frame(lower = 29.5, upper = 41.5),
  Ilttilskud = data.frame(lower = 0, upper = 60)
)

for (col in names(VITAL_BOUNDS)) {
  if (col %in% names(df)) {
    bounds <- VITAL_BOUNDS[[col]]
    df <- df |>
      mutate(
        !!col := if_else(
          !is.na(.data[[col]]) &
            (.data[[col]] < bounds$lower | .data[[col]] > bounds$upper),
          NA_real_,
          .data[[col]]
        )
      )
  }
}

# Translation & Feature Engineering
df <- df |>
  mutate(
    Hospital = case_when(
      hospital_area_id == "5" ~ "Amager and Hvidovre Hospital",
      hospital_area_id == "7" ~ "Bornholm's Hospital",
      hospital_area_id == "3" ~ "Bispebjerg and Frederiksberg Hospitals",
      hospital_area_id == "2" ~ "Rigshospitalet",
      hospital_area_id == "13" ~ "Nykøbing Sygehus",
      hospital_area_id == "14" ~ "Næstved, Slagelse and Ringsted Hospitals",
      hospital_area_id == "9" ~ "Zealands University Hospital",
      hospital_area_id == "1" ~ "HGH, Herlev and Gentofte Hospital",
      hospital_area_id == "6" ~ "NOH, Hospital of North Zealand",
      hospital_area_id == "12" ~ "Holbæk Sygehus",
      .default = NA_character_
    ),
    Department_Name = na_if(as.character(AFSNIT_SPECIALE), "NULL"),
    Oxygen_Supplement = if_else(Ilttilskud == 0, "Air", "Oxygen"),
    Consciousness = case_when(
      Bevidsthed == "A" ~ "A",
      Bevidsthed %in% c("P", "U", "V") ~ "VPU"
    ),
    Sex = case_when(sex == "Mand" ~ "Male", sex == "Kvinde" ~ "Female"),
    Age = if_else(age > 105, 105, age),
    Respiration_Rate = Resp_frekvens,
    Pulse = Puls,
    Status24H = if_else(mort24H == 0, "Alive", "Deceased"),

    day_type = if_else(wday(recorded_time) %in% 2:6, "Weekday", "Weekend"),
    time_of_day = case_when(
      hour(recorded_time) >= 6 & hour(recorded_time) < 12 ~ "Morning",
      hour(recorded_time) >= 12 &
        hour(recorded_time) < 22 ~ "Afternoon/Evening",
      TRUE ~ "Night"
    ),
    month = month(recorded_time, label = TRUE, abbr = TRUE)
  ) |>
  select(
    -Ilttilskud,
    -Bevidsthed,
    -any_of("Bevidsthed.Fak"),
    -sex,
    -age,
    -Resp_frekvens,
    -Puls
  )

# Create History Variables (Previous Hospitalization)
df <- df |>
  group_by(PT_ID) |>
  mutate(
    Previous_Hosp = cumsum(!duplicated(CSN)) - 1,
    Identifier = paste0(
      PT_ID,
      "_",
      cumsum(CSN != lag(CSN, default = first(CSN)))
    )
  ) |>
  ungroup()

df <- df |>
  relocate(CSN, .after = Identifier)

# Check the details of df

length(unique(df$PT_ID)) # 898,662 unique patients
length(unique(df$CSN)) # 2,351,828 unique hospital contacts
# 18,865,713 assessments

df |> write_parquet("full_ews_cleaned_v2.parquet")

# Save the single unimputed one

df_first <- df |>
  group_by(PT_ID, CSN) |>
  arrange(recorded_time) |>
  slice(1) |>
  ungroup() |>
  arrange(Identifier)

write_parquet(df_first, "single_ews_v2.parquet")

# IMPUTATION #

numeric_vars <- c(
  "Pulse",
  "Respiration_Rate",
  "Temperature",
  "Saturation",
  "Blood_Pressure.Sys",
  "Blood_Pressure.Dia",
  "Hosp_spent_time",
  "Age",
  "Previous_Hosp"
)

categorical_vars <- c(
  "Oxygen_Supplement",
  "Consciousness",
  "Sex",
  "Hospital",
  "Department_Name",
  "day_type",
  "time_of_day",
  "month"
)

get_mode <- function(v) {
  uniqv <- unique(na.omit(v))
  if (length(uniqv) == 0) {
    return(NA)
  }
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

df_imputed <- df

# Impute Numeric (Median)
df_imputed <- df_imputed |>
  mutate(across(
    all_of(numeric_vars),
    ~ ifelse(is.na(.), median(as.numeric(.), na.rm = TRUE), .)
  ))

# Impute Categorical (Mode)
df_imputed <- df_imputed |>
  mutate(across(all_of(categorical_vars), ~ ifelse(is.na(.), get_mode(.), .)))


#### SCORING #####

df_imputed <- df_imputed |>
  mutate(
    Age_Group = case_when(
      Age >= 18 & Age < 66 ~ "18-65",
      Age >= 66 & Age <= 80 ~ "66-80",
      Age > 80 ~ "80+"
    ) |>
      factor(levels = c("18-65", "66-80", "80+")),

    # NEWS Components
    Respiration_Score = case_when(
      Respiration_Rate <= 8 ~ 3,
      Respiration_Rate <= 11 ~ 1,
      Respiration_Rate <= 20 ~ 0,
      Respiration_Rate <= 24 ~ 2,
      TRUE ~ 3
    ),
    Saturation_Score = case_when(
      Saturation <= 91 ~ 3,
      Saturation <= 93 ~ 2,
      Saturation <= 95 ~ 1,
      TRUE ~ 0
    ),
    Oxygen_Score = if_else(Oxygen_Supplement == "Oxygen", 2, 0),
    Temp_Score = case_when(
      round(Temperature, 1) <= 35.0 ~ 3,
      round(Temperature, 1) <= 36.0 ~ 1,
      round(Temperature, 1) <= 38.0 ~ 0,
      round(Temperature, 1) <= 39.0 ~ 1,
      TRUE ~ 2
    ),
    BP_Score = case_when(
      Blood_Pressure.Sys <= 90 ~ 3,
      Blood_Pressure.Sys <= 100 ~ 2,
      Blood_Pressure.Sys <= 110 ~ 1,
      Blood_Pressure.Sys <= 219 ~ 0,
      TRUE ~ 3
    ),
    Pulse_Score = case_when(
      Pulse <= 40 ~ 3,
      Pulse <= 50 ~ 1,
      Pulse <= 90 ~ 0,
      Pulse <= 110 ~ 1,
      Pulse <= 130 ~ 2,
      TRUE ~ 3
    ),
    Consciousness_Score = if_else(Consciousness == "A", 0, 3),

    # Totals
    EWS_score = Respiration_Score +
      Saturation_Score +
      Pulse_Score +
      Oxygen_Score +
      Temp_Score +
      BP_Score +
      Consciousness_Score,
    EWS_light = Respiration_Score +
      Saturation_Score +
      Pulse_Score +
      Oxygen_Score +
      Consciousness_Score
  ) |>
  select(
    -Respiration_Score,
    -Saturation_Score,
    -Oxygen_Score,
    -Temp_Score,
    -BP_Score,
    -Pulse_Score,
    -Consciousness_Score
  )

# DEWS / IEWS Logic
df_imputed <- df_imputed |>
  mutate(
    IEWS_Light = case_when(
      Respiration_Rate <= 20 ~ 0,
      Respiration_Rate <= 24 ~ 2,
      TRUE ~ 3
    ) +
      case_when(Saturation <= 91 ~ 3, Saturation <= 95 ~ 1, TRUE ~ 0) +
      if_else(Oxygen_Supplement == "Oxygen", 1, 0) +
      case_when(Pulse <= 50 ~ 1, Pulse <= 90 ~ 0, Pulse <= 110 ~ 1, TRUE ~ 2) +
      if_else(Consciousness == "A", 0, 5) +
      if_else(Sex == "Male", 1, 0) +
      case_when(
        Age <= 40 ~ 0,
        Age <= 50 ~ 1,
        Age <= 60 ~ 2,
        Age <= 65 ~ 3,
        Age <= 75 ~ 4,
        Age <= 80 ~ 5,
        Age <= 90 ~ 6,
        TRUE ~ 7
      )
  )

# Risk Groups (Escalation Thresholds)
df_imputed <- df_imputed |>
  mutate(
    Risk_Groups_EWS = case_when(
      EWS_score >= 7 ~ "High",
      EWS_score >= 5 & EWS_score <= 6 ~ "Medium",
      (Respiration_Rate <= 8 | Respiration_Rate >= 25) |
        (Saturation <= 91) |
        (Pulse <= 40 | Pulse >= 131) |
        (Consciousness == "VPU") |
        (Temperature <= 35) |
        (Blood_Pressure.Sys <= 90 | Blood_Pressure.Sys >= 220) ~ "Low-Medium",
      EWS_score >= 0 & EWS_score <= 4 ~ "Low"
    ) |>
      as.factor()
  )

write_parquet(df_imputed, "full_ews_18m_imp_v2.parquet")

length(unique(df_imputed$PT_ID)) # 898,662 unique patients
length(unique(df_imputed$CSN)) # 2,351,828 unique encounters
dim(df_imputed) # 18,865,713 total assessments

# EXPORT #

final_cols <- c(
  "PT_ID",
  "Identifier",
  "CSN",
  "hospital_area_id",
  "Hosp_admsn_time",
  "HOSP_DISCH_TIME",
  "recorded_time",
  "deathDate",
  "endDate",
  "followUpDays",
  "ADT_DEPARTMENT_ID",
  "mort24H",
  "mort48H",
  "mort7D",
  "mort30D",
  "Status24H",
  numeric_vars,
  categorical_vars,
  "Age_Group",
  "EWS_score",
  "EWS_light",
  "IEWS_Light",
  "Risk_Groups_EWS"
)

df_imputed_first <- df_imputed |>
  select(any_of(final_cols)) |>
  group_by(PT_ID, CSN) |>
  arrange(recorded_time) |>
  slice(1) |>
  ungroup() |>
  arrange(Identifier)

write_parquet(df_imputed_first, "single_ews_imp_v2.parquet")

# For the one row per contact

# We have removed 16,513,885 rows (88%), 2,351,828 assessments remaining

length(unique(df_imputed_first$PT_ID)) # 898,662 unique patients

length(unique(df_imputed_first$CSN)) # 2,351,828 unique contacts

# Latency Period

# Calculate Latency
# Logic: Current Admission Time - Previous Discharge Time
# We group by PT_ID and ensure correct sorting by time.

df_latency <- df_imputed_first |>
  select(PT_ID, CSN, Hosp_admsn_time, HOSP_DISCH_TIME) |>
  arrange(PT_ID, Hosp_admsn_time) |>
  group_by(PT_ID) |>
  mutate(
    # Get the discharge time of the PREVIOUS visit
    Prev_Disch_Time = lag(HOSP_DISCH_TIME),

    # Calculate difference (Latency) in Days
    Latency_Days = as.numeric(difftime(
      Hosp_admsn_time,
      Prev_Disch_Time,
      units = "days"
    )),

    # Optional: Latency in Hours for finer granularity
    Latency_Hours = as.numeric(difftime(
      Hosp_admsn_time,
      Prev_Disch_Time,
      units = "hours"
    ))
  ) |>
  ungroup()

# 3. Summary Statistics
# We only look at rows where Latency is not NA (i.e., excluding the first visit)

latency_stats <- df_latency |>
  filter(!is.na(Latency_Days)) |>
  summarise(
    Median_Days = median(Latency_Days, na.rm = TRUE),
    IQR_Lower = quantile(Latency_Days, 0.25, na.rm = TRUE),
    IQR_Upper = quantile(Latency_Days, 0.75, na.rm = TRUE),

    # How many are "Rapid Readmissions" (< 24 hours)?
    Rapid_Readmits_N = sum(Latency_Hours < 24),
    Rapid_Readmits_Pct = mean(Latency_Hours < 24) * 100,

    # How many are "Short Term" (< 30 days)?
    Readmits_30d_Pct = mean(Latency_Days < 30) * 100
  )

# Print the results for your response letter
print(latency_stats)

# Filter out administrative overlaps (Negative Latency)
# We assume valid readmission must happen *after* discharge
clean_stats <- df_latency |>
  filter(Latency_Hours >= 0) |> # Remove the -2.58 days artifacts
  summarise(
    Median_Days = median(Latency_Days, na.rm = TRUE),
    IQR_Lower = quantile(Latency_Days, 0.25, na.rm = TRUE),
    IQR_Upper = quantile(Latency_Days, 0.75, na.rm = TRUE),
    Rapid_Readmits_Pct = mean(Latency_Hours < 24) * 100
  )

print(clean_stats)

clean_stats |> write_parquet("clean_latency_nov.parquet")

# Count the negative values
n_negative <- sum(df_latency$Latency_Hours < 0, na.rm = TRUE)
pct_negative <- mean(df_latency$Latency_Hours < 0, na.rm = TRUE) * 100

cat("Number of overlapping/negative records:", n_negative, "\n")
cat("Percentage of total records:", round(pct_negative, 2), "%\n")

# View the first few to confirm they are overlaps
negative_rows <- df_latency |>
  filter(Latency_Hours < 0) |>
  select(PT_ID, CSN, Prev_Disch_Time, Hosp_admsn_time, Latency_Hours)

print(head(negative_rows))

# Analyze the "Negative" (Overlap) group specifically
overlap_stats <- df_latency |>
  filter(Latency_Hours < 0) |>
  summarise(
    Median_Overlap_Hours = median(abs(Latency_Hours), na.rm = TRUE),
    p25_Overlap = quantile(abs(Latency_Hours), 0.25, na.rm = TRUE),
    p75_Overlap = quantile(abs(Latency_Hours), 0.75, na.rm = TRUE),

    # How many are overlapping by less than 24 hours? (Typical transfer window)
    Transfer_Rate = mean(abs(Latency_Hours) < 24) * 100
  )

print(overlap_stats)

# Get comprehensive statistics for supplement
latency_summary <- df_latency |>
  filter(!is.na(Latency_Days)) |>
  summarise(
    Total_with_prior = n(),

    # Overlapping encounters
    N_overlapping = sum(Latency_Hours < 0),
    Pct_overlapping = mean(Latency_Hours < 0) * 100,

    # Same-day (0 to <24 hours)
    N_sameday = sum(Latency_Hours >= 0 & Latency_Hours < 24),
    Pct_sameday = mean(Latency_Hours >= 0 & Latency_Hours < 24) * 100,

    # Combined overlapping + same-day
    N_overlap_or_sameday = sum(Latency_Hours < 24),
    Pct_overlap_or_sameday = mean(Latency_Hours < 24) * 100,

    # Short-term readmissions (<30 days)
    Pct_within_30days = mean(Latency_Days < 30) * 100,

    # For non-overlapping encounters only
    Median_Days_valid = median(Latency_Days[Latency_Hours >= 0], na.rm = TRUE),
    IQR_Lower_valid = quantile(
      Latency_Days[Latency_Hours >= 0],
      0.25,
      na.rm = TRUE
    ),
    IQR_Upper_valid = quantile(
      Latency_Days[Latency_Hours >= 0],
      0.75,
      na.rm = TRUE
    )
  )

print(latency_summary)

# Also useful: distribution breakdown
latency_distribution <- df_latency |>
  filter(!is.na(Latency_Days)) |>
  mutate(
    Latency_Category = case_when(
      Latency_Hours < 0 ~ "Overlapping",
      Latency_Hours < 24 ~ "Same-day (0-24h)",
      Latency_Days < 7 ~ "Within 1 week",
      Latency_Days < 30 ~ "Within 30 days",
      Latency_Days < 90 ~ "Within 90 days",
      TRUE ~ "90+ days"
    )
  ) |>
  count(Latency_Category) |>
  mutate(Percentage = n / sum(n) * 100)

print(latency_distribution)


non_overlap_stats <- df_latency |>
  filter(!is.na(Latency_Days), Latency_Hours >= 0) |>
  summarise(
    N_non_overlapping = n(),
    Median_Days = median(Latency_Days, na.rm = TRUE),
    IQR_Lower = quantile(Latency_Days, 0.25, na.rm = TRUE),
    IQR_Upper = quantile(Latency_Days, 0.75, na.rm = TRUE)
  )

print(non_overlap_stats)


prev_hosp_distribution <- data |>
  group_by(Previous_Hosp) |>
  summarise(
    N = n(),
    Weighted_N = sum(weights_new),
    Pct = n() / nrow(data) * 100
  ) |>
  arrange(Previous_Hosp)

prev_hosp_distribution

# Summary stats
prev_hosp_summary <- data |>
  summarise(
    N_with_zero = sum(Previous_Hosp == 0),
    Pct_first_visit = mean(Previous_Hosp == 0) * 100,
    Pct_repeat_visit = mean(Previous_Hosp >= 1) * 100,
    Median = median(Previous_Hosp),
    Mean = mean(Previous_Hosp)
  )

print(prev_hosp_summary)
