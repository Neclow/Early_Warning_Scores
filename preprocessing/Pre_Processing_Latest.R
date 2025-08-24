# Load some libraries

library(tidyverse)
library(arrow)
library(tidylog)
library(tidymodels)

setwd("/home/alex/ews/NEWS2_Evaluation")

# Load the initial data provided

load("scoreDataEnriched.Rdata")


###############################################################
############### Start the initial pre-processing ##############
###############################################################

# Now order by ptID

scoreData <- scoreData |> arrange(ptID)

# Now we need to make further modifications

VITAL_BOUNDS <- list(
  Resp_frekvens = data.frame(lower = 6, upper = 60),
  Saturation = data.frame(lower = 60, upper = 100),
  Blood_Pressure.Sys = data.frame(lower = 30, upper = 305),
  Blood_Pressure.Dia = data.frame(lower = 20, upper = 180),
  Puls = data.frame(lower = 30, upper = 250),
  Temperature = data.frame(lower = 29.5, upper = 41.5),
  Ilttilskud = data.frame(lower = 0, upper = 60)
)

# Step 1: Check and replace character columns with numeric versions
cat("Step 1: Checking and replacing character columns with numeric versions\n")
num_cols <- names(scoreData)[str_detect(names(scoreData), "\\.Num$")]
for (col in num_cols) {
  base_col <- str_remove(col, "\\.Num$")
  if (base_col %in% names(scoreData)) {
    are_equal <- identical(
      suppressWarnings(as.numeric(scoreData[[base_col]])),
      scoreData[[col]]
    )
    cat(paste0(base_col, " == ", col, "? ", are_equal, "\n"))
  }
}

df <- scoreData
for (col in num_cols) {
  base_col <- str_remove(col, "\\.Num$")
  if (base_col %in% names(df)) {
    df[[base_col]] <- df[[col]]
  }
}

cat(paste0("Rows after column replacement: ", nrow(df), "\n\n"))

# Step 2: Convert temperature from Fahrenheit to Celsius
cat("Step 2: Converting temperature from Fahrenheit to Celsius\n")
df <- df |>
  mutate(
    Temperature = if_else(
      !is.na(Temperature),
      (Temperature - 32) * 5 / 9,
      Temperature
    )
  )

cat(paste0("Rows after temperature conversion: ", nrow(df), "\n\n"))

# Step 3: Compute hospital spent time
cat("Step 3: Computing hospital spent time\n")
cat("Sample datetime formats:\n")
cat(
  "Hosp_admsn_time samples:",
  head(df$Hosp_admsn_time[!is.na(df$Hosp_admsn_time)], 3),
  "\n"
)
cat(
  "HOSP_DISCH_TIME samples:",
  head(df$HOSP_DISCH_TIME[!is.na(df$HOSP_DISCH_TIME)], 3),
  "\n\n"
)

df <- df |>
  mutate(
    Hosp_admsn_time = ymd_hms(Hosp_admsn_time),
    HOSP_DISCH_TIME = ymd_hms(HOSP_DISCH_TIME),
    Hosp_spent_time = HOSP_DISCH_TIME - Hosp_admsn_time
  )

cat(paste0("Rows after time computation: ", nrow(df), "\n\n"))

# Step 4: Filter out short hospital stays
cat("Step 4: Filtering out short hospital stays (≤15 minutes)\n")
rows_before <- nrow(df)
df <- df |>
  filter(as.numeric(Hosp_spent_time, units = "secs") > 15 * 60)

rows_after <- nrow(df)
cat(paste0(
  "Rows before: ",
  rows_before,
  ", after: ",
  rows_after,
  ", removed: ",
  rows_before - rows_after,
  "\n\n"
))

# Step 5: Filter out explicit unknown gender
cat("Step 5: Filtering out explicit unknown gender ('Ukendt')\n")
rows_before <- nrow(df)
df <- df |>
  filter(sex != "Ukendt" | is.na(sex))

rows_after <- nrow(df)
cat(paste0(
  "Rows before: ",
  rows_before,
  ", after: ",
  rows_after,
  ", removed: ",
  rows_before - rows_after,
  "\n\n"
))

# Step 6: Remove .Num columns
cat("Step 6: Removing .Num columns\n")
cols_before <- ncol(df)
df <- df |>
  select(-ends_with(".Num"))

cols_after <- ncol(df)
cat(paste0(
  "Columns before: ",
  cols_before,
  ", after: ",
  cols_after,
  ", removed: ",
  cols_before - cols_after,
  "\n\n"
))

# Step 7: Fill missing oxygen supplementation
cat("Step 7: Filling missing Ilttilskud with 0\n")
na_before <- sum(is.na(df$Ilttilskud))
df <- df |>
  mutate(Ilttilskud = replace_na(Ilttilskud, 0))

na_after <- sum(is.na(df$Ilttilskud))
cat(paste0(
  "Missing Ilttilskud before: ",
  na_before,
  ", after: ",
  na_after,
  "\n\n"
))

# Step 8: Set out-of-bounds vital signs to NA
cat("Step 8: Setting out-of-bounds vital signs to NA\n")
vital_cols <- c(
  "Puls",
  "Resp_frekvens",
  "Blood_Pressure.Sys",
  "Blood_Pressure.Dia",
  "Saturation",
  "Ilttilskud",
  "Temperature"
)

for (col in vital_cols) {
  if (col %in% names(df) && col %in% names(VITAL_BOUNDS)) {
    bounds <- VITAL_BOUNDS[[col]]
    oob_before <- sum(
      !is.na(df[[col]]) &
        (df[[col]] < bounds$lower | df[[col]] > bounds$upper)
    )

    df <- df |>
      mutate(
        !!col := if_else(
          !is.na(.data[[col]]) &
            (.data[[col]] < bounds$lower | .data[[col]] > bounds$upper),
          NA_real_,
          .data[[col]]
        )
      )

    if (oob_before > 0) {
      cat(paste0(col, ": ", oob_before, " values set to NA\n"))
    }
  }
}

cat(paste0("\nFinal dataset: ", nrow(df), " rows, ", ncol(df), " columns\n"))


# Renaming the variable

df <- df |>
  rename(PT_ID = ptID)


##############################################
########## Adding new data ###################
##############################################

# Now I want to add further data on the df

# First I will load the dataframe that has the full information on the new variables

full_pats <- read_parquet("PAT_CSN_Full.parquet") # That's the very initial dataset with more variables included

full_pats <- full_pats |>
  ename(PT_ID = PAT_ID)

full_pats <- full_pats |>
  rename(CSN = PAT_ENC_CSN_ID)

# Now arrange for PT_ID

full_pats <- full_pats |>
  arrange(PT_ID)

# Filter full_pats to include only the individuals in df

full_pats <- full_pats |>
  filter(PT_ID %in% df$PT_ID) # That removes 39,597 rows (<1%)


############### Merge the relevant information #####################

df1 <- df |>
  group_by(PT_ID) |>
  mutate(obs = row_number()) |>
  ungroup()

full_pats <- full_pats |>
  group_by(PT_ID) |>
  mutate(obs = row_number()) |>
  ungroup()

# Make the CSN character

full_pats <- full_pats |>
  mutate(CSN = as.character(CSN))

# Join

df1 <- df1 |>
  left_join(
    full_pats |>
      select(PT_ID, obs, CSN, hospital_area_id, AFSNIT_SPECIALE),
    by = c("PT_ID", "obs")
  ) |>
  select(-obs)

df <- df1

df <- df |>
  relocate(CSN, .after = PT_ID)

df <- df |>
  relocate(hospital_area_id, .after = CSN)


############################################
############ Done / Fix datetime ###########
############################################

df <- df |>
  mutate(across(
    where(~ inherits(.x, c("POSIXct", "POSIXlt"))),
    ~ as.POSIXct(
      format(.x, "%Y-%m-%d %H:%M:%S"),
      tz = "",
      format = "%Y-%m-%d %H:%M:%S"
    )
  ))


# Save this df

df |> write_parquet("full_ews_18m.parquet")


#####################################
##### Implementing hospitals ########
#####################################

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
      .default = NA
    )
  ) |>
  mutate_at(vars(Hospital), as.factor)


#################################################
###### Implementing the Department Name #########
#################################################

df <- df |>
  rename(Department_Name = AFSNIT_SPECIALE) |>
  mutate_at(vars(Department_Name), as.factor)

df <- df |>
  mutate(
    Department_Name = if_else(Department_Name == "NULL", NA, Department_Name)
  ) |>
  mutate_at(vars(Department_Name), as.factor)

df <- df |>
  mutate(
    Department_Name = Department_Name |>
      fct_recode(NULL = "NULL") |> # Convert "NULL" string to actual NA
      fct_drop()
  )


####################################################################
### Adding some further information on the recorded time ###########
####################################################################

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


###################################

# Now we need some general reshaping to make variables in English

# For Oxygen Supplement
df <- df |>
  mutate(Ilttilskud = if_else(Ilttilskud == 0, "Air", "Oxygen")) |>
  mutate_at(vars(Ilttilskud), as.factor) |>
  rename(Oxygen_Supplement = Ilttilskud)


# For Consciousness
df <- df |>
  mutate(
    Consciousness = case_when(
      Bevidsthed == "A" ~ "A",
      Bevidsthed == "P" ~ "VPU",
      Bevidsthed == "U" ~ "VPU",
      Bevidsthed == "V" ~ "VPU"
    )
  ) |>
  mutate_at(vars(Consciousness), as.factor) |>
  dplyr::select(-Bevidsthed)


df <- df |>
  select(-Bevidsthed.Fak)


# For the Sex variable
df <- df |>
  mutate(
    Sex = case_when(
      sex == "Mand" ~ "Male",
      sex == "Kvinde" ~ "Female",
      .default = NA
    )
  ) |>
  mutate_at(vars(Sex), as.factor) |>
  dplyr::select(-sex)


# For the Age variable

df <- df |>
  mutate(Age = if_else(age > 105, 105, age)) |>
  select(-age)


# Respiration Rate
df <- df |>
  rename(Respiration_Rate = Resp_frekvens)


# Pulse

df <- df |>
  rename(Pulse = Puls)


# Now create some variables measuring previous hospitalizations and a unique identifier

df <- df |>
  group_by(PT_ID) |>
  mutate(Previous_Hosp = cumsum(!duplicated(CSN)) - 1) |>
  ungroup()


df <- df |>
  group_by(PT_ID) |>
  mutate(
    Identifier = paste0(
      PT_ID,
      '_',
      cumsum(CSN != lag(CSN, default = first(CSN)))
    )
  ) |>
  ungroup() |>
  relocate(Identifier, .after = PT_ID)

df <- df |>
  relocate(CSN, .after = Identifier)


# Create a factor variable on mortality 24 hours

df <- df |>
  mutate(Status24H = if_else(mort24H == 0, "Alive", "Deceased")) |>
  mutate_at(vars(Status24H), as.factor)

# Save this df (That's the full unimputed dataset with multiple measurements per individual)

df |> write_parquet("full_ews_18m.parquet")


################################################################################################################################
###### We now need to impute the missings with the median for continuous variables and the mode for the categorical ones #######
################################################################################################################################

df_imputed <- df

data_recipe <- recipe(~., data = df_imputed) |>
  # Select specific predictors
  step_select(
    PT_ID,
    Identifier,
    CSN,
    hospital_area_id,
    Hosp_admsn_time,
    HOSP_DISCH_TIME,
    recorded_time,
    Pulse,
    Respiration_Rate,
    Temperature,
    Saturation,
    Oxygen_Supplement,
    deathDate,
    endDate,
    followUpDays,
    ADT_DEPARTMENT_ID,
    mort24H,
    mort48H,
    mort7D,
    mort30D,
    Blood_Pressure.Sys,
    Blood_Pressure.Dia,
    Hosp_spent_time,
    Department_Name,
    Hospital,
    day_type,
    time_of_day,
    month,
    Consciousness,
    Sex,
    Age,
    Previous_Hosp,
    Status24H
  ) |>

  # Impute categorical variables with mode
  step_impute_mode(all_nominal_predictors()) |>

  # Impute numeric variables with median
  step_impute_median(all_numeric_predictors())

# Prep and bake

data_recipe_prepped <- prep(data_recipe)

df_imputed <- bake(data_recipe_prepped, new_data = df_imputed)


#############

# Create the Age Group variable

df_imputed <- df_imputed |>
  mutate(
    Age_Group = case_when(
      Age >= 18 & Age < 66 ~ "18-65", # 18 up to (but not including) 66
      Age >= 66 & Age <= 80 ~ "66-80", # 66 to 80 (including 80)
      Age > 80 ~ "80+", # Over 80
      TRUE ~ NA_character_
    ) |>
      factor(levels = c("18-65", "66-80", "80+"))
  )


##################

# Now create the scores

# NEWS Score

df_imputed <- df_imputed |>
  mutate(
    Temperature_Rounded = round(Temperature, 1),

    Respiration_Score = case_when(
      Respiration_Rate <= 8 ~ 3,
      Respiration_Rate >= 9 & Respiration_Rate <= 11 ~ 1,
      Respiration_Rate >= 12 & Respiration_Rate <= 20 ~ 0,
      Respiration_Rate >= 21 & Respiration_Rate <= 24 ~ 2,
      Respiration_Rate >= 25 ~ 3
    ),
    Saturation_Score = case_when(
      Saturation <= 91 ~ 3,
      Saturation >= 92 & Saturation <= 93 ~ 2,
      Saturation >= 94 & Saturation <= 95 ~ 1,
      Saturation >= 96 ~ 0
    ),
    Oxygen_Supplement_Score = case_when(
      Oxygen_Supplement == "Oxygen" ~ 2,
      Oxygen_Supplement == "Air" ~ 0
    ),
    Temperature_Score = case_when(
      Temperature_Rounded <= 35.0 ~ 3,
      Temperature_Rounded >= 35.1 & Temperature_Rounded <= 36.0 ~ 1,
      Temperature_Rounded >= 36.1 & Temperature_Rounded <= 38.0 ~ 0,
      Temperature_Rounded >= 38.1 & Temperature_Rounded <= 39.0 ~ 1,
      Temperature_Rounded >= 39.1 ~ 2
    ),
    Blood_Pressure_Score = case_when(
      Blood_Pressure.Sys <= 90 ~ 3,
      Blood_Pressure.Sys >= 91 & Blood_Pressure.Sys <= 100 ~ 2,
      Blood_Pressure.Sys >= 101 & Blood_Pressure.Sys <= 110 ~ 1,
      Blood_Pressure.Sys >= 111 & Blood_Pressure.Sys <= 219 ~ 0,
      Blood_Pressure.Sys >= 220 ~ 3
    ),
    Pulse_Score = case_when(
      Pulse <= 40 ~ 3,
      Pulse >= 41 & Pulse <= 50 ~ 1,
      Pulse >= 51 & Pulse <= 90 ~ 0,
      Pulse >= 91 & Pulse <= 110 ~ 1,
      Pulse >= 111 & Pulse <= 130 ~ 2,
      Pulse >= 131 ~ 3
    ),
    Consciousness_Score = case_when(
      Consciousness == "A" ~ 0,
      Consciousness == "VPU" ~ 3
    ),
    Total_Score = Respiration_Score +
      Saturation_Score +
      Pulse_Score +
      Oxygen_Supplement_Score +
      Temperature_Score +
      Blood_Pressure_Score +
      Consciousness_Score
  ) |>
  select(
    -Temperature_Rounded,
    -Saturation_Score,
    -Respiration_Score,
    -Pulse_Score,
    -Oxygen_Supplement_Score,
    -Temperature_Score,
    -Blood_Pressure_Score,
    -Consciousness_Score
  )

df_imputed <- df_imputed |>
  mutate(EWS_score = Total_Score) |>
  select(-Total_Score)


# NEWS-Light

df_imputed <- df_imputed |>
  mutate(
    Respiration_Score = case_when(
      Respiration_Rate <= 8 ~ 3,
      Respiration_Rate >= 9 & Respiration_Rate <= 11 ~ 1,
      Respiration_Rate >= 12 & Respiration_Rate <= 20 ~ 0,
      Respiration_Rate >= 21 & Respiration_Rate <= 24 ~ 2,
      Respiration_Rate >= 25 ~ 3
    ),
    Saturation_Score = case_when(
      Saturation <= 91 ~ 3,
      Saturation >= 92 & Saturation <= 93 ~ 2,
      Saturation >= 94 & Saturation <= 95 ~ 1,
      Saturation >= 96 ~ 0
    ),
    Oxygen_Supplement_Score = case_when(
      Oxygen_Supplement == "Oxygen" ~ 2,
      Oxygen_Supplement == "Air" ~ 0
    ),
    Pulse_Score = case_when(
      Pulse <= 40 ~ 3,
      Pulse >= 41 & Pulse <= 50 ~ 1,
      Pulse >= 51 & Pulse <= 90 ~ 0,
      Pulse >= 91 & Pulse <= 110 ~ 1,
      Pulse >= 111 & Pulse <= 130 ~ 2,
      Pulse >= 131 ~ 3
    ),
    Consciousness_Score = case_when(
      Consciousness == "A" ~ 0,
      Consciousness == "VPU" ~ 3
    ),
    Total_Score = Respiration_Score +
      Saturation_Score +
      Pulse_Score +
      Oxygen_Supplement_Score +
      Consciousness_Score
  ) |>
  select(
    -Saturation_Score,
    -Respiration_Score,
    -Pulse_Score,
    -Oxygen_Supplement_Score,
    -Consciousness_Score
  )

df_imputed <- df_imputed |>
  mutate(EWS_light = Total_Score) |>
  select(-Total_Score)


##### DEWS (NEWS-Light with age and sex)

df_imputed <- df_imputed |>
  mutate(
    Respiration_Score = case_when(
      Respiration_Rate >= 0 & Respiration_Rate <= 20 ~ 0,
      Respiration_Rate >= 21 & Respiration_Rate <= 24 ~ 2,
      Respiration_Rate >= 25 ~ 3
    ),
    Saturation_Score = case_when(
      Saturation <= 91 ~ 3,
      Saturation >= 92 & Saturation <= 95 ~ 1,
      Saturation >= 96 ~ 0
    ),
    Oxygen_Supplement_Score = case_when(
      Oxygen_Supplement == "Oxygen" ~ 1,
      Oxygen_Supplement == "Air" ~ 0
    ),
    Pulse_Score = case_when(
      Pulse <= 50 ~ 1,
      Pulse >= 51 & Pulse <= 90 ~ 0,
      Pulse >= 91 & Pulse <= 110 ~ 1,
      Pulse >= 111 ~ 2
    ),
    Consciousness_Score = case_when(
      Consciousness == "A" ~ 0,
      Consciousness == "VPU" ~ 5
    ),
    Sex_Score = if_else(
      Sex == "Male",
      1,
      0
    ),
    Age_Score = case_when(
      Age < 41 ~ 0,
      Age >= 41 & Age < 51 ~ 1,
      Age >= 51 & Age < 61 ~ 2,
      Age >= 61 & Age < 66 ~ 3,
      Age >= 66 & Age < 76 ~ 4,
      Age >= 76 & Age < 81 ~ 5,
      Age >= 81 & Age < 91 ~ 6,
      Age >= 91 ~ 7
    ),
    Total_IScore = Respiration_Score +
      Saturation_Score +
      Pulse_Score +
      Oxygen_Supplement_Score +
      Consciousness_Score +
      Sex_Score +
      Age_Score
  ) |>
  select(
    -Saturation_Score,
    -Respiration_Score,
    -Pulse_Score,
    -Oxygen_Supplement_Score,
    -Consciousness_Score,
    -Sex_Score,
    -Age_Score
  )

df_imputed <- df_imputed |>
  mutate(IEWS_Light = Total_IScore) |>
  select(-Total_IScore)


# Create a clinical risk group out of the NEWS2 scoring system

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
        (Blood_Pressure.Sys <= 90 | Blood_Pressure.Sys >= 220) ~
        "Low-Medium",
      EWS_score >= 0 & EWS_score <= 4 ~ "Low"
    )
  ) |>
  mutate(Risk_Groups_EWS = as.factor(Risk_Groups_EWS))


# Save this dataframe now

df_imputed |> write_parquet("full_ews_18m_imp.parquet") # That's the big dataset including all the measurements per individual and imputed


########## Now let's create two more dataframes ################

# 1. We will create the imputed dataframe which keeps only the first EWS measurement per hospitalization and per patient

df_imputed_first <- df_imputed |>
  group_by(PT_ID, CSN) |>
  arrange(recorded_time) |>
  slice(1) |>
  ungroup()

length(unique(df_imputed_first$PT_ID)) # 898662 individuals


df_imputed_first <- df_imputed_first |>
  arrange(Identifier)


# Save this dataframe now (it's going to be used for the further analysis)

df_imputed_first |> write_parquet("single_ews_imp.parquet")


# 2. We will create the unimputed dataframe which keeps only the first EWS measurement per hospitalization and per patient

df_first <- df |>
  group_by(PT_ID, CSN) |>
  arrange(recorded_time) |>
  slice(1) |>
  ungroup()

length(unique(df_first$PT_ID)) # 898662 individuals


df_first <- df_first |>
  arrange(Identifier)


# Save this dataframe now (it's going to be used for the further analysis)

df_first |> write_parquet("single_ews.parquet")
