# Set the working directory (not shown here)

#########################################
############# Packages ##################
#########################################

library(tidyverse) # For data analysis
library(arrow)

#######################################################


# Information
# 1. Age restricted to 105
# 2. NEWS2-Light and IEWS score accounting for age and sex implemented (see below)

# NEWS2-Light

# df <- df %>%
#   mutate(
#     Respiration_Score = case_when(
#       Respiration_Rate <= 8 ~ 3,
#       Respiration_Rate >= 9 & Respiration_Rate <= 11 ~ 1,
#       Respiration_Rate >= 12 & Respiration_Rate <= 20 ~ 0,
#       Respiration_Rate >= 21 & Respiration_Rate <= 24 ~ 2,
#       Respiration_Rate >= 25 ~ 3
#     ),
#     Saturation_Score = case_when(
#       Saturation <= 91 ~ 3,
#       Saturation >= 92 & Saturation <= 93 ~ 2,
#       Saturation >= 94 & Saturation <= 95 ~ 1,
#       Saturation >= 96 ~ 0,
#     ),
#     Oxygen_Supplement_Score = case_when(
#       Oxygen_Supplement == "Oxygen" ~ 2,
#       Oxygen_Supplement == "Air"  ~ 0,
#     ),
#     Pulse_Score = case_when(
#       Pulse <= 40 ~ 3,
#       Pulse >= 41 & Pulse <= 50 ~ 1,
#       Pulse >= 51 & Pulse <= 90 ~ 0,
#       Pulse >= 91 & Pulse <= 110 ~ 1,
#       Pulse >= 111 & Pulse <= 130 ~ 2,
#       Pulse >= 131 ~ 3,
#     ),
#     Consciousness_Score = case_when(
#       Consciousness == "A" ~ 0,
#       Consciousness == "VPU" ~ 3
#     ),
#     Total_Score = Respiration_Score + Saturation_Score + 
#       Pulse_Score + Oxygen_Supplement_Score +
#       Consciousness_Score
#   ) |> 
#   select(-Saturation_Score,-Respiration_Score,-Pulse_Score,-Oxygen_Supplement_Score,-Consciousness_Score)


# df <- df |> 
#   mutate(EWS_light = Total_Score) |> 
#   select(-Total_Score)


# # I-EWS with age and sex

# df <- df %>%
#   mutate(
#     Respiration_Score = case_when(
#       Respiration_Rate >= 0 & Respiration_Rate <= 20 ~ 0,
#       Respiration_Rate >= 21 & Respiration_Rate <= 24 ~ 2,
#       Respiration_Rate >= 25 ~ 3
#     ),
#     Saturation_Score = case_when(
#       Saturation <= 91 ~ 3,
#       Saturation >= 92 & Saturation <= 95 ~ 1,
#       Saturation >= 96 ~ 0,
#     ),
#     Oxygen_Supplement_Score = case_when(
#       Oxygen_Supplement == "Oxygen" ~ 1,
#       Oxygen_Supplement == "Air"  ~ 0,
#     ),
#     Pulse_Score = case_when(
#       Pulse <= 50 ~ 1,
#       Pulse >= 51 & Pulse <= 90 ~ 0,
#       Pulse >= 91 & Pulse <= 110 ~ 1,
#       Pulse >= 111 ~ 2
#     ),
#     Consciousness_Score = case_when(
#       Consciousness == "A" ~ 0,
#       Consciousness == "VPU" ~ 5
#     ),
#     Sex_Score = if_else(
#       Sex == "Male", 1,0
#     ),
#     Age_Score = case_when(
#       Age < 41 ~ 0,
#       Age >= 41 & Age <  51 ~ 1,
#       Age >= 51 & Age <  61 ~ 2,
#       Age >= 61 & Age <  66 ~ 3,
#       Age >= 66 & Age <  76 ~ 4,
#       Age >= 76 & Age <  81 ~ 5,
#       Age >= 81 & Age <  91 ~ 6,
#       Age >= 91 ~ 7
#     ),
#     Total_IScore = Respiration_Score + Saturation_Score + 
#       Pulse_Score + Oxygen_Supplement_Score +
#       Consciousness_Score + Sex_Score + Age_Score
#     ) |> 
#   select(-Saturation_Score,-Respiration_Score,
#          -Pulse_Score,-Oxygen_Supplement_Score,-Consciousness_Score,
#          -Sex_Score,-Age_Score)


# df <- df |> 
#   mutate(IEWS_Light = Total_IScore) |> 
#   select(-Total_IScore)


# 3. Initially restrict to Max 20 measurements per person and per hospitalization to avoid over-representation
# 4. Hospitals added
# 5. Unique hospitalization number added, i.e CSN
# 6. Department name added
# 7. Imputation with median and mode for NAs (various other ways possible, no difference in results based on previous work)
# 8. Blood tests, interventions, diagnoses, intensive care unit data added
# 9. Early warning scores are summarised into Max / mean per hospitalization and per patient, so every patient is repeated only based on the number of times they are hospitalized


# Open the initial dataset

data <- read_parquet("EWS_Final_Unique.parquet")

# Shift the position of the two first columns
# Here PT_ID is the personal identifier, and Identifier is a pseudo number that represents the unique PT_ID for each hospitalization

data <- data |> 
  relocate(PT_ID, .before = Identifier)

# Change some variables from characters to factors
# SKS_Category is diagnosis of the patient

data <- data |> 
  mutate_at(vars(ITA_Indicator,SKS_Category),as.factor)

data <- as.data.frame(data)

# Factorize interventions

data <- data |>
  mutate(Interventions = if_else(Interventions == 0, "No_Intervention","Intervention")) |>
  mutate_at(vars(Interventions),as.factor)

# Further pre-processing

data$Status30D <- relevel(data$Status30D,"Deceased")

# Refactoring the department categories (everything with less than 27486 counts gets lumped into the "Other" category)

data |> count(Department_Name)

data <- data |> 
  mutate(Department_Name_Fac = fct_lump_min(Department_Name,min =  27486))


# Categorizing previous hospitalization

data <- data |> 
  mutate(Previous_Hosp_Fac = case_when(Previous_Hosp == 0 ~ "Zero",
                                       Previous_Hosp == 1 ~ "One",
                                       Previous_Hosp == 2 ~ "Two",
                                       Previous_Hosp == 3 ~ "Three",
                                       Previous_Hosp == 4 ~ "Four",
                                       Previous_Hosp == 5 ~ "Five",
                                       .default = "6 or more")) |> 
  mutate_at(vars(Previous_Hosp_Fac),as.factor)


# Some statistics on the diagnoses

data |> count(SKS_Category,sort = T)

data <- data |> 
  mutate(SKS_Category = if_else(is.na(SKS_Category), "Unknown category",SKS_Category)) |> 
  mutate_at(vars(SKS_Category),as.factor)

# Defining Risk Groups based on EWS

data <- data |> 
  mutate(Risk_Groups_EWS = case_when(
    EWS_score >= 7 ~ "High",
    EWS_score >= 5 & EWS_score <= 6 ~ "Medium",
    (Respiration_Rate <= 8 | Respiration_Rate >=25) | (Saturation <= 91) | 
      (Pulse <= 40 | Pulse >= 131) | (Consciousness == "VPU") | (Temperature <= 35) | 
      (Blood_Pressure.Sys <= 90 | Blood_Pressure.Sys >= 220) ~ "Low-Medium",
    EWS_score >= 0 & EWS_score <= 4 ~ "Low")) |> 
  mutate(Risk_Groups_EWS = as.factor(Risk_Groups_EWS))

# Save the dataset just in case we work in Python

write_parquet(data,"df_august.parquet")

# We are going to use df_august.parquet for our further analysis