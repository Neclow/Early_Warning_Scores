# Load the neccessary packages

library(arrow)
library(tidyverse)
library(tidylog)

# Set the working directory

setwd("/home/alex/ews/NEWS2_Evaluation/Additional_Data")

# First I will start by pre-processing the diagnoses

diagnoses <- read_parquet("diagnoses.parquet")

#########

diagnoses <- diagnoses |>
  mutate(across(where(is.character), ~ ifelse(.x == "NULL", NA_character_, .x)))

diagnoses <- diagnoses |>
  mutate(
    `Kontakt slut` = as.POSIXct(
      `Kontakt slut`,
      format = "%Y-%m-%d %H:%M:%S",
      tz = ""
    ),
    `Kontakt start` = as.POSIXct(
      format(`Kontakt start`, "%Y-%m-%d %H:%M:%S"),
      tz = "",
      format = "%Y-%m-%d %H:%M:%S"
    )
  )

diagnoses <- diagnoses |>
  rename(Kontakt_start = `Kontakt start`) |>
  rename(Kontakt_slut = `Kontakt slut`)

# Define a function that takes the EnterpriseID column, replaces first letter of the string and adds 1 to the number remaining

transform_column <- function(column) {
  paste0("Z", as.numeric(str_sub(column, 2)) + 1)
}

# Implement PT_ID in diagnoses
diagnoses <- diagnoses |>
  mutate(PT_ID = transform_column(EnterpriseID))


# Relocate PT_ID to be after EnterpriseID
diagnoses <- diagnoses |>
  relocate(PT_ID, .after = EnterpriseID)

# We have to take out observations with missing CSNs

diagnoses <- diagnoses |>
  filter(!is.na(CSN))

diagnoses <- diagnoses |>
  select(
    EnterpriseID,
    PT_ID,
    CSN,
    `Department ID`,
    Kontakt_start,
    Aktionsdiagnose,
    `Aktionsdiagnose kode`
  ) |>
  rename(Diagnosis_Name = Aktionsdiagnose) |>
  rename(Department_ID = `Department ID`) |>
  rename(Diagnosis_Code = `Aktionsdiagnose kode`) |>
  arrange(Kontakt_start)

diagnoses <- diagnoses |>
  arrange(PT_ID)

# Now create the categories

diagnoses <- diagnoses |>
  mutate(Diagnosis_Code = str_sub(Diagnosis_Code, 2)) |>
  mutate(
    Diagnosis_Category = case_when(
      str_detect(Diagnosis_Code, "^[AB]") ~
        "Certain infectious and parasitic diseases",
      str_detect(Diagnosis_Code, "^C|^D[0-4]") ~ "Neoplasms",
      str_detect(Diagnosis_Code, "^D[5-8]") ~
        "Diseases of the blood and blood-forming organs and certain disorders involving the immune mechanism",
      str_detect(Diagnosis_Code, "^E") ~
        "Endocrine, nutritional and metabolic diseases",
      str_detect(Diagnosis_Code, "^F") ~
        "Mental, Behavioral and Neurodevelopmental disorders",
      str_detect(Diagnosis_Code, "^G") ~ "Diseases of the nervous system",
      str_detect(Diagnosis_Code, "^H[0-5]") ~ "Diseases of the eye and adnexa",
      str_detect(Diagnosis_Code, "^H[6-9]") ~
        "Diseases of the ear and mastoid process",
      str_detect(Diagnosis_Code, "^I") ~ "Diseases of the circulatory system",
      str_detect(Diagnosis_Code, "^J") ~ "Diseases of the respiratory system",
      str_detect(Diagnosis_Code, "^K") ~ "Diseases of the digestive system",
      str_detect(Diagnosis_Code, "^L") ~
        "Diseases of the skin and subcutaneous tissue",
      str_detect(Diagnosis_Code, "^M") ~
        "Diseases of the musculoskeletal system and connective tissue",
      str_detect(Diagnosis_Code, "^N") ~ "Diseases of the genitourinary system",
      str_detect(Diagnosis_Code, "^O") ~
        "Pregnancy, childbirth and the puerperium",
      str_detect(Diagnosis_Code, "^P") ~
        "Certain conditions originating in the perinatal period",
      str_detect(Diagnosis_Code, "^Q") ~
        "Congenital malformations, deformations and chromosomal abnormalities",
      str_detect(Diagnosis_Code, "^R") ~
        "Symptoms, signs and abnormal clinical and laboratory findings, not elsewhere classified",
      str_detect(Diagnosis_Code, "^[ST]") ~
        "Injury, poisoning and certain other consequences of external causes",
      str_detect(Diagnosis_Code, "^[VWX]|^Y") ~ "External causes of morbidity",
      str_detect(Diagnosis_Code, "^Z") ~
        "Factors influencing health status and contact with health services",
      str_detect(Diagnosis_Code, "^U") ~ "Codes for special purposes",
      TRUE ~ "Unknown category"
    )
  )

diagnoses |> write_parquet("diagnoses_latest.parquet")


##############################

### Procedures

procedures <- read_parquet("procedures.parquet")

# Let's add the PT_ID in the procedures
procedures <- procedures |>
  mutate(PT_ID = transform_column(EnterpriseID))

# Relocate PT_ID to be after EnterpriseID
procedures <- procedures |>
  relocate(PT_ID, .after = EnterpriseID)

# Rename some variables in procedures dataframe
procedures <- procedures |>
  rename(
    SKS_Code = `SKS-Kode`,
    Procedure_Date = `Udført procedurer dato`
  )

# Arrange per PT_ID

procedures <- procedures |>
  arrange(PT_ID)

# Convert procedure date into a proper datetime without UTC included (timezone naive)
procedures <- procedures |>
  mutate(
    Procedure_Date = as.POSIXct(
      format(ymd_hms(Procedure_Date), "%Y-%m-%d %H:%M:%S"),
      tz = "",
      format = "%Y-%m-%d %H:%M:%S"
    )
  )


# Replace all "NULL" string values into nulls
procedures <- procedures |>
  mutate(across(where(is.character), ~ ifelse(.x == "NULL", NA_character_, .x)))


# Remove procedures without a CSN

procedures <- procedures |>
  filter(!is.na(CSN))

# Create a new variable that categorizes the SKS Codes

procedures <- procedures |>
  mutate(
    SKS_Group = case_when(
      str_starts(SKS_Code, "A") ~ "Administrative Matters",
      str_starts(SKS_Code, "B") ~ "Treatment Care",
      str_starts(SKS_Code, "D") ~ "Diseases Health Conditions",
      str_starts(SKS_Code, "E") ~ "External Causes Injury",
      str_starts(SKS_Code, "F") ~ "Functional Ability",
      str_starts(SKS_Code, "K") ~ "Surgical Operations",
      str_starts(SKS_Code, "M") ~ "Drug Substance ATC",
      str_starts(SKS_Code, "N") ~ "Anesthesia or Intensive Care",
      str_starts(SKS_Code, "R") ~ "Results report",
      str_starts(SKS_Code, "U") ~ "Clinical Examinations / Radiological",
      str_starts(SKS_Code, "W") ~ "Clinical Physiological Examinations",
      str_starts(SKS_Code, "Z") ~ "Various Procedures",
      TRUE ~ NA_character_
    )
  ) |>
  relocate(SKS_Group, .after = SKS_Code)


# Save the dataframe

procedures |> write_parquet("procedures_latest.parquet")


####################

# Intensive care data

### Intensive care unit data processing

ita <- read_parquet("ita_respirator.parquet")

# Let's add the PT_ID in the intensive care unit dataframe
ita <- ita |>
  mutate(PT_ID = transform_column(EnterpriseID))

# Relocate PT_ID to be after EnterpriseID
ita <- ita |>
  relocate(PT_ID, .after = EnterpriseID)


# I need to change the "NULL" into proper NAs
ita <- ita |>
  mutate(across(where(is.character), ~ ifelse(.x == "NULL", NA_character_, .x)))

# Convert the strings which are dates into datetime variables (timezone naive)

ita <- ita |>
  mutate(
    Respirator_slut = as.POSIXct(
      `Respirator slut`,
      format = "%Y-%m-%d %H:%M:%S",
      tz = ""
    ),
    ITA_slut = as.POSIXct(`ITA slut`, format = "%Y-%m-%d %H:%M:%S", tz = ""),
    Respirator_start = as.POSIXct(
      format(`Respirator start`, "%Y-%m-%d %H:%M:%S"),
      tz = "",
      format = "%Y-%m-%d %H:%M:%S"
    ),
    ITA_start = as.POSIXct(
      format(`ITA start`, "%Y-%m-%d %H:%M:%S"),
      tz = "",
      format = "%Y-%m-%d %H:%M:%S"
    )
  )

ita <- ita |>
  arrange(PT_ID)

# Rename some variables

ita <- ita |>
  select(
    EnterpriseID,
    PT_ID,
    Respirator_start,
    Respirator_slut,
    ITA_start,
    ITA_slut,
    ITA_Department = `Ita Afsnit`,
    Respirator_Num = `Antal respiratorbehandlinger`
  )


ita |> write_parquet("ita_latest.parquet")


####################

# Blood tests

tests <- read_parquet("prøvesvar.parquet")

# Creation of PT_ID column
tests <- tests |>
  rename(
    EnterpriseID = V1,
    Blood_Test_Code = V2,
    Blood_Test_Name = V3,
    Blood_Test_Value = V4,
    Blood_Test_Status = V5,
    Blood_Test_Start = V6,
    Blood_Test_End = V7
  ) |>
  mutate(PT_ID = transform_column(EnterpriseID))

# Relocate PT_ID to be after EnterpriseID
tests <- tests |>
  relocate(PT_ID, .after = EnterpriseID)


# Fix datetime variables

tests <- tests |>
  mutate(
    Blood_Test_Start = as.POSIXct(
      format(Blood_Test_Start, "%Y-%m-%d %H:%M:%S"),
      tz = "",
      format = "%Y-%m-%d %H:%M:%S"
    ),
    Blood_Test_End = as.POSIXct(
      format(Blood_Test_End, "%Y-%m-%d %H:%M:%S"),
      tz = "",
      format = "%Y-%m-%d %H:%M:%S"
    )
  )

# tests |> write_parquet("tests_latest.parquet")

# Check unique blood tests
unique_test_names <- tests |>
  distinct(Blood_Test_Name) |>
  pull(Blood_Test_Name)

# Pivot the data
tests <- tests |>
  pivot_wider(
    names_from = Blood_Test_Name,
    values_from = Blood_Test_Value,
    id_cols = c(
      EnterpriseID,
      PT_ID,
      Blood_Test_Status,
      Blood_Test_Start,
      Blood_Test_End
    ),
    values_fn = first
  ) |>
  arrange(PT_ID, Blood_Test_Start)

# Filter for final results only
tests <- tests |>
  filter(Blood_Test_Status == "Endelig")

last_11_cols <- tail(names(tests), 11)

# Fix any weird characters in the values of tests as NAs

for (col in last_11_cols) {
  if (col %in% names(tests)) {
    tests <- tests |>
      mutate(
        !!sym(col) := case_when(
          # Keep existing NAs
          is.na(!!sym(col)) ~ NA_character_,
          # Keep values that can be converted to numeric
          !is.na(suppressWarnings(as.numeric(!!sym(col)))) ~ !!sym(col),
          # Keep detection limit values (< or > followed by numbers, decimals, spaces)
          str_detect(!!sym(col), "^[><]\\s*[0-9]+\\.?[0-9]*") ~ !!sym(col),
          # Convert everything else to NA (text, ANSI codes, etc.)
          TRUE ~ NA_character_
        )
      )
  }
}

head(tests)

# Imputation of weird values with medians

# Get the blood test columns (last 11 columns)
blood_test_cols <- tail(names(tests), 11)
cat(
  "Blood test columns to impute:",
  paste(blood_test_cols, collapse = ", "),
  "\n\n"
)


# Initialize result dataframe with first 5 columns (metadata)
tests_imputed <- tests |> select(1:5)

# Process each blood test column
for (col in blood_test_cols) {
  cat("Processing:", col, "\n")

  # Step 1: Get all values for this column
  values <- tests[[col]]

  # Step 2: Extract numeric values (our reference values)
  numeric_values <- as.numeric(values)
  sure_values <- numeric_values[!is.na(numeric_values)]

  # Step 3: Find problematic values starting with ">" or "<"
  problematic_values <- values |>
    .[!is.na(.)] |>
    .[str_detect(., "^[><]")] |>
    unique()

  cat("  Found", length(problematic_values), "problematic values\n")

  # Step 4: Calculate replacement values
  if (length(problematic_values) > 0) {
    # Create a mapping table
    replacement_map <- tibble(original = problematic_values) |>
      mutate(
        operator = str_sub(original, 1, 1),
        bound = as.numeric(str_sub(original, 2))
      ) |>
      rowwise() |>
      mutate(
        replacement = case_when(
          operator == "<" ~
            {
              # For "<X": median of all values below X
              lower_vals <- sure_values[sure_values < bound]
              if (length(lower_vals) > 0) median(lower_vals) else NA_real_
            },
          operator == ">" ~
            {
              # For ">X": median of all values above X
              upper_vals <- sure_values[sure_values > bound]
              if (length(upper_vals) > 0) median(upper_vals) else NA_real_
            },
          TRUE ~ NA_real_
        )
      ) |>
      ungroup()

    # Show what we're replacing
    cat("  Replacements:\n")
    replacement_map |>
      select(original, replacement) |>
      slice_head(n = 5) |>
      pwalk(~ cat("    ", ..1, "->", round(..2, 3), "\n"))

    # Create lookup vector for fast replacement
    lookup <- setNames(replacement_map$replacement, replacement_map$original)

    # Apply replacements
    imputed_values <- case_when(
      values %in% names(lookup) ~ lookup[values],
      TRUE ~ as.numeric(values)
    )
  } else {
    # No problematic values, just convert to numeric
    imputed_values <- as.numeric(values)
  }

  # Step 5: Add imputed column to result
  new_col_name <- paste0(col, "_imputed")
  tests_imputed[[new_col_name]] <- imputed_values

  cat(
    "  Original NAs:",
    sum(is.na(as.numeric(values))),
    "-> Final NAs:",
    sum(is.na(imputed_values)),
    "\n\n"
  )
}

# Make some modifications in the names of the blood tests

# More explicit approach
tests_imputed <- tests_imputed |>
  rename(
    Hemoglobin = `Hæmoglobin;B_imputed`,
    Leukocytter = `Leukocytter;B_imputed`,
    Trombocytter = `Trombocytter;B_imputed`,
    Kreatinin = `Kreatinin;P_imputed`,
    ALAT = `Alanintransaminase [ALAT];P_imputed`,
    LDH = `Laktatdehydrogenase [LDH];P_imputed`,
    Albumin = `Albumin;P_imputed`,
    CRP = `C-reaktivt protein [CRP];P_imputed`,
    Laktat_ab = `Laktat;P(aB)_imputed`,
    Troponin_T = `Troponin T;P_imputed`,
    Laktat_vb = `Laktat;P(vB)_imputed`
  )

names(tests_imputed)

tests_imputed |> write_parquet("tests_latest.parquet")


# First we start by joining blood tests with the original ews data

df <- read_parquet("/home/alex/ews/NEWS2_Evaluation/single_ews_imp.parquet")

blood_test_columns <- names(tests_imputed)[6:ncol(tests_imputed)]

df <- df |>
  inner_join(
    tests_imputed |> select(PT_ID, Blood_Test_End, all_of(blood_test_columns)),
    by = "PT_ID"
  ) |>
  filter(Blood_Test_End < recorded_time) |>
  group_by(PT_ID, recorded_time) |>
  summarise(
    across(
      all_of(blood_test_columns),
      ~ median(.x, na.rm = TRUE),
      .names = "median_{.col}"
    ),
    .groups = "drop"
  ) |>
  right_join(df, by = c("PT_ID", "recorded_time"))

# Replace NaNs with NAs

df <- df |>
  mutate(across(starts_with("median_"), ~ ifelse(is.nan(.x), NA_real_, .x)))

# Small re-ordering of columns

non_median_cols <- names(df)[!str_detect(names(df), "^median_")]
median_cols <- names(df)[str_detect(names(df), "^median_")]

# Reorder
df <- df |>
  select(all_of(non_median_cols), all_of(median_cols))

df <- df |> arrange(PT_ID, Identifier)

# Save the df

df |> write_parquet("df_with_bt_latest.parquet")


################################

# Merging EWS data with ITA info

# Create two new columns based on ITA data:
# 1. Previous ICU/Respiratory history (before recorded_time)
# 2. Early ICU/Respiratory in current admission (within 24 hours after recorded_time)

# First, let's create a column for previous ICU/respiratory history
# Check if patient had any ITA_Start or Respirator_Start before recorded_time

# Create a cross join to get all combinations of df records with ita records
previous_icu_check <- df |>
  select(PT_ID, Identifier, recorded_time) |>
  inner_join(
    ita |> select(PT_ID, ITA_start, Respirator_start),
    by = "PT_ID"
  ) |>
  filter(
    # Check if either ITA_Start or Respirator_Start occurred before recorded_time
    (ITA_start < recorded_time) | (Respirator_start < recorded_time)
  )

# Get unique combinations of PT_ID and recorded_time that had previous ICU/respiratory
previous_icu_patients <- previous_icu_check |>
  select(PT_ID, recorded_time) |>
  distinct() |>
  # Add a flag column
  mutate(previous_icu_respiratory = 1)

# Join back to main dataframe
df <- df |>
  left_join(previous_icu_patients, by = c("PT_ID", "recorded_time")) |>
  mutate(previous_icu_respiratory = replace_na(previous_icu_respiratory, 0))

cat("Previous ICU/Respiratory history column added\n")

# Second, create a column for early ICU/respiratory in current admission
# Check if patient had any ITA_Start or Respirator_Start within 24 hours after recorded_time

# Calculate 24 hours after recorded_time
early_icu_check <- df |>
  select(PT_ID, Identifier, recorded_time) |>
  mutate(recorded_time_plus_24h = recorded_time + hours(24)) |>
  inner_join(
    ita |> select(PT_ID, ITA_start, Respirator_start),
    by = "PT_ID"
  ) |>
  filter(
    # Check if either ITA_Start or Respirator_Start occurred within 24 hours after recorded_time
    ((ITA_start >= recorded_time) & (ITA_start <= recorded_time_plus_24h)) |
      ((Respirator_start >= recorded_time) &
        (Respirator_start <= recorded_time_plus_24h))
  )

# Get unique combinations that had early ICU/respiratory
early_icu_patients <- early_icu_check |>
  select(PT_ID, recorded_time) |>
  distinct() |>
  # Add a flag column
  mutate(early_icu_respiratory_24h = 1)

# Join back to main dataframe
df <- df |>
  left_join(early_icu_patients, by = c("PT_ID", "recorded_time")) |>
  mutate(early_icu_respiratory_24h = replace_na(early_icu_respiratory_24h, 0))


#######################

# Procedures data will now be implemented

procedures <- read_parquet("procedures_latest.parquet")

# Function to properly convert scientific notation PT_IDs
fix_pt_id <- function(pt_id) {
  case_when(
    str_detect(pt_id, "e\\+") ~
      {
        # Extract the Z part and the scientific number part
        z_part <- str_extract(pt_id, "^Z")
        number_part <- str_remove(pt_id, "^Z")
        # Convert scientific notation to regular number
        regular_number <- format(
          as.numeric(number_part),
          scientific = FALSE,
          trim = TRUE
        )
        paste0(z_part, regular_number)
      },
    TRUE ~ pt_id
  )
}

procedures <- procedures |>
  mutate(PT_ID = fix_pt_id(PT_ID))


# Now proceed with the procedures aggregation
# Create EWS reference data
ews_with_ptid <- df |>
  select(PT_ID, CSN, recorded_time) |>
  rename(ews_time = recorded_time)

# Clean procedures data
procedures_clean <- procedures |>
  select(
    EnterpriseID,
    PT_ID,
    SKS_Code,
    SKS_Group,
    Procedurenavn,
    Procedure_Date
  ) |>
  filter(!is.na(PT_ID), !is.na(Procedure_Date))

# Find procedures before each EWS time
procs_before_ews <- ews_with_ptid |>
  inner_join(procedures_clean, by = "PT_ID") |>
  filter(Procedure_Date < ews_time)

# Aggregate procedures by CSN
aggregated_procedures <- procs_before_ews |>
  group_by(CSN) |>
  summarise(
    Aggregated_Procedures = str_c(Procedurenavn, collapse = " | "),
    .groups = "drop"
  )

# Add to main dataframe
df <- df |>
  left_join(aggregated_procedures, by = "CSN") |>
  mutate(Aggregated_Procedures = replace_na(Aggregated_Procedures, "Ingen"))


# Save the dataframe

df |> write_parquet("df_with_bt_procs_latest.parquet")


###########################

# Diagnoses now

diagnoses <- read_parquet("diagnoses_latest.parquet")

diagnoses <- diagnoses |>
  filter(!duplicated(CSN)) |>
  arrange(PT_ID, Kontakt_start)

# Step 1: Fix PT_ID scientific notation issue in diagnoses
fix_pt_id <- function(pt_id) {
  case_when(
    str_detect(pt_id, "e\\+") ~
      {
        z_part <- str_extract(pt_id, "^Z")
        number_part <- str_remove(pt_id, "^Z")
        regular_number <- format(
          as.numeric(number_part),
          scientific = FALSE,
          trim = TRUE
        )
        paste0(z_part, regular_number)
      },
    TRUE ~ pt_id
  )
}

# Fix PT_IDs in diagnoses
diagnoses <- diagnoses |>
  mutate(PT_ID = fix_pt_id(PT_ID))

# Step 2: Create EWS reference data
ews_with_ptid <- df |>
  select(PT_ID, CSN, recorded_time) |>
  rename(ews_time = recorded_time)

# Step 3: Clean diagnoses data - keep only needed columns and remove missing values
diagnoses_clean <- diagnoses |>
  select(
    EnterpriseID,
    PT_ID,
    CSN,
    Department_ID,
    Kontakt_start,
    Diagnosis_Name,
    Diagnosis_Code
  ) |>
  filter(!is.na(PT_ID), !is.na(Kontakt_start), !is.na(Diagnosis_Name))

# Step 4: Find diagnoses before each EWS time (use explicit suffixes)
diag_before_ews <- ews_with_ptid |>
  inner_join(diagnoses_clean, by = "PT_ID", suffix = c("_ews", "_diag")) |>
  filter(Kontakt_start < ews_time)

# Step 5: Group by EWS CSN and concatenate diagnosis names with | separator
aggregated_diagnoses <- diag_before_ews |>
  group_by(CSN_ews) |> # Group by the EWS CSN (the hospitalization we're predicting for)
  summarise(
    Aggregated_Diagnoses = str_c(Diagnosis_Name, collapse = " | "),
    .groups = "drop"
  ) |>
  rename(CSN = CSN_ews)

# Step 6: Join the aggregated diagnoses to main df
df <- df |>
  left_join(aggregated_diagnoses, by = "CSN") |>
  mutate(Aggregated_Diagnoses = replace_na(Aggregated_Diagnoses, "Ingen"))


# Save the dataframe

df |> write_parquet("df_with_bt_procs_diags_latest.parquet")


##############

# Implementing the current diagnosis of CSN in the dataframe

diagnoses <- read_parquet("diagnoses_latest.parquet")

diagnoses <- diagnoses |>
  filter(!duplicated(CSN)) |>
  arrange(PT_ID, Kontakt_start)

# Step 1: Fix PT_ID scientific notation issue in diagnoses
fix_pt_id <- function(pt_id) {
  case_when(
    str_detect(pt_id, "e\\+") ~
      {
        z_part <- str_extract(pt_id, "^Z")
        number_part <- str_remove(pt_id, "^Z")
        regular_number <- format(
          as.numeric(number_part),
          scientific = FALSE,
          trim = TRUE
        )
        paste0(z_part, regular_number)
      },
    TRUE ~ pt_id
  )
}

# Fix PT_IDs in diagnoses
diagnoses <- diagnoses |>
  mutate(PT_ID = fix_pt_id(PT_ID))


df <- df |>
  left_join(
    diagnoses |>
      select(PT_ID, CSN, Diagnosis_Category),
    by = c("PT_ID", "CSN")
  )


df |> write_parquet("df_with_bt_procs_diags_latest.parquet")


###########

# Implement the interventions at 24 hours after recorded time

procedures <- read_parquet("procedures_latest.parquet")
df <- read_parquet("df_with_bt_procs_diags_latest.parquet")

# Function to properly convert scientific notation PT_IDs
fix_pt_id <- function(pt_id) {
  case_when(
    str_detect(pt_id, "e\\+") ~
      {
        # Extract the Z part and the scientific number part
        z_part <- str_extract(pt_id, "^Z")
        number_part <- str_remove(pt_id, "^Z")
        # Convert scientific notation to regular number
        regular_number <- format(
          as.numeric(number_part),
          scientific = FALSE,
          trim = TRUE
        )
        paste0(z_part, regular_number)
      },
    TRUE ~ pt_id
  )
}

procedures <- procedures |>
  mutate(PT_ID = fix_pt_id(PT_ID))


# Clean procedures data
procedures <- procedures |>
  filter(!is.na(PT_ID), !is.na(Procedure_Date))


# Join recorded_time from df to procedures
procedures <- procedures |>
  left_join(
    df |> select(PT_ID, CSN, recorded_time),
    by = c("PT_ID", "CSN")
  )

# Filter procedures that happen within 24H after recorded_time
procedures <- procedures |>
  mutate(cutoff_time = recorded_time + hours(24)) |>
  filter(
    !is.na(recorded_time),
    !is.na(Procedure_Date),
    Procedure_Date >= recorded_time,
    Procedure_Date <= cutoff_time
  ) |>
  arrange(PT_ID, CSN)

# Keep procedures that are only found in the df dataframe
procedures <- procedures |>
  filter(CSN %in% df$CSN)

procedures |> head()

# Count the number of distinct procedures each individual (per CSN) has been exposed to
csns_interventions <- procedures |>
  group_by(CSN) |>
  summarise(SKS_Group = list(unique(SKS_Group)), .groups = "drop") |>
  unnest(SKS_Group) |>
  arrange(CSN)

csns_interventions |> head()

# Individuals that have received Anesthesia or Intensive Care or Surgical Operations
csns_interventions_groups <- csns_interventions |>
  filter(
    SKS_Group == "Anesthesia or Intensive Care" |
      SKS_Group == "Surgical Operations"
  )

# Get CSNs with SKS codes containing "BGDA" (ventilation support)
csns_bgda <- procedures |>
  filter(str_detect(SKS_Code, "BGDA")) |>
  select(CSN) |>
  distinct()

# Combine both criteria: either specific SKS_Groups OR BGDA codes
csns_with_interventions <- bind_rows(
  csns_interventions_groups |> select(CSN) |> distinct(),
  csns_bgda
) |>
  distinct()

# Remove any null values
csns_with_interventions <- csns_with_interventions |>
  drop_na()

# Now we need to go back to the df data
# If we have individuals that have been exposed to Anesthesia/ITA or Surgical Operations
# OR have BGDA codes (ventilation support), OR have early_icu_respiratory_24h == 1
# we consider that there has been an intervention
df <- df |>
  mutate(
    Interventions_24 = case_when(
      CSN %in% csns_with_interventions$CSN | early_icu_respiratory_24h == 1 ~ 1,
      TRUE ~ 0
    )
  )

# Save the df now

df |> write_parquet("df_without_embeddings.parquet")
