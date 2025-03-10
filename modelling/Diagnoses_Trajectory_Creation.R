# Load the libraries used

library(arrow)
library(tidyverse)
library(data.table)

# setwd("/home/alex/ews/aggregated")

# Load the EWS data (df_august file containing EWS scores for patients, plus other metadata)

ews <- open_dataset("df_august.parquet")

# Load the diagnoses

# setwd("/home/alex/ews/diagnoses")

diagnoses <- open_dataset("diagnoses_newest_with_sks_group.parquet")

# Now make some minor modifications to the diagnoses dataframe

diagnoses <- diagnoses |> 
  select(PT_ID,CSN,Aktionsdiagnose)

diagnoses <- diagnoses |>
  mutate(CSN = as.character(CSN)) |>
  mutate(PT_ID = as.character(PT_ID))

diagnoses_ordered <- ews |> 
  select(PT_ID, CSN, Identifier) |>
  left_join(diagnoses, by = c("PT_ID","CSN")) |>
  arrange(PT_ID)

# Now let's collect the data

diagnoses_ordered <- diagnoses_ordered |> collect()

# Filter duplicated hospitalization numbers

diagnoses_ordered <- diagnoses_ordered |>
  filter(!duplicated(CSN))

# Use the data.table library for faster preprocessing of the data

setDT(diagnoses_ordered)

diagnoses_ordered[, Identifier := NULL]

setorder(diagnoses_ordered, PT_ID)

# Now we need to merge the primary diagnoses (aktionsdiagnose) for each patient

# This will use accumulate and shift function to include all the previous diagnoses before the hospitalization

diagnoses_ordered[, Previous_Diagnoses := shift(Reduce(function(x, y) paste(x, y, sep = " | "), Aktionsdiagnose, accumulate = TRUE), 
                                                fill = "NA"), by = PT_ID]


diagnoses_ordered[, Aktionsdiagnose := NULL]


# Store the data as chunks / batches

# Get unique PT_IDs and split into batches of 1000
unique_ids <- unique(diagnoses_ordered$PT_ID)
id_batches <- split(unique_ids, ceiling(seq_along(unique_ids) / 1000))

# Directory to save Parquet files (create if it doesn't exist)
# output_dir <- "/home/alex/ews/diagnoses"

# Save each batch of 1000 PT_IDs as a separate Parquet file
for (i in seq_along(id_batches)) {
  # Filter data for the current batch of PT_IDs
  batch_data <- diagnoses_ordered[PT_ID %in% id_batches[[i]]]
  
  # Define the filename
  file_name <- file.path(output_dir, paste0("diagnoses_ordered_new_batch_", i, ".parquet"))
  
  # Write to Parquet
  write_parquet(batch_data, file_name)
  
  # Optional: Print confirmation
  cat("Saved batch", i, "to", file_name, "\n")
  
  rm(batch_data)
  
}