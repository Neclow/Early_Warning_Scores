# Load the libraries used

library(arrow)
library(dplyr)
library(DBI)
library(duckdb)
library(future.apply)

# Set up parallel processing
plan(multisession, workers = parallel::detectCores() - 1)  # Adjust number of cores as needed

# Set working directory and load initial data
procedures <- read_parquet("procedures_newest.parquet")

# Sort the data by PT_ID and Procedure_Date
procedures <- procedures %>% arrange(PT_ID, Procedure_Date)

# Unique PT_IDs for batching
unique_pt_ids <- unique(procedures$PT_ID)
batch_size <- 1000  # Adjust based on system capacity
total_batches <- ceiling(length(unique_pt_ids) / batch_size)

# Create DuckDB connection (USE Of DuckDB for better memory usage)
con <- dbConnect(duckdb::duckdb())
dbExecute(con, "PRAGMA max_temp_directory_size='20GiB'")

# Step 1: Split `procedures` into smaller batch files and store file paths
batch_file_paths <- list()

for (i in seq(1, length(unique_pt_ids), by = batch_size)) {
  # Define the batch of PT_IDs
  pt_ids_batch <- unique_pt_ids[i:min(i + batch_size - 1, length(unique_pt_ids))]
  
  # Filter the current batch from procedures
  procedures_batch <- procedures %>% filter(PT_ID %in% pt_ids_batch)
  
  # Define file path for this batch and save as parquet
  batch_file_path <- paste0("batch_data_PTID_", pt_ids_batch[1], "_to_", pt_ids_batch[length(pt_ids_batch)], ".parquet")
  write_parquet(procedures_batch, batch_file_path)
  
  # Store file path
  batch_file_paths[[length(batch_file_paths) + 1]] <- batch_file_path
}

# Check

# output_dir <- here

# Get the full paths of all batch files
batch_file_paths <- list.files(output_dir, pattern = "^batch_data_PTID_.*\\.parquet$", full.names = TRUE)

# Verify the list of batch file paths
print(length(batch_file_paths))  # Should be 1439
print(batch_file_paths[1:5])     # Preview the first few paths

# Step 2

process_and_save_batch <- function(batch_file_path) {
  # Verify file exists
  if (!file.exists(batch_file_path)) {
    message("File not found: ", batch_file_path)
    return(NULL)
  }
  
  # Establish a DuckDB connection within each worker
  con <- dbConnect(duckdb::duckdb())
  dbExecute(con, "PRAGMA max_temp_directory_size='20GiB'")
  
  # Load the batch data from the parquet file
  procedures_batch <- read_parquet(batch_file_path)
  
  # Write the batch to DuckDB
  dbWriteTable(con, "df_batch", procedures_batch, overwrite = TRUE)
  
  # Define the query with cumulative aggregation
  query <- "
    SELECT 
      PT_ID, 
      CSN,
      Procedure_Date,
      CASE 
        WHEN ROW_NUMBER() OVER (PARTITION BY PT_ID ORDER BY Procedure_Date) = 1 THEN NULL 
        ELSE STRING_AGG(Procedurenavn, ', ') OVER (
          PARTITION BY PT_ID 
          ORDER BY Procedure_Date 
          ROWS BETWEEN UNBOUNDED PRECEDING AND 1 PRECEDING
        )
      END AS Aggregated_Procedures,
      ROW_NUMBER() OVER (PARTITION BY CSN ORDER BY Procedure_Date DESC) AS rn
    FROM df_batch
    ORDER BY PT_ID, Procedure_Date
  "
  
  # Execute the query and retrieve results for the current batch
  batch_result <- dbGetQuery(con, query) %>%
    filter(rn == 1) %>%                            # Keep only the latest row per CSN
    mutate(Aggregated_Procedures = ifelse(is.na(Aggregated_Procedures), "NA", Aggregated_Procedures)) %>%
    select(-rn) %>%                                # Drop the `rn` column
    arrange(PT_ID, Procedure_Date) %>%             # Sort for readability
    select(-Procedure_Date)                        # Drop Procedure_Date if not needed
  
  # Define the output file path for the processed result
  processed_file_path <- file.path(output_dir, sub("batch_data_", "processed_batch_", basename(batch_file_path)))
  
  # Save the processed result as a parquet file
  write_parquet(batch_result, processed_file_path)
  
  # Disconnect from DuckDB
  dbDisconnect(con)
  
  # Display progress
  message("Processed and saved batch to ", processed_file_path)
}

plan(multisession, workers = parallel::detectCores() - 1)

# Run the processing function on each batch file in parallel
future_lapply(batch_file_paths, process_and_save_batch)


#### Now let's merge them into one big dataset

# Directory where processed files are stored
output_dir <- "/Users/jkv465/Desktop/Batches_Procedures/"

# List all processed batch files
processed_files <- list.files(output_dir, pattern = "^processed_batch_.*\\.parquet$", full.names = TRUE)