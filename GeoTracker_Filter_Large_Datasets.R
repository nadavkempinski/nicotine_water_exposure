## Import libraries - some may be unnecessary
library(readr)
library(stringr)
library(fs)
library(purrr)
library(furrr)
library(tidyverse)
library(here)
library(janitor)
library(glue) # for good looking message writing!
library(patchwork) # putting together plots
plan(multisession)  # for parallel processing or multicore (Unix/macOS)


# Define your base directory with .zip files
zip_dir <- "C:\\Users\\nadav\\Downloads\\StatewideEDF"  # Change to where your .zip files are

# Create a temp folder to extract each .zip to
extract_dir <- "./unzipped"
dir_create(extract_dir)

# Step 1: Extract all zip files
zip_files <- dir_ls(zip_dir, glob = "*.zip")

walk(zip_files, function(zip_path) {
  unzip(zip_path, exdir = file.path(extract_dir, path_ext_remove(path_file(zip_path))))
})

# Step 2: Get all .txt files from all unzipped folders
txt_files <- dir_ls(extract_dir, recurse = TRUE, glob = "*.txt")


## Function: Intake datasets, one chunk at a time
filter_large_file_fast <- function(infile, outfile,
                                   keywords = c("cotinine", "nicotine"),
                                   max_hits = 1e5,
                                   chunk_size = 10000,
                                   encodings = c("Windows-1252", "ISO-8859-1")) {
  keyword_pattern <- regex(paste(keywords, collapse = "|"), ignore_case = TRUE)
  
  if (file.exists(outfile)) file.remove(outfile)
  hits <- character()
  
  for (enc in encodings) {
    message("Trying: ", basename(infile), " with encoding: ", enc)
    tryCatch({
      callback <- SideEffectChunkCallback$new(function(lines, pos) {
        if (length(hits) >= max_hits) return()
        matches <- lines[str_detect(tolower(lines), keyword_pattern)]
        if (length(matches) > 0) hits <<- c(hits, matches)
        if (length(hits) >= max_hits) stop("Reached max hits", call. = FALSE)
      })
      
      read_lines_chunked(infile,
                         callback = callback,
                         chunk_size = chunk_size,
                         progress = FALSE,
                         locale = locale(encoding = enc))
      
      break  # success — exit encoding loop
      
    }, error = function(e) {
      if (!grepl("Reached max hits", e$message)) {
        message("Encoding ", enc, " failed for: ", basename(infile))
      }
    })
  }
  
  if (length(hits) > 0) writeLines(hits, outfile)
  return(invisible(length(hits)))
}

# Step 3: Prepare output files to input into
output_files <- txt_files %>%
  path_file() %>%
  path_ext_remove() %>%
  paste0("_filtered.csv") %>%
  file.path("filtered_outputs", .)

dir.create("filtered_outputs", showWarnings = FALSE)

# Step 4: Process. Data files will be available after this in the created directory
filtered_data = future_walk2(txt_files, output_files, filter_large_file_fast)

# Step 5: Extract successful results and bind
all_results <- filtered_data %>%
  keep(~ is.null(.x$error)) %>%
  map_dfr("result")

# Optional: save final combined result into one csv
write_csv(all_results, "filtered_nicotine_cotinine_data.csv")


## Step 5: Aggregate from saved files & add header from AlamedaEDF
aggregate_filtered_csvs <- function(filtered_dir, header_file, output_path) {
  library(readr)
  library(dplyr)
  library(purrr)
  
  # Step 1: Get all CSV file paths in filtered_dir
  csv_files <- list.files(filtered_dir, pattern = "\\.csv$", full.names = TRUE)
  
  # Step 2: Read the first line of the big header file (just 1 row)
  header <- read_lines(header_file, n_max = 1) %>%
    strsplit(split = ",") %>%
    unlist()
  
  # Step 3: Read each CSV and apply header
  data_list <- map(csv_files, ~{
    df <- read_csv(.x, col_names = FALSE, show_col_types = FALSE)
    names(df) <- header[1:ncol(df)]
    df
  })
  
  # Step 4: Combine all into one dataframe
  combined_data <- bind_rows(data_list)
  
  # Step 5: Write output to CSV
  write_csv(combined_data, delim= "\t", output_path)
  
  message("Combined file saved to: ", output_path)
}

aggregate_filtered_csvs(
  filtered_dir = "C:/Users/nadav/OneDrive/Desktop/R Studio Repo/nicotine_water_exposure/filtered_outputs",
  header_file = "C:/Users/nadav/OneDrive/Desktop/R Studio Repo/nicotine_water_exposure/unzipped/AlamedaEDF/AlamedaEDF.txt",
  output_path = "combined_output.csv"
)
