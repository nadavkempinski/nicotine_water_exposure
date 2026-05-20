### Data reviewer
# Author: Nadav Kempinski
# ---

### Packages
library(tidyverse)
library(here)
library(janitor)
library(glue) # for good looking message writing!
library(patchwork) # putting together plots

### Data Import
swrcb_ddw_data = read_csv("C:/Users/nadav/Downloads/drinking-water-quality-data-clip-sql-all-2/Drinking Water Quality Data CLIP (SQL) ALL.csv")
ceden_cotinine_data = read.delim("C:/Users/nadav/Downloads/export (1).tsv", sep = "\t")
### Parameters (if needed)

### Main Script

## Review shapefiles without importing
library(foreign)
library(sf)
waterboards_shp <- st_read("C:/Users\\nadav\\Downloads\\Proposed_2024_Integrated_Report_Polygons_2\\Final_2024_IR_Map_Pol2y.dbf")


# Now search for a specific word
keyword <- "cot"
matches <- waterboards_shp[apply(dbf, 1, function(row) any(grepl(keyword, row, ignore.case = TRUE))), ]

print(matches)

# take in all data files 1-27
df0 = readr::read_delim("C:/Users/nadav/Downloads/gama_ddw_statewide_v2/gama_ddw_statewide_v2.txt", delim = "\t") |> filter(GM_CHEMICAL_NAME=="N-Nitrosodimethylamine (NDMA)")
df1 = readr::read_delim("C:/Users/nadav/Downloads/gama_ddw_statewide_v2/p1.txt", delim = "\t") |> filter(GM_CHEMICAL_NAME=="N-Nitrosodimethylamine (NDMA)")
df2 = readr::read_delim("C:/Users/nadav/Downloads/gama_ddw_statewide_v2/p2.txt", delim = "\t") |> filter(GM_CHEMICAL_NAME=="N-Nitrosodimethylamine (NDMA)")

## For USGS GAMA data
df1_filtered = df1 |>
  filter(GM_CHEMICAL_NAME=="N-Nitrosodimethylamine (NDMA)")

file_list <- list.files(path = "C:/Users/nadav/Downloads/gama_ddw_statewide_v2/", pattern = "\\.txt$", full.names = TRUE)

# Define your filter function
read_and_filter <- function(file) {
  read_delim(file, delim = "\t") %>%
    filter(GM_CHEMICAL_NAME=="N-Nitrosodimethylamine (NDMA)") %>%  # Replace with your actual filter condition
    mutate(source_file = basename(file))  # Optional: track file source
}

# Combine all filtered data into one data frame
filtered_data_gama <- map_dfr(file_list, read_and_filter)

# Save to a single CSV file
write_csv(filtered_data, "filtered_combined.csv")


## for GeoTracker data
library(tidyverse)
library(fs)

# Define your base directory with .zip files
zip_dir <- "C:/Users/nadav/Downloads/StatewideEDF"  # Change to where your .zip files are

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

# Step 3: Function to read + filter cotinine/nicotine data
read_and_filter <- function(file) {
  df <- read_delim(file, delim = "\t", col_types = cols(.default = "c"))  # safer to read as character first
  # Check for expected column names if needed; or use partial match
  df <- df %>% filter(str_detect(tolower(across(everything(), paste, collapse = " ")), "cotinine|nicotine"))
  if (nrow(df) > 0) df <- mutate(df, source_file = file)
  df
}

# Step 4: Apply and combine
filtered_data_geotracker <- map_dfr(txt_files, read_and_filter)

# Step 5: Extract successful results and bind
all_results <- filtered_data %>%
  keep(~ is.null(.x$error)) %>%
  map_dfr("result")

# Optional: save final combined result
write_csv(all_results, "filtered_nicotine_cotinine_data.csv")
