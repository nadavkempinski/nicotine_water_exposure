### Purpose: Gather USGS data to determine accurate water body

# nwis api
library(dataRetrieval)
library(dplyr)
library(purrr)

### Check how many samples include USGS site data: 3116
data_zeros |>
  filter(str_detect(site_id, "USGS")) |>
  View()

### Prepare Site IDs & analytes for USGS API
unique_locs <- unique(data_zeros$site_id) |> na.omit() # 2208
unique_analytes <- unique(data_zeros$analyte) |> na.omit() # 2

### Prepare API call 
fetch_usgs_metadata <- function(loc_id, analytes) {
  
  # 1. Fetch data safely
  raw_data <- tryCatch({
    read_waterdata_samples(
      monitoringLocationIdentifier = loc_id,
      characteristic = analytes,
      stateFips = "US:06", 
      dataType = "results",
      dataProfile = "fullphyschem"
    )
  }, error = function(e) NULL)
  
  # 2. Check if the API returned empty data or failed
  if (is.null(raw_data) || nrow(raw_data) == 0) {
    message("No data found for location ID: ", loc_id)
    return(tibble()) # Return an empty tibble so map_dfr can skip it without crashing
  }
  
  # 3. Select columns safely using exact names from your dataset
  clean_data <- raw_data |>
    select(
      any_of(c(
        "Location_Identifier",         # Join key 1
        "Result_Characteristic",       # Join key 2
        "Location_Name", 
        "Location_Type", 
        "Location_HUCTwelveDigitCode", 
        "Location_Latitude", 
        "Location_Longitude", 
        "Activity_MediaSubdivision", 
        "Activity_Comment"
      ))
    )
  
  # 4. Filter for distinct rows using the exact column names
  # Only attempt to use distinct if the columns actually exist in the result
  group_cols <- intersect(
    names(clean_data), 
    c("Location_Identifier", "Result_Characteristic")
  )
  
  if (length(group_cols) > 0) {
    clean_data <- clean_data |> 
      distinct(across(all_of(group_cols)), .keep_all = TRUE)
  }
  
  return(clean_data)
}

# 2. Fetch the data
usgs_metadata_table <- map_dfr(unique_locs, ~ fetch_usgs_metadata(.x, unique_analytes))

# 2a. Review any missing sample locations with USGS site IDs and try to collect
missing_samples = augmented_dataset |> 
  filter(str_detect(site_id, "USGS"), is.na(Location_Type)) |>
  select(site_id) 
missing_locs = unique(missing_samples$site_id)
usgs_missing = map_dfr(missing_locs, ~ fetch_usgs_metadata(.x, unique_analytes))

# 2b. Merge location info datasets
all_usgs_sites = bind_rows(usgs_metadata_table, usgs_missing, .id="api_set")

# 3. Join the data back
augmented_dataset <- data_zeros |>
  left_join(
    all_usgs_sites,
    by = c(
      "site_id" = "Location_Identifier", 
      "analyte" = "Result_Characteristic"
    )
  )

# 4. Check how many USGS sites are missing data. Why would this be?
missing_samples = augmented_dataset |> 
  filter(str_detect(site_id, "USGS"), is.na(Location_Type)) |>
  select(site_id) 
count(missing_samples) # 65

# 5. Review comparison between water_body and Activity_MediaSubdivision
augmented_dataset |>
  filter(!is.na(Activity_MediaSubdivision), tolower(water_body) != tolower(Location_Type)) |>
  select(row_index, site_id, Location_Name, water_body, Location_Type, Activity_MediaSubdivision, notes, Activity_Comment) |>
  distinct(water_body, Location_Type) |>
  View()

# 6. Fix USGS dataset and re-assert
all_usgs_sites_fixed <- all_usgs_sites |>
  # first, convert Artificial to Groundwater. All of them use wells. Confirmed via dataset and USGS data: https://waterdata.usgs.gov/monitoring-location/USGS-401754122161601/#dataTypeId=measurements-72019-0&period=P1Y
  mutate(
    Activity_MediaSubdivision = replace_when(
      Activity_MediaSubdivision,
      # Removes all spaces/punctuation, converts to lowercase, then checks for "artificial"
      str_detect(
        str_to_lower(str_replace_all(Activity_MediaSubdivision, "[^a-zA-Z]", "")), 
        "artificial"
      ) ~ "Groundwater"
    )
  )
# View(all_usgs_sites_fixed)
### Note: Updated 17 samples due to incorrect water body designation