### Purpose: Check basin # sampling counts 
library(sf)
library(tidyverse)
library(kableExtra)

# Load your points and assign classification
data_sf = data_zeros |>
  drop_na(latitude, longitude)
sample_data <- st_as_sf(data_sf, coords=c('longitude', "latitude"), crs=4326)  # or use st_as_sf

## check by basin area: basins_sf
basins_sf = st_read("C:/Users/nadav/Downloads/i08_B118_CA_GroundwaterBasins") |> st_transform(4326)
sf::sf_use_s2(FALSE) # due to duplicate vertex

samples_with_basins <- st_join(sample_data, basins_sf[, c("Basin_Numb")], left = TRUE) |>
  mutate(basin_major = str_extract(Basin_Numb, "^[^-]+")) |>
  filter(water_body == "Groundwater")
sf::sf_use_s2(TRUE) # make sure it's back for any future use I may need outside this project

samples_with_basins |>
  group_by(basin_major) |>
  summarize(count=n()) |>
  kable() |>
  kable_styling()

## visualize what areas we have
ggplot() +
  # geom_sf(data = basins_sf, color = "gray30", alpha = 0.3) +
  geom_sf(data = samples_with_basins, aes(color = basin_major), size = 1, alpha = 0.8) +
  scale_fill_viridis_d(option = "turbo", name = "Major Basin") +
  scale_color_viridis_d(option = "turbo", name = "Major Basin") +
  theme_minimal() +
  labs(title = "Sample Points by Major Groundwater Basin (1–9)",
       subtitle = "Based on CA Bulletin 118 Basin Numbering",
       x = "Longitude", y = "Latitude")
