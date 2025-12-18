### Purpose: Plot map of CA & samples
# Import libraries
library(tidyverse)
library(here)
library(glue) # for good looking message writing!
library(patchwork) # putting together plots
library(sf) # for map-making
library(rnaturalearth) # map-making
library(tigris) # for sf layers
options(tigris_use_cache = TRUE) # to save sf layers instead of re-downloading
library(lubridate)

## Create a map of CA
ca_state_shp = ne_states(country="United States of America", returnclass = "sf") |> # get US shapefile & filter for CA
  filter(name=="California") |> st_make_valid()
# get base layer coordinate system so we can project all layers over
ca_state_shp_crs = st_crs(ca_state_shp)
ca_counties_shp = tigris::counties(state="CA") |>
  st_transform(ca_state_shp_crs) # get CA counties 
ca_rivers_shp = ne_download(scale="large", type = "rivers_lake_centerlines", category="physical", returnclass="sf") |> st_intersection(ca_state_shp) |> # get rivers
  st_transform(ca_state_shp_crs) |> # get CA rivers & lakes 
  st_make_valid()
ca_cities_shp = ne_download(scale = "large", type = "populated_places", category = "cultural", returnclass = "sf") |> filter(ADM1NAME == "California") |> # get cities
  st_transform(ca_state_shp_crs) # get CA population centers
ca_lakes_shp = ne_download(scale = "large", type = "lakes", category = "physical", returnclass = "sf") |> # get lakes
  st_make_valid() |> # because of weird cropping issues
  st_intersection(ca_state_shp) |> # crop to CA 
  st_transform(ca_state_shp_crs) # get CA lakes 

# # get reference datum for each layer
# st_crs(ca_state_shp)
# st_crs(ca_counties_shp)

# check over the basemap
data_range_year = paste0(year(min(data_zeros$sample_date)), "-", year(max(data_zeros$sample_date)))

ca_base_map = ggplot() +
  geom_sf(data = ca_land, fill = "white", color = "black") +
  geom_sf(data = ca_counties_shp, color = "gray60", fill = NA, size = 0.2) +
  geom_sf(data = ca_rivers_shp, color = "lightblue") +
  geom_sf(data = ca_lakes_shp, color = "lightblue", fill = "lightblue") +
  theme_void() +
  theme(
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  ) +
  labs(title=paste0("California Map of Tobacco-Related Analyte Samples (", data_range_year, ")"))
ca_base_map

## FIGURE: Map of CA with all samples. Must state grays in caption
ca_samples_map <- ca_base_map +
  # Plot non-detects in gray
  geom_point(data = data_zeros |> filter(sample_concentration_normalized==0),
             aes(x = longitude, y = latitude, shape = analyte),
             fill = "gray90", color = "black", size = 2, alpha = 0.5) +
  
  # Plot detects with log-scaled fill
  geom_point(data = data_detects,
             aes(x = longitude, y = latitude, shape = analyte, fill = sample_concentration_normalized),
             color = "black", size = 2, alpha = 0.8) +
  
  scale_fill_gradientn(
    name = "Concentration (ng/L)",
    colors = c("skyblue", "blue", "red", "black"),
    values = scales::rescale(log10(c(1, 10, 100, 1000))),
    trans = "log",
    breaks = c(1, 10, 100, 1000),
    labels = c("1", "10", "100", "1000")
  ) +
  
  scale_shape_manual(
    name = "Analyte",
    values = c("Cotinine" = 21, "Nicotine" = 24)
  )+
  theme(
    plot.title = element_text(size = 7.5, hjust = 0.5, face = "bold"),
    legend.title = element_text(size = 7),  # legend title font size
    legend.text  = element_text(size = 7),   # legend labels font size
    legend.key.height = unit(0.35, "cm"),     # size of the color bar boxes
    legend.key.width  = unit(0.25, "cm"),
    legend.position = c(0.75, 0.75),   # adjust until it sits perfectly
    legend.background = element_rect(fill = alpha("white", 0.7), color = NA),
  )
ca_samples_map # will remind you that Masoner is missing, no lat/lon data!

# save for journal
ggsave(
  "ca_map.png",
  ca_samples_map,  # object from below
  width  = 3.33,         # inches  (240 pt)
  height = 4,            # anything < ~9.17"
  units  = "in",
  dpi    = 600
)

#### Water utilities mapping ####
utilities_split = read.csv(here::here("data/utilities_split.csv")) |> janitor::clean_names()
utilities_access = read.csv(here::here("data/utilities_access.csv")) |> janitor::clean_names()

# download reservoir locations
library(googlesheets4)
reservoir_file = read_sheet("https://docs.google.com/spreadsheets/d/1a8hoejs6n8PcZhkKPoWvtCVLCqwu8YyjdWZbeQUa_tE/edit?gid=0#gid=0") # using gSheets as I work on this online. using na="-" messes up the detection_method column
reservoirs = reservoir_file |> janitor::clean_names() |>
  filter(!is.na(lon), !is.na(lat)) |>
  mutate(lat = trim(lat), lon = trim(lon))



# Plot along current data points
ca_base_map+
  geom_point(data = utilities_split, aes(x=lon, y=lat, shape="Split"), size=1.5, color="darkred", fill="darkred") +
  geom_point(data = utilities_access, aes(x=lon, y=lat, shape="Sample"), size=1.5, color="green", fill="green")+
  geom_point(data = reservoirs, aes(x = lon, y = lat, shape="Reservoirs"),
    size = 1.5,
    color = "lightblue",
    fill = "lightblue"
  ) + 
  scale_shape_manual(
    name = "Type",
    values = c("Split" = 16, "Sample" = 15, "Reservoirs" = 17)  # square, triangle
  )

# for exporting to Google Maps for collaboration
max_r = 2000
df1 <- data_zeros[1:min(max_r, nrow(data_zeros)), ]
df2 <- data_zeros[(max_r + 1):nrow(data_zeros), ]
write_csv(df1, "data_zerosp1.csv")
write_csv(df2, "data_zerosp2.csv")
