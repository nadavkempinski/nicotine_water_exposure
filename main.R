## Purpose: Create a map of CA-based nicotine (and metabolite) concentrations found in drinking (or source) water

### Pseudocode:
# 1. Intake concentrations data
# 1a. Clean data
# 1b. Geocode data
# 2. Create a map of CA
# 3. Insert points on the map
# 3a. Visually show on map: concentration level, contaminant type, water type
# 3b. Insert legends for each visual scale
# 4. Save map as image for export

## 0. Import libraries
library(tidyverse)
library(here)
library(janitor)
library(glue) # for good looking message writing!
library(patchwork) # putting together plots
library(sf) # for map-making
library(rnaturalearth) # map-making
library(tigris) # for sf layers
options(tigris_use_cache = TRUE) # to save sf layers instead of re-downloading
library(googlesheets4) # to import concentrations data. Note: requires authorization. In the future, concentration data will be stored with the project

## 1. Intake data
data_file = read_sheet("https://docs.google.com/spreadsheets/d/1r73-RLY5JXLARDwIof56cgKEzioUuLcAKdD_s3ZkhYU/edit?gid=1804529984#gid=1804529984", sheet = "Table: CA Concentrations") # using gSheets as I work on this online
data = data_file |>
  clean_names() |> 
  select(-c(x9:aim_for))

## what else needs to happen with the data
## - create factors for visible layers (compound, water type)
## - normalize continuous data & determine value for NDs
## - get geolocation data to place on a map


## 2. Create a map of CA
ca_state_shp = ne_states(country="United States of America", returnclass = "sf") |> filter(name=="California") # get US shapefile & filter for CA
ca_counties_shp = tigris::counties(state="CA") # get CA counties 
ca_rivers_shp = ne_download(scale="large", type = "rivers_lake_centerlines", category="physical", returnclass="sf") |> st_intersection(ca_state_shp) # get rivers
ca_cities_shp = ne_download(scale = "large", type = "populated_places", category = "cultural", returnclass = "sf") |> filter(ADM1NAME == "California") # get ciies
ca_lakes_shp <- ne_download(scale = "large", type = "lakes", category = "physical", returnclass = "sf") |> # get lakes
  st_make_valid() |> # because of weird cropping issues
  st_intersection(ca_state_shp) # crop to CA

# check over the basemap
ca_base_map = ggplot() +
  geom_sf(data = ca_state_shp, fill="white", color="black")+
  coord_sf()+
  theme_void()+
  labs(title=str_wrap("California Map of Rivers, Lakes, and Population Centers"))+
  geom_sf(data=ca_counties_shp)+
  geom_sf(data=ca_rivers_shp)+
  # geom_sf(data=ca_cities_shp)+
  geom_sf(data=ca_lakes_shp)
  
## 3. Insert points on map of CA
ca_data_map = ca_base_map +
  geom_point(data=data, aes(x=lon,y=lat, color=concentration, shape=compound))
  # set visible properties based on water type, concentration, and compound
  
  