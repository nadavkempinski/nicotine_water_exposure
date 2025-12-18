### Purpose: Geolocate addresses. Used to map water utilities into Google My Maps


library(tidygeocoder)
library(dplyr)

# Example data frame with addresses
addresses = read.csv("C:/Users/nadav/Downloads/Water Utility Responses.xlsx - Split Only (2).csv")

# Geocode using default (Nominatim / OpenStreetMap)
geo_results <- addresses %>% janitor::clean_names() |>
  geocode(address = address_sdwis, method = "osm", lat = latitude, long = longitude)

View(geo_results |> select(address_sdwis, latitude, longitude))

geocoded = geo_results |> filter(!is.na(latitude), !is.na(longitude))
to_geocode = geo_results |> filter(is.na(latitude) | is.na(longitude)) |>
  mutate(zip = substr(address_sdwis, nchar(address_sdwis) - 4, nchar(address_sdwis)))
View(to_geocode |> select(address_sdwis, latitude, longitude, zip))



geo_results2 = to_geocode |> geocode(address = zip, method="osm", lat=latitude, long=longitude, limit=1)
View(geo_results2 |> select(address_sdwis, latitude, longitude, zip))
