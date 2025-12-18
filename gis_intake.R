library(sf)
library(ggplot2)
library(esri2sf) #remotes::install_github("yonghah/esri2sf"), # install.packages("pbapply")
library(maptiles) # for the basemap
library(tidyverse)

url_sccwrp_sediment_chem = "https://services1.arcgis.com/gfvMa5URH3rFRQ7T/ArcGIS/rest/services/Bight13SedimentChemistryHarmonized2/FeatureServer/0" # did not have TRC data
url_sccwrp_sediment_chem_2 = "https://services1.arcgis.com/gfvMa5URH3rFRQ7T/arcgis/rest/services/Bight_18_Sediment_Chemistry_Harmonized2/FeatureServer/0" # no
url_sccwrp_sediment_chem_24 = "https://gis.sccwrp.org/arcserver/rest/services/Bight03SedimentChemistryUnified/FeatureServer/0" # no
url_sccwrp_sediment_chem_99 = "https://gis.sccwrp.org/arcgis/rest/services/Bight94SedimentChemistryUnified/FeatureServer/0" # no data
url_sccwrp_sediment_chem_130 = "https://services1.arcgis.com/gfvMa5URH3rFRQ7T/arcgis/rest/services/Chemistry_and_Station_Combined/FeatureServer/0" #no
url_sccwrp_sediment_chem_134 = "https://services1.arcgis.com/gfvMa5URH3rFRQ7T/arcgis/rest/services/qrytblAirFilteringVolumes/FeatureServer/0" # no
url_sccwrp_sediment_chem_135 = "https://services1.arcgis.com/gfvMa5URH3rFRQ7T/arcgis/rest/services/Atmospheric_Deposition_of_Pollutants_to_Santa_Monica_Bay_2000_Measurement/FeatureServer/0" #issues with esri2sf
url_sccwrp_sediment_chem_136 = "https://services1.arcgis.com/gfvMa5URH3rFRQ7T/arcgis/rest/services/Atmospheric_Deposition_of_Pollutants_to_Santa_Monica_Bay_2000_Field_Samples/FeatureServer/0" # no data
url_sccwrp_sediment_chem_138 = "https://services1.arcgis.com/gfvMa5URH3rFRQ7T/arcgis/rest/services/Atmospheric_Deposition_of_Pollutants_to_Santa_Monica_Bay_2000_Chemistry/FeatureServer/0" #no data

url_gis_basins = "https://utility.arcgis.com/usrsvcs/servers/49807a1fbc584631bdf88d9ca71dd083/rest/services/Geoscientific/i08_B118_CA_GroundwaterBasins/MapServer/0"
url_gis_water_districts = "https://gis.water.ca.gov/arcgis/rest/services/Boundaries/i03_WaterDistricts/MapServer/0"
url_hydrologic_regions = "https://gis.water.ca.gov/arcgis/rest/services/Boundaries/i03_Hydrologic_Regions/MapServer/0"

gis_hydrologic_regions = saveSHP(url_hydrologic_regions)
saveSHP = function(url, outputName = "") {

  attempt = try(features <- esri2df(url))
  if(inherits(attempt, "try-error")) {
    print(paste0("Error using esri2df: ", attempt))
    attempt2 = try(features <- esri2sf(url))
    if(inherits(attempt2, "try-error")) {
      print(paste0("Error using esri2sf: ", attempt2))
      attempt3 = try(st_read(paste0(url, "/query?where=1=1&outFields=*&f=json")))
      if(inherits(attempt3, "try-error")) {
        print(paste0("Error using st_read (stopping here): ", attempt3))
      } else {
        features = attempt3
      }
    }
  }

  if (outputName != "") {
    outputFile = paste0(outputName, ".shp")
    sf::st_write(features, outputFile, append = FALSE)
  }
  # 
  # # get the basemap layer for this region, to show off!
  # basemap = get_tiles(features, provider="OpenStreetMap", crop=TRUE)
  # # show what we've created for confirmation
  # print(
  #   ggplot() +
  #     ggspatial::layer_spatial(basemap) +
  #     geom_sf(data=features)+
  #     theme_minimal()
  # )
  
  return(features)
}

View(sccwrp_sediment_chem)

# plot it all
basemap = get_tiles(scm_points_2020, provider="OpenStreetMap", crop=TRUE)

ggplot() +
  ggspatial::layer_spatial(basemap)+
  geom_sf(data=seasonal_wetlands_2017)+
  geom_sf(data=infiltration_features_2020)+
  geom_sf(data=scm_points_2020)+
  labs(title="UCSB Stormwater Control Measures")+
  theme_minimal()

### merge into 1 SHP

# maintain source for easy filtering later on
scm_points_2020$source = "SCM Points (2020)"
infiltration_features_2020$source = "Infiltration Features (2020)"
seasonal_wetlands_2017$source = "Seasonal Wetlands (2017)"

merged_features = dplyr::bind_rows(seasonal_wetlands_2017, infiltration_features_2020, scm_points_2020)
st_write(merged_features, "scm_features.shp")
