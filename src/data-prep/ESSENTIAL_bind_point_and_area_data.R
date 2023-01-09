library(readr)
# Spatial Packages
library(sf)
library(stars)
library(terra)
library(raster)


# Preparing LSMS Data
lsms_data <- readr::read_csv("data/raw-data/lsms/farm-size-all-points.csv", na=c("-999","NA", "n/a"))
lsms_data <- lsms_data[!is.na(lsms_data$latitude) & !is.na(lsms_data$longitude),]
lsms_data <- st_as_sf(lsms_data, coords = c("longitude", "latitude"), 
                      crs = 4326, agr = "constant", remove = F)

# Preparing RHoMIS Data
rhomis_data <- readr::read_csv("data/raw-data/rhomis/processed_data.csv", na=c("-999","NA", "n/a"))
indicator_data <- readr::read_csv("data/raw-data/rhomis/indicator_data.csv", na=c("-999","NA", "n/a"))
indicator_data <- indicator_data %>% merge(rhomis_data[c("id_unique","gps_lat", "gps_lon")],by="id_unique")
indicator_data <- indicator_data[!is.na(indicator_data$gps_lat) & !is.na(indicator_data$gps_lon),]
indicator_data <- st_as_sf(indicator_data, coords = c("gps_lon", "gps_lat"), 
                      crs = 4326, agr = "constant", remove = F)


# Reading the FAO data
fao_level_2 <- geojsonsf::geojson_sf("./data/prepared-data/fao_level_2.geojson")

joined_lsms_df <- st_join(x=lsms_data, 
                     y=fao_level_2,
                     left=T)

joined_rhomis_df <- st_join(x=indicator_data, 
                          y=fao_level_2,
                          left=T)
readr::write_csv(joined_lsms_df, paste0("data/prepped-data/lsms-ee-gaez.csv"))
readr::write_csv(joined_rhomis_df, paste0("data/prepped-data/rhomis-ee-gaez.csv"))




