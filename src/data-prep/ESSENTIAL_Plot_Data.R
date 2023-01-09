library(readr)
library(dplyr)
library(tidyr)
library(tibble)
library(jsonlite)
library(XML)
library(fastDummies)
library(magrittr)

# Spatial Packages
library(sf)
library(stars)
library(terra)
library(raster)

library(sp)
library(geojsonsf)
library(corrplot)
# Graph Plotting
library(ggplot2)
library(RColorBrewer)
library(spData)

library(spData)

opt <- list()
opt$directory <- "./"

fao_level_2 <- geojsonsf::geojson_sf("./data/prepared-data/fao_level_2.geojson")
fao_level_2 <- fao_level_2[fao_level_2[["ADM0_NAME"]]=="Uganda",]

lsms_data <- readr::read_csv("./data/lsms/farm-size-all-points.csv", na=c("-999","NA", "n/a"))
lsms_data <- lsms_data[!is.na(lsms_data$latitude) & !is.na(lsms_data$longitude),]
lsms_data <- st_as_sf(lsms_data, coords = c("longitude", "latitude"), 
         crs = 4326, agr = "constant", remove = F)


joined_df <- st_join(x=lsms_data, 
                     y=fao_level_2,
                     left=T)

joined_df <- joined_df[!is.na(joined_df$polygon_index),]



aez_33_classes <- raster::raster(x = paste0(opt$directory,"data/gaez/33_classes.tif"))
adjusted_length_growing_period  <- raster(paste0(opt$directory,"data/aez/gaez_v4_57_class/adjusted_length_growing_period.tif"))
adjusted_length_growing_period <- projectRaster(adjusted_length_growing_period,aez_33_classes)

fao_level_2_outline <- fao_level_2 %>% 
  summarise(do_union = T) 



spatial_fao_outline <- as(fao_level_2_outline, "Spatial")
spatial_fao_outline <- spTransform(spatial_fao_outline,crs(adjusted_length_growing_period))




adjusted_length_growing_period <- raster::crop(adjusted_length_growing_period,extent(spatial_fao_outline))
adjusted_length_growing_period <- raster::mask(adjusted_length_growing_period,spatial_fao_outline)

adjusted_length_growing_period_values
fao_level_2$level_2_mean_length_growing_season
# Subnational Data
subnatinal_plot  <- ggplot()+
  # geom_sf(data=world[world$iso_a2=="KE",])+
  geom_sf(data=fao_level_2, aes(fill=level_2_mean_length_growing_season))+
  scale_fill_viridis_c()+
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
        )


coords <- xyFromCell(adjusted_length_growing_period, seq_len(ncell(adjusted_length_growing_period)))
adjusted_length_growing_period_values <- stack(as.data.frame(getValues(adjusted_length_growing_period)))

adjusted_length_growing_period_df <- cbind(coords, adjusted_length_growing_period_values)
adjusted_length_growing_period_df <- adjusted_length_growing_period_df[!is.na(adjusted_length_growing_period_df$values),]
ggplot(adjusted_length_growing_period_df) + 
  geom_tile(aes(x, y, fill = values)) +
  scale_fill_viridis_c()+
  coord_equal()+
  xlab("")+
  ylab("")+
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        axis.text.x = element_blank(),
        panel.background = element_rect(fill = "white", colour = NA),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )




subnatinal_plot_and_households  <- ggplot()+
  # geom_sf(data=world[world$iso_a2=="KE",])+
  geom_sf(data=fao_level_2_outline, fill="white")+
  geom_sf(data=joined_df)+
  
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )


subnatinal_plot_and_households  <- ggplot()+
  # geom_sf(data=world[world$iso_a2=="KE",])+
  geom_sf(data=fao_level_2, aes(fill=level_2_mean_length_growing_season))+
  scale_fill_viridis_c()+
  geom_sf(data=joined_df)+
  
  theme_bw() + 
  theme(panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(),
        axis.text.x = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks = element_blank()
  )





