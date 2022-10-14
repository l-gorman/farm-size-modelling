#' Exploring and Analysing Farm-Size Dsitributions
# -------------------------------------------------------------------------------------------------------------
# Loading Libraries -------------------------------------------------------------------------------------------
# -------------------------------------------------------------------------------------------------------------
# Data Cleaning and reformatting
library(readr)
library(dplyr)
library(tibble)
library(jsonlite)

#' Main analysis script
#' here is 
library(tidyr)



# Graph Plotting
library(ggplot2)
library(ggridges)
library(RColorBrewer)
library(corrplot)


library(brms) # General package for bayesian models with lme4 syntax and stan backend
library(FactoMineR) # Package for pca
library(factoextra) # Extra pca features




# Reading in Data ---------------------------------------------------------

farm_size_data <- readr::read_csv("./data/prepared-data/lsms-ee-gaez.csv")
farm_size_data <- farm_size_data[!is.na(farm_size_data$longitude) & !is.na(farm_size_data$latitude),]
farm_size_data_geo <- st_as_sf(farm_size_data, coords = c("longitude", "latitude"), 
                               crs = 4326, agr = "constant", remove = F)

# FAO administrative data
fao_level_2 <- geojson_sf('data/prepared-data/fao_level_2.geojson')
fao_level_2 <- sf::st_as_sf(x = fao_level_2, wkt = "geometry")
fao_level_2_geo <-st_set_crs(fao_level_2,'EPSG:4326')


fao_level_2 <- tibble::as_tibble(fao_level_2)


land_categories <-  readr::read_csv("./data/prepared-data/land_cover_classes.csv")

farm_size_data <- farm_size_data %>% merge(land_categories,
                                            by.x="land_cover_point",
                                            by.y="Value",
                                            all.x=T,
                                            all.y=F)
farm_size_data <- farm_size_data %>% dplyr::rename(point_land_cover_description=Description)
farm_size_data <- farm_size_data %>% dplyr::rename(point_land_cover_tag=Tag)

farm_size_data <- farm_size_data[!is.na(farm_size_data$farm_size_ha),]
farm_size_data <- farm_size_data[!is.na(farm_size_data$AEZ_Classes_33),]

land_cat_columns <- paste0("land_cat_",c(1:17))

farm_size_data[land_cat_columns] <- lapply(farm_size_data[land_cat_columns] , function(column){
  column[is.na(column)] <- 0
  # Dividing the number of pixels by total pixel count for that area
  column <- column/farm_size_data$pixelCount
  return(column)
}) %>% dplyr::bind_cols()

fao_level_2[land_cat_columns] <- lapply(fao_level_2[land_cat_columns] , function(column){
  column <- as.numeric(column)
  column[is.na(column)] <- 0
  # Dividing the number of pixels by total pixel count for that area
  column <- column/fao_level_2$pixelCount
  return(column)
}) %>% dplyr::bind_cols()


new_land_cat_columns <- land_categories$Tag
colnames(farm_size_data)[colnames(farm_size_data) %in% land_cat_columns] <- new_land_cat_columns
colnames(fao_level_2)[colnames(fao_level_2) %in% land_cat_columns] <- new_land_cat_columns

colnames(farm_size_data)[colnames(farm_size_data) == "accessibility_mean"] <-"healthcare_traveltime"
colnames(farm_size_data)[colnames(farm_size_data) == "b1_mean_mean"] <- "nightlights"
colnames(farm_size_data)[colnames(farm_size_data) == "population_density_mean_mean"] <- "population_density"

colnames(farm_size_data)[colnames(farm_size_data) == "elevation_mean"] <- "elevation"
colnames(farm_size_data)[colnames(farm_size_data) == "NDVI_mean_mean"] <- "ndvi"
colnames(farm_size_data)[colnames(farm_size_data) == "constant_mean"] <- "topographic_diversity"

colnames(farm_size_data)[colnames(farm_size_data) == "level_2_mean_length_growing_season"] <- "length_growing_season"

colnames(fao_level_2)[colnames(fao_level_2) == "accessibility_mean"] <-"healthcare_traveltime"
colnames(fao_level_2)[colnames(fao_level_2) == "b1_mean_mean"] <- "nightlights"
colnames(fao_level_2)[colnames(fao_level_2) == "population_density_mean_mean"] <- "population_density"

colnames(fao_level_2)[colnames(fao_level_2) == "elevation_mean"] <- "elevation"
colnames(fao_level_2)[colnames(fao_level_2) == "NDVI_mean_mean"] <- "ndvi"
colnames(fao_level_2)[colnames(fao_level_2) == "constant_mean"] <- "topographic_diversity"

colnames(fao_level_2)[colnames(fao_level_2) == "level_2_mean_length_growing_season"] <- "length_growing_season"


farm_size_data$geo_id <- paste0(farm_size_data$ADM0_CODE, "_", 
                                farm_size_data$ADM1_CODE, "_",
                                farm_size_data$ADM2_CODE)

fao_level_2$geo_id <- paste0(fao_level_2$ADM0_CODE, "_", 
                             fao_level_2$ADM1_CODE, "_",
                             fao_level_2$ADM2_CODE)





# Outlier Detection -------------------------------------------------------


outlier_filter <- quantile(farm_size_data[["farm_size_ha"]], probs = c(0.01,0.99))

table(farm_size_data[c("farm_size_ha")]==outlier_filter[1]) # 236 with 0 land cult (investigate further)
table(farm_size_data[c("farm_size_ha")]>outlier_filter[2]) # 201 with land cult greater than 99th percentile (24th)


farm_size_data <- farm_size_data[farm_size_data[c("farm_size_ha")] != outlier_filter[1] & farm_size_data[c("farm_size_ha")] <= outlier_filter[2],]



# Correlation Plots -------------------------------------------------------

x <- c("healthcare_traveltime",
       "nightlights",
       "population_density",
       
       
       "elevation",
       "ndvi",
       "topographic_diversity",
       "length_growing_season",
       
       
       
       
       
       new_land_cat_columns
)

aez_33 <- grep("level_2_aez_33_classes_", colnames(farm_size_data), value=T)




y <-c("farm_size_ha")


# Looking at subnational indicators vas land cultivated
cor(farm_size_data[c(x,aez_33,y)])


farm_size_data[aez_33]

corr_matrix <- round(cor(farm_size_data[c(x,aez_33,y)]),2) %>% tibble::as_tibble()
corr_matrix$var <- colnames(corr_matrix)
corr_matrix <- corr_matrix %>% pivot_longer(cols = colnames(corr_matrix)[colnames(corr_matrix)!="var"])

colnames(corr_matrix) <- c("var1", "var2", "value")


land_cult_corr <- corr_matrix[corr_matrix$var1=="farm_size_ha", ]
land_cult_corr <- land_cult_corr[land_cult_corr$var2!="farm_size_ha",]

land_cult_corr$var2 <- gsub("level_2_aez_33_classes_", "",land_cult_corr$var2)

land_cult_corr$var2 <- factor(land_cult_corr$var2, levels=c(x,gsub("level_2_aez_33_classes_", "",aez_33)), ordered = T)
land_cult_corr$factor_level <- as.numeric(land_cult_corr$var2)

socio_economic <- c("healthcare_traveltime",
                    "nightlights",
                    "population_density")

environmental <- c("elevation",
                   "ndvi",
                   "topographic_diversity",
                   "length_growing_season")



land_cat <- new_land_cat_columns

land_cult_corr$var_group <- NA
land_cult_corr$var_group[land_cult_corr$var2 %in% socio_economic] <- "Socio-Economic"
land_cult_corr$var_group[land_cult_corr$var2 %in% environmental] <- "Environmental"
land_cult_corr$var_group[land_cult_corr$var2 %in% land_cat] <- "Land-Cover-Class"
land_cult_corr$var_group[land_cult_corr$var2 %in% gsub("level_2_aez_33_classes_", "",aez_33)] <- "Agro-Eco-Zone"

land_cult_corr$var_group <- factor(land_cult_corr$var_group, 
                                   levels=c("Agro-Eco-Zone",
                                            "Land-Cover-Class",
                                            "Environmental",
                                            "Socio-Economic"),
                                   ordered=T, )

land_cult_corr <- land_cult_corr[!is.na(land_cult_corr$value),]

colors <- brewer.pal(length(unique(land_cult_corr$var_group)), name="Dark2")


land_cult_corr <- land_cult_corr %>%
  mutate(., color = with(., case_when(
    (var_group=="Socio-Economic") ~ colors[4],
    (var_group=="Environmental")  ~ colors[3],
    (var_group=="Land-Cover-Class")  ~ colors[2],
    (var_group=="Agro-Eco-Zone")  ~ colors[1]
  )))


ggplot(data = land_cult_corr, aes(y=var2, x=value)) + 
  geom_segment( aes(x=0, xend=value, y=var2, yend=var2), color="black")+
  geom_point( size=4, aes(x=value, color=var_group)) +
  scale_color_manual(values=colors) +
  
  coord_cartesian(xlim = c(-0.3, 0.3), # This focuses the x-axis on the range of interest
                  clip = 'off')+
  
  labs(title="Correlations with Land Cultivated (ha)",
       x ="Pearsons Correlation Coeff", y ="Landscape predictiors",
       color="Variable Class",
       caption = "\nLandscape predictors sourced from GAEZ v4 and Google Earth Engine.
     Environmental, land-cover, and socio-economic data aggregated to FAO GAUL level 2.
     Lenght growing period and AEZ classes extracted at the point level.
     AEZ simplified 33 class has been used, only 10/33 AEZs featured in the data.
     Correlations represent correlation between household level land cultivated (ha)
     and aggregated landscape variable.
     Outliers have been removed using 99th percentile (>24ha)*
     ") +
  theme(text = element_text(size = 16),
        axis.text.y = element_text(colour = land_cult_corr$color)
  )           


land_cult_corr <- land_cult_corr[order(sqrt(land_cult_corr$value^2),decreasing = T),]


# Initial Modelling -------------------------------------------------------

write_csv(farm_size_data,"./data/prepared-data/final-modelling-dataset.csv")


