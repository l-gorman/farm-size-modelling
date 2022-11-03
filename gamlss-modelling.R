library(gamlss)
library()


final_df <- readr::read_csv(paste0(opt$base,"prepared-data/final-modelling-dataset.csv"))


land_cover_columns <-c("evergreen_needle_leaf",
                       "evergreen_broad_leaf",
                       "deciduous_needle_leaf",
                       "deciduous_broad_leaf",
                       "mixed_forest",
                       "closed_shrubland",
                       "open_shrubland",
                       "woody_savana",
                       "savana",
                       "grass_land",
                       "wetland",
                       "cropland",
                       "urban",
                       "crop_natural_mosaic",
                       "snow_ice",
                       "barren",
                       "water_bodies")

aez_columns <- c("level_2_aez_33_classes_tropics_lowland_sub_humid",
                 "level_2_aez_33_classes_tropics_highland_sub_humid",
                 "level_2_aez_33_classes_land_with_severe_soil_or_terrain_limitations",
                 "level_2_aez_33_classes_tropics_lowland_semi_arid",
                 "level_2_aez_33_classes_tropics_highland_semi_arid",
                 "level_2_aez_33_classes_dominantly_water",
                 "level_2_aez_33_classes_tropics_highland_humid",
                 "level_2_aez_33_classes_tropics_lowland_humid",
                 "level_2_aez_33_classes_dominantly_hydromorphic_soils",
                 "level_2_aez_33_classes_dominantly_very_steep_terrain",
                 "level_2_aez_33_classes_no_valid_pixels",
                 "level_2_aez_33_classes_land_with_ample_irrigated_soils",
                 "level_2_aez_33_classes_cold_no_permafrost_wet",
                 "level_2_aez_33_classes_dominantly_built_up_land",
                 "level_2_aez_33_classes_desert_or_arid_climate",
                 "level_2_aez_33_classes_cold_no_permafrost_moist",
                 "level_2_aez_33_classes_sub_tropics_warm_semi_arid")

x <- c("healthcare_traveltime",
       "nightlights",
       "population_density",
       
       
       "elevation",
       "ndvi",
       "topographic_diversity",
       "length_growing_season"
)



# Rescaling  --------------------------------------------------------------

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

for (column in c(land_cover_columns,aez_columns,x)){
  final_df[[column]]<- range01(final_df[[column]])
  
}

table(complete.cases(final_df))
colSums(is.na(final_df))

# Testing with Skew Normal Model ------------------------------------------

gamlss::gam

formula <- as.formula(paste0(" ~ ", paste0(x, collapse=" + ")))
y_formula <- as.formula(paste0(paste0("farm_size_ha ~ ", paste0(x, collapse=" + "))))


flat_model <- gamlss(farm_size_ha ~1, 
       sigma.formula = ~1, 
       nu.formula = ~1, 
       tau.formula = ~1,
       data =final_df[,c(x,"farm_size_ha")],  
       family=BCT(),
       control = gamlss.control(n.cyc = 200)
)
plot(flat_model)

location_model <- gamlss(y_formula, 
                     sigma.formula =~1, 
                     nu.formula = ~1, 
                     tau.formula = ~1,
                     data =final_df[,c(x,"farm_size_ha")],  
                       family=BCTo(),
                     control = gamlss.control(n.cyc = 200)
)

plot(location_model)


location_scale_model <- gamlss(y_formula, 
                         sigma.formula = formula, 
                         nu.formula = ~1, 
                         tau.formula = ~1,
                         data =final_df[,c(x,"farm_size_ha")],  
                         family=BCTo(),
                         control = gamlss.control(n.cyc = 200)
)
plot(location_scale_model)

location_scale_skew_model <- gamlss(y_formula, 
                               sigma.formula = formula, 
                               nu.formula =  formula, 
                               tau.formula = ~1,
                               data =final_df[,c(x,"farm_size_ha")],  
                               family=BCTo(),
                               control = gamlss.control(n.cyc = 200)
)
plot(location_scale_skew_model)

location_scale_skew_kurtosis_model <- gamlss(y_formula, 
                                    sigma.formula = formula, 
                                    nu.formula =  formula, 
                                    tau.formula = formula,
                                    data =final_df[,c(x,"farm_size_ha")],  
                                    family=BCTo(),
                                    control = gamlss.control(n.cyc = 200)
)
plot(location_scale_skew_kurtosis_model)
summary(location_scale_skew_kurtosis_model)












