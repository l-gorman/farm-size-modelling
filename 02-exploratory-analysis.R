
# Setup -------------------------------------------------------------------
library(brms) 
library(ggplot2)
library(parallel)
library(optparse)
library(magrittr)
library(bayesplot)
set.seed(404)

option_list = list(
  make_option(c("-i", "--iter"),  type='integer',
              help="Iterations"),
  make_option(c("-d", "--directory"), type='character',
              help="The directory where the file will be stored and loaded from")
  
  
)
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);

opt <- list(
  iter=2000,
  directory="iter_comparison"
)

if (opt$iter==2000){
  warmup <- 1000
}
if (opt$iter==5000){
  warmup <- 2000
}
if (opt$iter==10000){
  warmup <- 4000
}


if (is.null(warmup)){
  stop("need to select a number of warmup iterations")
}


# Data Loading and Prep ------------------------------------------------

final_df <- readr::read_csv("./data/prepared-data/final-modelling-dataset.csv")


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



# Directory Creation ------------------------------------------------------


main_folder <- paste0("./outputs/",opt$directory)
sub_folder <- paste0("./outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter)

conv_folder <- paste0("./outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter,"/simple_regression")
dist_folder <- paste0("./outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter,"/free_variance")
lss_folder <- paste0("./outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter,"/location_scale_shape")


dir.create(path=main_folder,showWarnings = F)
dir.create(path=sub_folder,showWarnings = F)

dir.create(path=conv_folder,showWarnings = F)
dir.create(path=dist_folder,showWarnings = F)
dir.create(path=lss_folder,showWarnings = F)




# Conventional Model ------------------------------------------------------

conv_fm <- bf(
  #farm_size_ha | trunc(lb = 0) ~ 0 + Intercept + level_2_aez_33_classes_desert_or_arid_climate + barren + length_growing_season + ndvi + healthcare_traveltime
  
  farm_size_ha  ~ 0 + Intercept + level_2_aez_33_classes_desert_or_arid_climate + barren + length_growing_season + ndvi + healthcare_traveltime
)

sample <- 5000
# brms::get_prior(conv_fm,final_df[1:sample,])

conv_brm <-brm(conv_fm, 
    data = final_df[1:sample,], 
    cores = 4,
    chains = 4,
    control=list(adapt_delta = 0.9, max_treedepth=12),
    iter=opt$iter,
    warmup=warmup,
    init = 0,
    seed = 404,
    prior = c(
      prior("normal(0, 10)", class = b, coef = Intercept),
      # prior("normal(0, 10)", class = Intercept),
      prior("normal(0, 100)", class = b, coef = level_2_aez_33_classes_desert_or_arid_climate),
      prior("normal(0, 100)", class = b, coef = barren),
      prior("normal(0, 100)", class = b, coef = length_growing_season),
      prior("normal(0, 100)", class = b, coef = ndvi),
      prior("normal(0, 100)", class = b, coef = healthcare_traveltime),
      
      prior("normal(0, 10)", class = sigma)
    )
      
    
)

bayesplot::mcmc_trace(conv_brm)

# Predict on original data
conv_post_pred <- predict(conv_brm, newdata = final_df[1:sample,]) %>% as_tibble()

# density_lower_bound <- density(conv_post_pred$Q2.5) 
# density_upper_bound <- density(conv_post_pred$Q97.5) 
# 
# density_bounds <- tibble::as_tibble(list(
#   x=density_lower_bound$x,
#   ymin=density_lower_bound$y,
#   ymax=density_upper_bound$y
#   
# ))



conv_plot <- ggplot() +
  geom_density(data=final_df, aes(x=farm_size_ha, color="Actual Distribuition"), )+
  geom_density(data=conv_post_pred, aes(x=Estimate, color="Predictions (on Same Data)"))+
  scale_colour_manual(c("",""),values=c("red","blue"))
  







#' Validate with K-FOLD cross-validation:
#' Use "projects" or "locations" as part of the 
#' "groups argument".
#' http://paul-buerkner.github.io/brms/reference/kfold.html






