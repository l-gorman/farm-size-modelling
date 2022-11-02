
# Setup -------------------------------------------------------------------
library(brms) 
library(tibble)
library(ggplot2)
library(parallel)
library(optparse)
library(magrittr)
library(bayesplot)
library(future)
set.seed(404)




option_list = list(
  make_option(c("-i", "--iter"),  type='integer',
              help="Iterations"),
  make_option(c("-n", "--number"),  type='integer',
              help="Number of hhs to sample"),
  make_option(c("-b", "--base"), type='character',
              help="Base directory where files will be loaded from"),
  make_option(c("-d", "--directory"), type='character',
              help="The directory where the file will be saved"),
  make_option(c("-c", "--ncores"), type='character',
              help="The number of chains/cores")
  

  
  
)
opt_parser = OptionParser(option_list=option_list);
opt = parse_args(opt_parser);
# 
# opt <- list(
#   iter=2000,
#   number=5000,
#   directory="initial_comparison",
#   base="./",
#   ncores="4"
# )

# ncores = 4

chains = 4

if (opt$iter==2000){
  warmup <- 1000
}
if (opt$iter==5000){
  warmup <- 2000
}
if (opt$iter==10000){
  warmup <- 4000
}

if (opt$iter==20000){
  warmup <- 8000
}
if (opt$iter==50000){
  warmup <- 20000
}



if (is.null(warmup)){
  stop("need to select a number of warmup iterations")
}


# Data Loading and Prep ------------------------------------------------

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


# Directory Creation ------------------------------------------------------

if (opt$number=="MAX"){
  opt$number <- nrow(final_df)
}

main_folder <- paste0(opt$base,"outputs/",opt$directory)
sub_folder <- paste0(opt$base,"outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter)

conv_folder <- paste0(opt$base,"outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter,"/simple_regression")
dist_folder <- paste0(opt$base,"outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter,"/free_variance")
lss_folder <- paste0(opt$base,"outputs/",opt$directory,"/n_",opt$number, "_iter_",opt$iter,"/location_scale_shape")


dir.create(path=paste0(opt$base,"outputs/"),showWarnings = F)

dir.create(path=main_folder,showWarnings = F)
dir.create(path=sub_folder,showWarnings = F)

dir.create(path=conv_folder,showWarnings = F)
dir.create(path=dist_folder,showWarnings = F)
dir.create(path=lss_folder,showWarnings = F)



# subsetting data ---------------------------------------------------------
sample <- sample(c(1:nrow(final_df)), opt$number)

sample_df <-  final_df[sample,]

# Conventional Model ------------------------------------------------------

conv_fm <- bf(
  #farm_size_ha | trunc(lb = 0) ~ 0 + Intercept + level_2_aez_33_classes_desert_or_arid_climate + barren + length_growing_season + ndvi + healthcare_traveltime
  
  farm_size_ha  ~ 0 + Intercept + level_2_aez_33_classes_desert_or_arid_climate + barren + length_growing_season + ndvi + healthcare_traveltime
)

sample <- 5000
# brms::get_prior(conv_fm,final_df[1:sample,])

conv_brm <-brm(conv_fm, 
    data = sample_df, 
    cores = chains,
    chains = chains,
    control=list(adapt_delta = 0.8, max_treedepth=10),
    iter=opt$iter,
    warmup=warmup,
    init = 0,
    seed = 404,
    prior = c(
      prior("normal(0, 10)", class = b, coef = Intercept),
      # prior("normal(0, 10)", class = Intercept),
      prior("normal(0, 10)", class = b, coef = level_2_aez_33_classes_desert_or_arid_climate),
      prior("normal(0, 10)", class = b, coef = barren),
      prior("normal(0, 10)", class = b, coef = length_growing_season),
      prior("normal(0, 10)", class = b, coef = ndvi),
      prior("normal(0, 10)", class = b, coef = healthcare_traveltime),
      
      prior("normal(0, 10)", class = sigma)
    )
      
    
)

save(conv_brm, file = paste0(conv_folder,"/distributional_fit.rda"))

png(filename = paste0(conv_folder,"/mcmc_plot.png"),width = 1000,height=1000)
bayesplot::mcmc_trace(conv_brm,)
dev.off()

png(filename = paste0(conv_folder,"/pp_check.png"), width = 1000,height=1000)
pp_check(conv_brm)
dev.off()

sink(paste0(conv_folder,"/fit_diagnostics.txt"))
cat("\n")
cat(paste0("Fit Summary\n"))
summary(conv_brm)
cat("\n")
cat(paste0("Prior Summary\n"))
prior_summary(conv_brm)
cat("\n")
sink()



# Predict on original data
# conv_post_pred <- predict(conv_brm, newdata = sample_df) %>% as_tibble()

# density_lower_bound <- density(conv_post_pred$Q2.5) 
# density_upper_bound <- density(conv_post_pred$Q97.5) 
# 
# density_bounds <- tibble::as_tibble(list(
#   x=density_lower_bound$x,
#   ymin=density_lower_bound$y,
#   ymax=density_upper_bound$y
#   
# ))


# 
# conv_plot <- ggplot() +
#   geom_density(data=final_df, aes(x=farm_size_ha, color="Actual Distribuition"), )+
#   geom_density(data=conv_post_pred, aes(x=Estimate, color="Predictions (on Same Data)"))+
#   scale_colour_manual(c("",""),values=c("red","blue"))
#   


# Distributional Model ----------------------------------------------------



dist_fm <- bf(
  farm_size_ha  ~ 0 + Intercept + level_2_aez_33_classes_desert_or_arid_climate + barren + length_growing_season + ndvi + healthcare_traveltime,
  sigma ~ 0 + Intercept + level_2_aez_33_classes_desert_or_arid_climate + barren + length_growing_season + ndvi + healthcare_traveltime
)

# get_prior(dist_fm,sample_df)
dist_brm <- brm(dist_fm, 
                data = sample_df,
                cores = chains,
                chains = chains,
                control=list(adapt_delta = 0.8, max_treedepth=10),
                iter = opt$iter,
                
                init = 0,
                seed = 404,
                prior = c(
                  # Location Parameter Intercept
                  prior("normal(0, 10)", class = b,coef = Intercept),
                  
                  # Coefficients of Location Parameters
                  prior("normal(0, 10)", class = b, coef = barren),
                  prior("normal(0, 10)", class = b, coef = healthcare_traveltime),
                  prior("normal(0, 10)", class = b, coef = length_growing_season),
                  prior("normal(0, 10)", class = b, coef = level_2_aez_33_classes_desert_or_arid_climate),
                  prior("normal(0, 10)", class = b, coef = ndvi),

                  
                  # Scale Parameter Intercept
                  prior("normal(0, 10)", coef=Intercept,dpar = sigma),
                  
                  # Coefficients of Scale Parameters
                  prior("normal(0, 10)", dpar = sigma, coef = barren),
                  prior("normal(0, 10)", dpar = sigma, coef = healthcare_traveltime),
                  prior("normal(0, 10)", dpar = sigma, coef = length_growing_season),
                  prior("normal(0, 10)", dpar = sigma, coef = level_2_aez_33_classes_desert_or_arid_climate),
                  prior("normal(0, 10)", dpar = sigma, coef = ndvi)

                ))


save(dist_brm, file = paste0(dist_folder,"/distributional_fit.rda"))

png(filename = paste0(dist_folder,"/mcmc_plot.png"),width = 1000,height=1000)

bayesplot::mcmc_trace(dist_brm)
dev.off()

png(filename = paste0(dist_folder,"/pp_check.png"),width = 1000,height=1000)
pp_check(dist_brm)
dev.off()


sink(paste0(dist_folder,"/fit_diagnostics.txt"))

cat("\n")
cat(paste0("Fit Summary\n"))
summary(dist_brm)
cat("\n")
cat(paste0("Prior Summary\n"))
prior_summary(dist_brm)
cat("\n")
sink()



# Model For Location Scale and Shape --------------------------------------




lss_fm <- bf(
  farm_size_ha  ~ 0 + Intercept + level_2_aez_33_classes_desert_or_arid_climate + barren + length_growing_season + ndvi + healthcare_traveltime,
  sigma ~ 0 + Intercept + level_2_aez_33_classes_desert_or_arid_climate + barren + length_growing_season + ndvi + healthcare_traveltime,
  alpha ~ 0 + Intercept + level_2_aez_33_classes_desert_or_arid_climate + barren + length_growing_season + ndvi + healthcare_traveltime
)

# get_prior(lss_fm,sample_df, family = skew_normal())
lss_brm <- brm(lss_fm, 
               data = sample_df, 
               family = skew_normal(
                 link = "identity", 
                 link_sigma = "log", 
                 link_alpha = "identity"),
               control=list(adapt_delta = 0.8, max_treedepth=10),
               cores = chains,
               chains =  chains,
               iter = opt$iter,
               prior = c(
                 
                 # Location Parameter Intercept
                 prior("normal(0, 10)", coef = Intercept, class=b),
                 
                 # Coefficients of Location Parameters
                 prior("normal(0, 10)", class = b, coef = barren),
                 prior("normal(0, 10)", class = b, coef = healthcare_traveltime),
                 prior("normal(0, 10)", class = b, coef = length_growing_season),
                 prior("normal(0, 10)", class = b, coef = level_2_aez_33_classes_desert_or_arid_climate),
                 prior("normal(0, 10)", class = b, coef = ndvi),
                 
                 # Scale Parameter Intercept
                 prior("normal(0, 10)", coef=Intercept ,dpar = sigma),
                 
                 # Coefficients of Scale Parameter
                 prior("normal(0, 10)", dpar = sigma, coef = barren),
                 prior("normal(0, 10)", dpar = sigma, coef = healthcare_traveltime),
                 prior("normal(0, 10)", dpar = sigma, coef = length_growing_season),
                 prior("normal(0, 10)", dpar = sigma, coef = level_2_aez_33_classes_desert_or_arid_climate),
                 prior("normal(0, 10)", dpar = sigma, coef = ndvi),  
                 
                 # Scale Parameter Intercept
                 prior("normal(0, 10)", coef=Intercept ,dpar = alpha),
                 
                 
                 # Coefficients of Shape Parameter
                 prior("normal(0, 10)", dpar = alpha, coef = barren),
                 prior("normal(0, 10)", dpar = alpha, coef = healthcare_traveltime),
                 prior("normal(0, 10)", dpar = alpha, coef = length_growing_season),
                 prior("normal(0, 10)", dpar = alpha, coef = level_2_aez_33_classes_desert_or_arid_climate),
                 prior("normal(0, 10)", dpar = alpha, coef = ndvi)                   
               ),init = 0,
               seed = 404
               
)

save(lss_brm, file = paste0(lss_folder,"/distributional_fit.rda"))

png(filename = paste0(lss_folder,"/mcmc_plot.png"),width = 1000,height=1000)
bayesplot::mcmc_trace(lss_brm)
dev.off()


png(filename = paste0(lss_folder,"/pp_check.png"),width = 1000,height=1000)
pp_check(lss_brm)
dev.off()

sink(paste0(lss_folder,"/fit_diagnostics.txt"))
cat("\n")
cat(paste0("Fit Summary\n"))
summary(lss_brm)
cat("\n")
cat(paste0("Prior Summary\n"))
prior_summary(lss_brm)
cat("\n")
sink()




# Validation --------------------------------------------------------------
# Rprof(tf <- "rprof.log", memory.profiling=TRUE)
# 
# loo_res <- brms::loo(conv_brm, dist_brm)#, lss_brm)
# Rprof(NULL)
# summaryRprof(tf)
#' Could use the "groups" param
#' to perform kfold ax
#' 
plan(multiprocess)
kfold_res <- kfold(conv_brm,dist_brm,lss_brm, K = 5)
# kfold_res <- brms::kfold(conv_brm, dist_brm, lss_brm)
# kfold_comparison <- kfold_res$diffs
save(kfold_res, file = paste0(sub_folder,"/kfold_results.rda"))


sink(paste0(sub_folder,"/fit_differences.txt"))
cat("\n")
cat(paste0("KFOLD Fit Differences\n"))
print(kfold_res)


cat("\n")
cat(paste0("KFOLD Fit Summary\n"))
summary(kfold_res)
cat("\n")
sink()


#' Validate with K-FOLD cross-validation:
#' Use "projects" or "locations" as part of the 
#' "groups argument".
#' http://paul-buerkner.github.io/brms/reference/kfold.html






