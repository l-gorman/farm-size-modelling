
library(gamlss)
library(optparse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)

base_path <- "./"
base_output_path <- paste0(base_path, "outputs/","gamlss-03-11-2022/")

final_df <- readr::read_csv(paste0(base_path,"prepared-data/final-modelling-dataset.csv"))


load(paste0(base_output_path, "flat_model.rda"))
load(paste0(base_output_path, "location_model.rda"))
load(paste0(base_output_path, "location_scale_model.rda"))
load(paste0(base_output_path, "location_scale_skew_model.rda"))
load(paste0(base_output_path, "location_scale_skew_kurtosis_model.rda"))
load(paste0(base_output_path, "location_scale_skew_kurtosis_model_selection.rda"))


anova_all <- as_tibble(location_scale_skew_kurtosis_model_selection$anovaAll)


names(location_scale_skew_kurtosis_model_selection$anovaAll)

tibble::as_tibble(location_scale_skew_kurtosis_model_selection$anova)

summary(location_scale_skew_kurtosis_model_selection)

summary(location_scale_skew_kurtosis_model_selection)


