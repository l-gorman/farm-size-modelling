
library(gamlss)
library(optparse)
library(readr)
library(tidyr)
library(dplyr)
library(ggplot2)
library(tibble)
base_path <- "./"
base_output_path <- paste0(base_path, "outputs/","gamlss_test/")

base_output_path <- paste0(base_path, "outputs/","gamlss_test/")


final_df <- readr::read_csv(paste0(base_path,"prepared-data/final-modelling-dataset.csv"))


load(paste0(base_output_path, "flat_model.rda"))
load(paste0(base_output_path, "location_model.rda"))
load(paste0(base_output_path, "location_scale_model.rda"))
load(paste0(base_output_path, "location_scale_skew_model.rda"))
load(paste0(base_output_path, "location_scale_skew_kurtosis_model.rda"))
load(paste0(base_output_path, "location_scale_skew_kurtosis_model_selection.rda"))

summary(location_scale_skew_kurtosis_model_selection)


 set.seed(404)
# row <- sample(1:nrow(final_df),1)

area_counts <- table(final_df$geo_id) %>% as.data.frame() 
area_counts <- area_counts[area_counts$Freq>200,]
areasample <- sample(area_counts$Var1, 1)
row <- which(final_df$geo_id==areasample)[1]
# area_counts <- area_counts[order(area_counts$Freq,decreasing = T),]


admin_info <- final_df[row, c("ADM0_NAME", "ADM1_NAME","ADM2_NAME")]

dataset <- final_df
location_information <- admin_info
farm_size_column="farm_size_ha"

x <- c("healthcare_traveltime",
    "nightlights",
    "population_density",
    
    
    "elevation",
    "ndvi",
    "topographic_diversity",
    "length_growing_season"
)



area_model_comparison <- function(dataset, 
                                  location_information, 
                                  flat_model,
                                  location_model,
                                  location_scale_model,
                                  location_scale_skew_model,
                                  location_scale_skew_kurtosis_model_selection,
                                  
                                  farm_size_column="farm_size_ha",
                                  x=c("healthcare_traveltime",
                                         "nightlights",
                                         "population_density",
                                         
                                         
                                         "elevation",
                                         "ndvi",
                                         "topographic_diversity",
                                         "length_growing_season"
                                  )
                                  
){
  
  location_data <- dataset[dataset$ADM0_NAME==location_information$ADM0_NAME&
                        dataset$ADM1_NAME==location_information$ADM1_NAME  &
                        dataset$ADM2_NAME== location_information$ADM2_NAME,] 
  
  
  
  
  
  farm_size_distribution <- ggplot(location_data, aes_string(x=farm_size_column))+
    geom_histogram()
  
  prediction_flat <- predictAll(flat_model,newdata = location_data[x])
  prediction_flat <- tibble::as_tibble(list(
    prediction_type="flat",
    mu=unique(prediction_flat$mu),
    sigma=unique(prediction_flat$sigma),
    nu=unique(prediction_flat$nu),
    tau=unique(prediction_flat$tau)
  ))
  if (nrow(prediction_flat)){
    stop("More than one prediction for the area")
  }
  
  
  
  prediction_location <- predictAll(location_model,newdata = location_data[x])
  
  flat_prediction <- 
                
  
  
}

anova_all <- as_tibble(location_scale_skew_kurtosis_model_selection$anovaAll)




names(location_scale_skew_kurtosis_model_selection$anovaAll)

tibble::as_tibble(location_scale_skew_kurtosis_model_selection$anova)

summary(location_scale_skew_kurtosis_model_selection)

summary(location_scale_skew_kurtosis_model_selection)


