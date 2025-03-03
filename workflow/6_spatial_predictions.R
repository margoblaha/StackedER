library(dplyr)

bio.matrix <- readRDS("C:/Dati/Output/biomatrix.RDS")
outdir <- ("C:/Dati/Output")

harm = T
Autoregr_model = 0
unique(subset(bio.matrix, year == 2022, select= "ID"))

if(Autoregr_model == 1) {
  stop("load NOT autoregressive model")
} else {
  spatial_cov <-readRDS("C:/Dati/Output/spatial_covariates_2024-03-18.RDS")
  spatial_cov <- spatial_cov %>% 
    filter(year == 2023) # filter for year 2023
  if(harm==T){

    # myL <- length(unique(bio.matrix$year))*length(unique(spatial_cov$ID))
    myL <- length(unique(2022))*length(unique(spatial_cov$ID))
    
    myHarm <- bio.matrix %>%
      filter(year == 2022, ID == 1136) %>% # I chose one of them for a given year because they are all identical
      select(year, week, S1.52, C1.52, S2.52,  C2.52)
    
    # myHarm = as.data.frame(sapply(myHarm[,4:7], rep.int, times=myL))
    myHarm = as.data.frame(sapply(myHarm[,4:7], rep_len, length.out=nrow(spatial_cov)))
    
    spatial_cov <- cbind(spatial_cov, myHarm)
    
    outname<-paste0(outdir, "spatialPredictors_2023_", Sys.Date(), ".RDS")
    saveRDS(spatial_cov, outname)
  }

  # Needs a check before predicting, to ensure the harmonics do not appear as "bands" on the map

  ggplot(spatial_cov, aes(x = lon, y = lat, fill = S2.52)) +
    geom_tile() + # or use geom_point() if you prefer points
    facet_wrap(~ week) +
    scale_fill_viridis_c(option = "magma") +
    theme_minimal() +
    theme(legend.position = "bottom")
  
  #spatial_cov<-na.omit(spatial_cov)
  test.sp.id<-spatial_cov$ID
  test.sp<-spatial_cov %>% 
    dplyr::select(-lon, -lat, -ID, -join.id, -year, -week)
  
  best.learner <- readRDS("C:/Dati/sensitivity_analysis_outputs/stacked_Model_train_2021_2022_Frac0.75_iter3_2024-03-11.RDS")
  
  ####
  spPred <- predict(best.learner,  newdata = test.sp, predict_type = "<Prediction>")
  # spPred
  my_spPred<-spatial_cov %>% 
    select(lon, lat, ID, temporalID,  join.id) %>% 
    bind_cols(spatial_pred = round(spPred$response), 
              spatial_pred_se = round(spPred$se))
  
  outname<-paste0("C:/Dati/Output/spatialPredictions_2023_21-22-model_075_", Sys.Date(), ".RDS")
  saveRDS(my_spPred, outname)
}

# ---- check ----
wide <- my_spPred %>%
  select(lon, lat, temporalID, spatial_pred) %>%
  pivot_wider(names_from = temporalID, values_from = spatial_pred) 

plot <- terra::rast(wide, type="xyz")

plot

plot(plot$"2023_43")
