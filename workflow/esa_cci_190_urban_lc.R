library(tidyverse)
library(sf)
library(terra)
library(dplyr)

setwd("~/Dati/esaCCI_urban_2010_2020")

file_list <- list.files(pattern = "190_flt_avr_1k")

raster_list <- list()

# Read each raster file and store it in the list
for (i in seq_along(file_list)) {
  var_name <- paste0("lc", 2010 + i - 1)  # adjust the base year as/if needed
  assign(var_name, terra::rast(file_list[[i]]))
  raster_list[[i]] <- rast(file_list[[i]])
}

# Stack all the raster files
stacked_rasters <- do.call(c, raster_list)

tileID <- readRDS("~/Dati/tileID.RDS") # raster with the grid of your data
plot(tileID)

tileID_polygons <- terra::as.polygons(tileID, dissolve = TRUE)

aggregated_results <- list()

for (i in 1:length(raster_list)) {
  # Load the raster
  current_raster <- terra::rast(raster_list[[i]])
  
  #current_raster <- project(current_raster, tileID)
  current_raster <- resample(current_raster, tileID)
  
  # Crop the raster using the extent of the tileID raster
  cropped_raster <- crop(current_raster, ext(tileID))
  
  # Aggregate the cropped raster using tileID as the zones
  # Calculate the mean value for each tileID
  result <- terra::zonal(cropped_raster, tileID, fun = "mean")
  
  # Store the result in the list
  aggregated_results[[i]] <- result
}

combined_results <- bind_rows(aggregated_results)

long_table <- combined_results %>% 
  pivot_longer(cols = starts_with("LC_"), 
             names_to = "Year", 
             values_to = "LC_Value") %>%
  mutate(Year = as.integer(str_extract(Year, "(?<=LC_)\\d{4}")))