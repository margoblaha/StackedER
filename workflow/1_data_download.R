# ---- Load the packages ----
library(tidyverse)
library(sf) # for reading the geojson file
library(terra)
library(leaflet)
library(htmltools)
library(dplyr)
library(rvest)

#---- 1. Download abiotic variables ----
url <- "https://dati-simc.arpae.it/opendata/erg5v2/timeseries/"
webpage <- read_html(url)

# Extract the links (href) from the webpage
links <- html_attr(html_nodes(webpage, "a"), "href")

# Extract only the directory links
directories <- grep("/$", links, value = TRUE)

# Remove the trailing slash from directories
directories <- sub("/$", "", directories) %>% as.character()

# Display the list of directories
head(directories)
tail(directories)
directories <- directories[-1] #remove first directory

# Loop through the directories to download the abiotic variables
for(n in 1:length(directories)){
  # n=1
  for (year in 2010:2023) { # 2024 is now available too
    # year = 2020
    message("Processing cell ID ", directories[n], " for the year ", year)
    url_erg5 <- paste0("https://dati-simc.arpae.it/opendata/erg5v2/timeseries/",directories[n], "/", directories[n], "_", year, ".zip" )
    file_name <- paste0("er_abio_", directories[n], "_", year, ".zip") # dynamic name
    download.file(url = url_erg5, destfile = file_name)
  }
}

#---- 2. Download grid geojson ----
url_json <- "https://dati-simc.arpae.it/opendata/erg5v2/timeseries/erg5v2.geojson"
download.file(url = url_json, destfile = "erg5v2.geojson")
er_geojson <- st_read("erg5v2.geojson")

er_geo <- er_geojson %>% 
  select(lon, lat, Codice) %>% 
  st_drop_geometry(er_geojson)

view(er_geo)

#---- 3. Aggregate variables ----
myFiles <- list.files("C:/Dati/", full.names=TRUE, pattern=".zip")
myOut <-list()
for(i in 1:length(myFiles)){
  # i=50
  # myFiles[i]
  message("Processing ", i, " out of ", length(myFiles))
  # Specify the path to the ZIP file
  zip_path <- myFiles[i]
  
  # Specify the name of the CSV file inside the ZIP archive
  csv_file <- paste0(substr(basename(myFiles[i]), 9, 18), "_d.csv")
  
  # Open a connection to the ZIP file
  zip_connection <- unz(zip_path, csv_file)
  test <- read.csv(zip_connection, header = TRUE)
  
  # view(test)
  myOut[[i]] <- test %>%
    mutate(ID = as.numeric(substr(csv_file, 1,5)),
           date = as.Date(PragaDate),
           week = lubridate::week(date), 
           year = lubridate::year(date)) %>% 
    group_by(ID, year, week) %>%
    summarise(
      # med_TMIN = median(DAILY_TMIN),
      # med_TMAX = median(DAILY_TMAX),
      med_TAVG = median(DAILY_TAVG, na.rm=TRUE),
      sum_PREC = sum(DAILY_PREC, na.rm=TRUE),
    )
  
}

class(myOut)
length(myOut)
weekly.abio <- do.call(rbind, myOut)
which(is.na(weekly.abio$med_TAVG))

# Join the table to the ID of the ER grid to obtain the coordinates

er_geo$Codice # common column -> need to rename to ID
weekly.abio$ID
geo_abio <- left_join(weekly.abio, er_geo %>% rename(ID = Codice), by = "ID")

geo_abio$temporalID <- paste0(geo_abio$year, "_", str_pad(geo_abio$week, 2, pad="0"))

# Save the environmental variables file as an .RDS
saveRDS(geo_abio, paste0("geo_abio_", Sys.Date(), ".RDS")) 
