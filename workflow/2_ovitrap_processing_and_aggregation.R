# ---- Load required packages ----
library(forecast)
library(tidyverse)
library(terra)
library(geosphere)
library(lubridate)
library(htmltools)

#---- 4. Tranform the environmental variables from a table to a raster ----
head(geo_abio)

wide_temp <- geo_abio %>%
  select(lon, lat, temporalID, med_TAVG) %>%
  pivot_wider(names_from = temporalID, values_from = med_TAVG)

myT <- terra::rast(wide_temp, type="xyz")
plot(myT)
saveRDS(myT, paste0("myT", Sys.Date(), ".RDS"))

wide_prec <- geo_abio %>%
  select(lon, lat, temporalID, sum_PREC) %>%
  pivot_wider(names_from = temporalID, values_from = sum_PREC) 

myP <- terra::rast(wide_prec, type="xyz")
plot(myP)
saveRDS(myT, paste0("myP", Sys.Date(), ".RDS"))

#---- 5. Create a raster from er_geo ----
er_geojson <- st_read("erg5v2.geojson")

er_geo <- er_geojson %>% 
  select(lon, lat, Codice) %>% 
  st_drop_geometry(er_geojson)

myGeo <- terra::rast(er_geo, type="xyz")
myGeo
plot(myGeo)
summary(er_geo)

writeRaster(myGeo, filename = "myGeo.tif", overwrite = TRUE)

#---- 6. Ovitraps data ----
ovitraps <- readRDS("ovitraps_EmiliaRomagna.RDS")
names(ovitraps)

myCoord <- ovitraps %>% 
  dplyr::select(ID, long, lat, Country, Region) %>%
  drop_na() %>% 
  distinct()  

# Visualise your data points
leaflet(data = myCoord) %>%
  addTiles() %>%
  addMarkers(~long, ~lat, label = ~htmlEscape(ID)) 

# From table to sf
myCoord <- myCoord %>% 
  st_as_sf(coords=c("long", "lat"), crs=4326)

# ---- 7. Load raster for raster ID ----
tileID <- terra::extract(myGeo, myCoord, df=TRUE, xy=TRUE)
tileID$ID <- myCoord$ID # Assign an ovitrap ID to each ID pixel
head(tileID)
myCoord <- myCoord %>% 
  left_join(tileID, by="ID")

myCoord_tileID <- terra::as.data.frame(tileID, xy=TRUE)
myCoord_tileID <- myCoord_tileID[myCoord_tileID$ID %in% tileID$ID,]
names(myCoord_tileID) <- c("ID",  "Codice", "lon", "lat")

head(myCoord_tileID) 
saveRDS(myCoord_tileID, paste0("myCoord_tileID", Sys.Date(), ".RDS"))

OvitrapsCheat_tab <- cbind.data.frame(st_drop_geometry(myCoord),
                                      ovitrapLong =st_coordinates(myCoord)[,1],
                                      ovitrapLat = st_coordinates(myCoord)[,2]) %>%
  rename(tileLong=x, tileLat=y, tileID=Codice)
OvitrapsCheat_tab
saveRDS(OvitrapsCheat_tab, paste0("ovitrapsID_cheatsheet_", Sys.Date(), ".RDS"))

# Check for missing coordinates
mySub<-ovitraps[which(is.na(ovitraps$lat)),]
table(mySub$year)
table(mySub$ContactPerson)
unique(mySub$ID)
summary(mySub$value)

#---- 8. Aggregate observed eggs value by raster cell id ----
# Group by tile ID
ovitraps
OvitrapsCheat_tab

aggr.df <- ovitraps %>%
  dplyr::select(ID, temporalID, value,  Country,  Region) %>% 
  dplyr::left_join(tileID, by="ID") %>% 
  as_tibble() %>%  
  dplyr::group_by(ID, temporalID, Country,  Region) %>% 
  dplyr::summarise(value=median(value, na.rm=TRUE)#, 
                   # value.lci=quantile(value, probs=0.025, na.rm=TRUE),
                   # value.uci=quantile(value, probs=0.975, na.rm=TRUE)
  ) %>% 
  dplyr::mutate(join.id=paste0(ID, "_", temporalID)
  ) 

aggr.df
summary(aggr.df)
outname<-paste0(outdir, "aggrDF", Sys.Date(), ".RDS")
saveRDS(aggr.df, outname)

# add dates
full.df <- aggr.df %>%
  mutate(year=substr(temporalID, 1,4)) %>%
  dplyr::select(ID, temporalID, year,  value,  Country,  Region) %>%
  drop_na()

mySeq<-data.frame("mydate"=seq.Date(as.Date('2000-01-01'), as.Date('2022-12-31'), by="week"))
mySeq$temporalID<-paste0(year(mySeq$mydate), "_",    str_pad(week(mySeq$mydate), 2, pad="0"))
mySeq$year<-year(mySeq$mydate)
uID<-unique(full.df$ID)

myOut<-list()
for(i in 1:length(uID)){
  # i=1
  message(uID[i])
  full.cal <- subset(full.df, ID==uID[i])
  myYears <- unique(full.cal$year)
  full.cal<-merge(x = full.cal, y =  mySeq[which(mySeq$year %in%  myYears), ], by = "temporalID", all.y = TRUE)
  full.cal$ID<-unique(full.cal$ID)[!is.na(unique(full.cal$ID))]
  full.cal$Country<-unique(full.cal$Country)[!is.na(unique(full.cal$Country))]
  full.cal$Region<-unique(full.cal$Region)[!is.na(unique(full.cal$Region))]
  myOut[[i]]<-full.cal %>% 
    dplyr::select(ID, Country, Region,  temporalID, mydate, value ) %>% #value.lci,  value.uci  
    rename(date=mydate)
}
bio.matrix<-do.call(rbind.data.frame, myOut)
outname<-paste0(outdir, "aggrDF_full", Sys.Date(), ".RDS")
saveRDS(bio.matrix, paste0("biomatrix", Sys.Date(), ".RDS"))

bio.matrix
length(unique(bio.matrix$ID))
length(unique(tileID$ID))

# ---- 9. GAP filling of eggs observations ----
full.df <- aggr.df %>%
  mutate(year=substr(temporalID, 1,4)) %>% 
  dplyr::select(ID, temporalID, year,  value,  Country,  Region  ) %>%
  drop_na()
summary(full.df)
uID<-unique(full.df$ID)

mySeq <- data.frame("mydate"=seq.Date(as.Date('2000-01-01'), as.Date('2022-12-31'), by="week"))
mySeq$temporalID <- paste0(year(mySeq$mydate), "_",    str_pad(week(mySeq$mydate), 2, pad="0"))
mySeq$year <- year(mySeq$mydate)

myOut <- list()

for(i in 1:length(uID)){
  # i=1
  message(uID[i])
  full.cal <- subset(full.df, ID==uID[i])
  myYears <- unique(full.cal$year)
  full.cal<-merge(x = full.cal, y =  mySeq[which(mySeq$year %in%  myYears), ], by = "temporalID", all.y = TRUE)
  full.cal$ID<-unique(full.cal$ID)[!is.na(unique(full.cal$ID))]
  full.cal<-full.cal %>% 
    dplyr::select(ID, Country, Region, temporalID, mydate, value ) %>% #value.lci,  value.uci  
    rename(date=mydate)
  
  # quality check
  if (sum(!is.na(full.cal$value), na.rm = TRUE)<=2){
    full.cal$value <- NA
    full.cal$S1.52 <- NA    
    full.cal$C1.52 <- NA     
    full.cal$S2.52 <- NA    
    full.cal$C2.52 <- NA
  } else {
    ### interpolation of missing data
    full.cal$value <- as.numeric(forecast::na.interp(full.cal$value))
    yt.ts <- ts(full.cal$value, start = 1, frequency = 52) # for computing the sine/cosine waves is the same as zoo above - start = 1 is ok even if the first starting time/day is at 
    aaarrr <- data.frame(fourier(yt.ts, K=2)) # on average enough
    full.cal<-cbind.data.frame(full.cal, aaarrr)
    
  }
  myOut[[i]]<-full.cal
}

bio.matrix<-do.call(rbind.data.frame, myOut)
head(bio.matrix)
dim(bio.matrix)
summary(bio.matrix)
