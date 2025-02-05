library(zoo)
library(sf)
library(terra)
library(dplyr)
library(geosphere)
library(stringr)

# ---- 10. Process covariates ----
geo_abio <- readRDS("geo_abio.RDS")

## 10.0 Temperatures ----
temp.obs <- geo_abio %>%
  select(ID, lat, lon, year, week, temporalID, med_TAVG)

# Compute lagged temp

temp.obs <- temp.obs %>%
  group_by(ID) %>%
  mutate(med_TAVG=zoo::na.approx(med_TAVG),
         med_TAVG.lag2=zoo::rollmean(med_TAVG,2, align="right", fill=NA), 
         med_TAVG.lag3=zoo::rollmean(med_TAVG,3, align="right",  fill=NA),
         join.id=paste0(ID, "_", temporalID)
  )

temp.obs

# Convert temperature to threshold-like variable
myT_thresh <- 15

temp.obs$med_TAVG<-ifelse(temp.obs$med_TAVG<myT_thresh, 0, temp.obs$med_TAVG)
temp.obs$med_TAVG.lag2<-ifelse(temp.obs$med_TAVG.lag2<myT_thresh, 0, temp.obs$med_TAVG.lag2)
temp.obs$med_TAVG.lag3<-ifelse(temp.obs$med_TAVG.lag3<myT_thresh, 0, temp.obs$med_TAVG.lag3)

# Fill NAs
temp.obs$med_TAVG.lag2 <- zoo::na.locf(temp.obs$med_TAVG.lag2, na.rm = FALSE, fromLast = TRUE)
temp.obs$med_TAVG.lag3 <- zoo::na.locf(temp.obs$med_TAVG.lag3, na.rm = FALSE, fromLast = TRUE)

## ---- 10.1 Weekly photoperiod for each trap ----
ph <- geo_abio %>% 
  ungroup()%>%
  dplyr::select(ID, lat) %>% 
  st_drop_geometry() %>% 
  distinct()

# Dates dataframe for photoperiod calculation
myDates<-data.frame(date=seq.Date(as.Date("2024-01-01"), as.Date("2024-12-31"), by="day"))
myDates$week<-week(myDates$date)
myDates$year<-year(myDates$date)
myDates$cut_date<-floor_date(myDates$date, "weeks", week_start = 1)
myDates$temporalID<-paste0(myDates$year, "_", myDates$week)
dim(myDates)

phOut<-list()
for(i in 1:nrow(ph)){
  # i=1 # for testing
  message(i)
  tmpDat<-myDates
  phOut[[i]]<-tmpDat %>%
    mutate(photop=geosphere::daylength(lat=ph[[i, 2]], doy=as.Date(myDates$date)))  %>% 
    group_by(year, week) %>% 
    summarise(medianPhotoweek=median(as.numeric(photop), na.rm=TRUE))  %>% 
    mutate( medianPhotoweek.lag2=zoo::rollmean(medianPhotoweek,2, align="right", fill=NA), 
            medianPhotoweek.lag3=zoo::rollmean(medianPhotoweek,3, align="right",  fill=NA), 
            temporalID=paste0(year, "_", str_pad(week, 2, pad="0")), 
            ID=ph[[i, 1]], 
            join.id=paste0(ID, "_", temporalID)) %>%  
    dplyr::select(join.id, medianPhotoweek, medianPhotoweek.lag2, medianPhotoweek.lag3)
  
} 
phOut <- do.call(rbind.data.frame, phOut)
phOut

# Fill NAs with last object carried forward
phOut$medianPhotoweek.lag2 <- zoo::na.locf(phOut$medianPhotoweek.lag2, na.rm = FALSE, fromLast = TRUE)
phOut$medianPhotoweek.lag3 <- zoo::na.locf(phOut$medianPhotoweek.lag3, na.rm = FALSE, fromLast = TRUE)

which(is.na(phOut))

## 10.2 Precipitation ----

prec.obs <- geo_abio %>%
  select(ID, lat, lon, year, week, temporalID, sum_PREC)

# Compute lagged temp

prec.obs <- prec.obs %>%
  group_by(ID) %>%
  mutate(sum_PREC=zoo::na.approx(sum_PREC),
         sum_PREC.lag2=zoo::rollmean(sum_PREC,2, align="right", fill=NA), 
         sum_PREC.lag3=zoo::rollmean(sum_PREC,3, align="right",  fill=NA),
         join.id=paste0(ID, "_", temporalID))

# Fill NAs with last object carried forward

prec.obs$sum_PREC.lag2 <- zoo::na.locf(prec.obs$sum_PREC.lag2, na.rm = FALSE, fromLast = TRUE)
prec.obs$sum_PREC.lag3 <- zoo::na.locf(prec.obs$sum_PREC.lag3, na.rm = FALSE, fromLast = TRUE)

# ---- 11. Assemble covariate dataset ----

wda <- temp.obs %>%
  #slice(1:10) %>%  # slice for testing
  left_join(phOut %>% ungroup() %>% select(-year), by="join.id") %>%
  left_join(prec.obs %>% ungroup() %>% select(-year, -ID, -lat, -lon, -week, -temporalID), by="join.id") %>% 
  distinct(join.id, .keep_all= TRUE)

wda <-  wda %>% 
  dplyr::select(ID.x , join.id, lon.x, lat.x, year.x, week.x, temporalID.x,
                med_TAVG, med_TAVG.lag2, med_TAVG.lag3,
                sum_PREC, sum_PREC.lag2, sum_PREC.lag3,
                med_RHAVG, med_RHAVG.lag2, med_RHAVG.lag3,
                medianPhotoweek, medianPhotoweek.lag2, medianPhotoweek.lag3
                 
  )  %>% 
  rename(year = year.x, 
         ID = ID.x,
         lat = lat.x,
         lon = lon.x, 
         year = year.x, 
         week = week.x,
         temporalID = temporalID.x)  %>% 
  filter(year >= 2010) 

# Export as RDS
outname <- paste0("climatic_covariates_", Sys.Date(), ".RDS")
saveRDS(wda, outname)
