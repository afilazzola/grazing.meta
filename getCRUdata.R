## necessary libraries
library(raster)
library(tidyverse)
library(ncdf4)

ms.data <- read.csv("data//synthesisdata//manuscript.revised.csv")
meta3 <- ms.data[!is.na(ms.data$StudyYear),]
meta3[,"StudyYear"] <- as.character(meta3[,"StudyYear"])

## split out years
metalong <- meta3 %>% mutate(tempYear = strsplit(StudyYear, ";")) %>% unnest(tempYear) ## unnest non-consecutive years
metalong$finalyear <-  lapply(as.list(metalong$tempYear), function(x) eval(parse(text=x))) ## sequence years 
metalong <- metalong %>% unnest(finalyear) %>%  data.frame(.) ## unnest sequence years

meta3 <- metalong

## Convert lat lon to spatial points
meta3 <- meta3 %>% filter(lat != "") ## remove blanks for lat and lon
meta3 <- meta3[!grepl(";", meta3$lat),] ## remove multiple entries
meta3$lat <- as.numeric(levels(meta3$lat))[meta3$lat] ## convert from factor to number
meta3$lon <- as.numeric(levels(meta3$lon))[meta3$lon] ## convert from factor to number

## generate dataframe
gps <- data.frame(uniqueID = meta3$uniqueID, lon=meta3$lon, lat=meta3$lat, Year=meta3$finalyear)
#gps <- gps[!duplicated(gps$uniqueID),]


## assign spatial points
crs.world <-CRS("+proj=longlat +datum=WGS84")
coordinates(gps) <- ~lon+lat
proj4string(gps) <- crs.world


## list variables
occurdat <- list.files("E:\\CRUdata\\", pattern="*.nc", recursive = TRUE,  full.names = TRUE)


## run loop to download selected year
crudat <- data.frame()

for(i in 1:63){
  ## Select year for iteration
  yearTarget <- ((1955+i)-1901)*12 ## determine surveyed year from multiples of 12
  yearRange <- seq(yearTarget-11,yearTarget+12, 1) ## generate range of years in multiples of 12 for dataset
  
  tryCatch({ ## put error catch for years without lakes surveyed
    ## subset GPS points by that year
    gpsYear <- subset(gps, Year==(1955+i))
    if (nrow(gpsYear)==0) stop("No lakes surveyed that year")
    
    yearData <- data.frame() ## create empty dataframe
    for(j in yearRange){
      temp <- raster(occurdat[4], band=j) ## select band iteration
      values <- raster::extract(temp, gpsYear) ## extract from raster based on coordinates
      time.frame <- temp@z[[1]] ## extract timeframe
      temporary <- data.frame(gpsYear$uniqueID, variable=values, date=time.frame,  ## assign to temporary dataframe 
                              surveyYear=ifelse(j < max(yearRange)-11, "lag","survey"))  ## create lag year column based on lower 12 months of yearly range
      yearData <- rbind(yearData, temporary) ## join temporary dataframe to master dataframe
    }
    crudat <- rbind(crudat, yearData)
  }, error=function(e){cat("ERROR :",conditionMessage(e), "\n")})
  print(i)
}

## move lag year data into a separate column
crudat[crudat$surveyYear=="survey","variable.lagged"] <- crudat[crudat$surveyYear=="lag", "variable"] ## move lag data into its own column
crudat <- crudat[crudat$surveyYear=="survey",] ## drop lag year data

## assign numeric date columns
crudat["Year"] <- as.numeric(format(as.Date(crudat$date, format="%Y-%m-%d"),"%Y"))
crudat["Month"] <- as.numeric(format(as.Date(crudat$date, format="%Y-%m-%d"),"%m"))


## Repeat for other variables and assign to datframe

## rename variable
names(crudat)[c(2,5)] <- c("tmax","tmax.lag")
tmax <- crudat

## drop repeated columns
tmax <- tmax[,-c(3:5)]
tmin <- tmin[,-c(3:5)]
pre <- pre[,-c(3:5)]
pet <- pet[,-c(3:5)]

## rename unique ID column
names(tmax)[1] <- "uniqueID"
names(tmin)[1] <- "uniqueID"
names(pre)[1] <- "uniqueID"
names(pet)[1] <- "uniqueID"

fullCru <- merge(tmax, tmin, by=c("uniqueID","Year","Month")) ## join tmax and tmin 
fullCru <- merge(fullCru, pre, by=c("uniqueID","Year","Month")) ## join precipitation
fullCru <- merge(fullCru, pet, by=c("uniqueID","Year","Month")) ## join potential evapotranspiration


## get average for study
avgCru <- fullCru %>% group_by(uniqueID) %>% summarize(temp = mean((tmax+tmin)/2), precip = mean(pre), PET = mean (pet), aridity = mean(pre/pet))


write.csv(avgCru, "data//climateCRU.csv", row.names=FALSE)