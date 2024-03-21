###Code that could be useful but didn't work or wasn't useful at the time



##Creating home ranges using AMT package
#convert simple feature back into a normal dataset
oyc_22_df <- sf_to_df(oyc_22_sf2, fill = TRUE)
#renaming columnns
oyc_22_df <- oyc_22_df %>% rename(Longitude = x, Latitude = y) %>% dplyr::select(Longitude, Latitude, UTC_datetime, device_id)
# Convert UTC_datetime to POSIXct. Make sure that this matches the format in the columns otherwise will return NAs
oyc_22_df$UTC_datetime <- as.POSIXct(oyc_22_df$UTC_datetime, format = "%Y-%m-%d %H:%M:%S") 
#make tracks for home range 
oyc_22_df2 <- oyc_22_df %>% filter(!is.na(UTC_datetime))
oyc_22_trk <- oyc_22_df2 %>% make_track(Longitude, Latitude, UTC_datetime, id = device_id,
                                        crs = 4326) # 4326 is the EPSG code for WGS84 coordinate reference system, which most GPS data will be in #
#filter for one individual
oyc_44328 <- oyc_22_trk %>% filter(id == "44328")
#akde home range
akde <- hr_akde(oyc_44328, fit_ctmm(oyc_44328, "auto"), levels = c(0.5, 0.95))
#plot akde home range
plot(akde, add.relocations = FALSE, add = TRUE, lty = 3)





#### Bearhop_CakeandCode: Session 1: Subsampling GPS data ####

# Created: 24/10/2023 Aimée McIntosh

### 1. Session Summary ----
# This code provides examples of how to subsample gps tracking data and includes code originally provided by Liam Langley

# 2. Load packages
# 3. Load data
# 4. Assessing sampling rate 
# 5. Function to sub-sample GPS data


### 2. Load packages ----
# pacman useful way of loading multiple packages quickly (don't need to write "library" every time)
pacman::p_load(tidyr,plyr,dplyr,zoo,data.table,amt,move,purrr,readr,
               lubridate,tidyverse,readr)



### 3. Load data ----
# Sample data of wintering Greenland barnacle geese (Branta leucopsis) on Islay
# .csv file can be found in the Sharepoint folder "Bearhop Research Group/Coding Club/Workshops/Example Data"
setwd("C:/Users/bgroo/Desktop/Dissertation/Data/All")
getwd()

all_22 <- read.csv("all22.csv")
all_23 <- read.csv("all23.csv")

all_22 <- all_22 %>% filter(UTC_datetime == as.Date('01/01/2022'))

str(all_22) # check format of UTC_datetime

# Convert UTC_datetime to POSIXct
all_22$UTC_datetime <- as.POSIXct(all_22$UTC_datetime, format = "%Y/%m/%d %H:%M:%S") # Make sure that this matches the format in the columns otherwise with return NAs
str(all_22) # check UTC_datetime has been converted properly
all_22$device_id <- as.factor(all_22$device_id)
all_22$species <- as.factor(all_22$species)
all_23$species <- as.factor(all_23$species)

### 4. Assessing sample rate ----
#removing rows with NAs
all_22 <- all_22[!is.na(all_22$UTC_datetime),]
# Create a track object using amt of all gps data
all_22_trk <- all_22 %>%
  make_track(Longitude, Latitude, UTC_datetime, id = device_id, HDOP = hdop,
             crs = 4326)
all_22_trk # This creates a tibble of all gps points

info_trk <- all_22_trk %>% summarize_sampling_rate_many(c("id"),time_unit = "hour") 
info_trk
# You can also specify if you want to view this to the nearest unit 
# e.g. hours/mins code is (summarize_sampling_rate_many(c("id")time_unit="hour"))


### 5. Function to sub-sample GPS data ----
# Sub-sample for consistent sampling rate to calculate daily distance traveled without being influenced by variable sampling rate

# 5.2. Example 2: Creating a function to subsample tracking data ----
# This uses code originally provided by Liam Langley
# Advantage of this code is that it does not need you to create a track object in amt and can be used on the gps data frame imported

trackSubSamp <- function(TD, dt=1, unit='hours'){
  
  ## order the UTC_datetimes
  TD <- TD[order(TD$UTC_datetime),] 
  
  # breakdown to datasets per bird
  unid = unique(TD$device_id) # the unique tag IDs in your data set
  nrid = length(unid)
  TDall = list(nrid)  
  TDred = list(nrid)
  timestep = paste(dt,unit)
  
  # create time sequence from min to max time with step sizes that are defined at the start of the function
  dati_start = min(TD$UTC_datetime,na.rm=T)
  dati_end = max(TD$UTC_datetime,na.rm=T)
  datiseq = seq(from=dati_start, to=dati_end, by=timestep)
  
  for (i in 1:nrid)
  {
    Dtemp = TD[TD$device_id == unid[i],]
    idx = sapply(datiseq, function( x) which.min( abs( difftime( Dtemp$UTC_datetime, x, units='mins') ) ) ) # finds closest time in data to your created time series
    TDall[[i]] = Dtemp
    TDred[[i]] = unique( Dtemp[idx,] ) # the function unique makes sure that the rows in Dtemp[idx,] are unique - so no duplicate points
  }
  
  TDred # return this list
}

# Run the function on the original data frame
length(all_22$Longitude) # Example to check number of fixes pre-subsample
list_all_22 <- trackSubSamp(all_22)

# Convert list to data frame
df_all_22_ss <- do.call("rbind", list_all_22)
length(df_all_22_ss$Longitude) # Smaller number of fixes after subsampling


#### END ####


library(amt)
library(dplyr)
library(readr)
library(purrr)
library(tidyr)
library(ggplot2)
library(units)


#### read in data and get formats sorted ####



all_22 <- all_22 %>%
  dplyr::select(device_id, species, deploy_site, UTC_datetime, UTC_date, UTC_time, 
                Latitude, Longitude, hdop)
library(lubridate)
all_22$Latitude <- as.numeric(all_22$Latitude)
all_22$Longitude <- as.numeric(all_22$Longitude)

str(all_22)
na_cols <- c("Longitude", "Latitude", "UTC_datetime")
all_22 <- all_22 %>%
  drop_na(all_of(na_cols))
rm(na_cols)

## remove duplicated datetimes
check2 <- subset(all_22, select = c("UTC_datetime")) %>% duplicated
sum(check2) 
## now remove these duplicate rows
all_22 <- all_22 %>% filter(!check2)
rm(check2)


## turn it into a track object ##



dat <- all_22 %>%
  make_track(Longitude, Latitude, UTC_datetime, id = device_id, HDOP = hdop,
             crs = 4326) # 4326 is the EPSG code for WGS84 coordinate reference system, which most GPS data will be in #
str(dat)


## home range for one bird ##
all_22_44328 <- dat %>% filter(id == "44328") # filtering for one tag id #
mcp1 <- hr_mcp(all_22_44328, levels = c(0.5, 0.95)) # minimum convex polygon, specifying we want the 50% and the 95% polygons #
kde1 <- hr_kde(all_22_44328, levels = c(0.5, 0.95)) # kernel density, again specifying 95% and 50% #
akde <- hr_akde(all_22_44328, fit_ctmm(all_22_44328, "auto"), levels = c(0.5, 0.95)) # autocorrelated kernel density #
akde1 <- hr_akde(all_22_44328,  fit_ctmm(all_22_44328, "auto", uere = 6.71), levels = c(0.5, 0.95)) # autocorrelated kernel density, using uere (location error metric) calculated from calibration data #

## plot the different home ranges out (add = TRUE puts them in the same plot) ##
plot(kde1)
plot(mcp1, add.relocations = FALSE, add = TRUE, lty = 2)
plot(akde1, add.relocations = FALSE, add = TRUE, lty = 3)

## look at the area for each home range (50% and 95%, in metres squared) ##
hr_area(mcp1)
hr_area(kde1)
hr_area(akde1)

## hr_isopleths converts the home ranges into sf (simple feature) objects that can be exported to QGIS ##
iso <- hr_isopleths(akde1)

## convert to shapefile - saves it to your working directory ##
st_write(iso, "all_22_44328akde.shp")



##EKS Package for one individual
kde_216281 <- oyc_22_sf2 %>%
  filter(device_id == "216281") %>%
  eks::st_kde() %>%
  eks::st_get_contour(., cont=c(95))
ggplot()+
  geom_sf(data = kde_216281)

##EKS filtered for weekday
kde_216281_workday <- oyc_22_sf2 %>%
  filter(device_id == "216281") %>%
  filter(weekday %in% workday) %>%
  eks::st_kde() %>%
  eks::st_get_contour(., cont=c(95))
ggplot()+
  geom_sf(data = kde_216281_workday)

##EKS filtered for weekend
kde_216281_weekend <- oyc_22_sf2 %>%
  filter(device_id == "216281") %>%
  filter(weekday %in% weekend) %>%
  eks::st_kde() %>%
  eks::st_get_contour(., cont=c(95))
ggplot()+
  geom_sf(data = kde_216281_weekend)


#-----------------------Home Ranges for weekend and weekday---------------------
###Home Range during week
#filter for only workdays
rk_trk_work <- rk_trk %>% 
  filter(day %in% workday)
#create akde home range
rk_akde_work <- hr_akde(rk_trk_work, 
                        model = fit_ctmm(rk_trk_work, "auto"), 
                        levels = c(0.5, 0.95),
                        trast = trast)
#calculate area of home range
rk_area_work <- hr_area(rk_akde_work)
#plot home range
plot(rk_akde_work, add.relocations = TRUE)

###Home Range during weekend
#filter for only weekend
rk_trk_weekend <- rk_trk %>% 
  filter(day %in% weekend)
#create akde of home range
rk_akde_weekend <- hr_akde(rk_trk_weekend, 
                           model = fit_ctmm(rk_trk_weekend, "auto"), 
                           levels = c(0.5, 0.95),
                           trast = trast)
#calculate area of home range
rk_area_weekend <- hr_area(rk_akde_weekend)
#plot home range
plot(rk_akde_weekend, add.relocations = TRUE)

###calculating difference between home ranges
rk_area_work[[2,3]]-rk_area_weekend[[2,3]]

###calculating overlap of two home ranges
hr_overlap(rk_akde_work, rk_akde_weekend, type = "hr") 
hr_overlap(rk_akde_weekend, rk_akde_work, type = "hr") 

