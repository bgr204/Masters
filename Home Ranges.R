#-----------------------Setup---------------------------------------------------


urlfile1 <- "https://raw.githubusercontent.com/bgr204/Masters/master/all22.txt" 
urlfile2 <- "https://raw.githubusercontent.com/bgr204/Masters/master/all23.txt"
urlfile3 <- "https://raw.githubusercontent.com/bgr204/Masters/master/tag%20metadata%2021-22.txt"
urlfile4 <- "https://raw.githubusercontent.com/bgr204/Masters/master/tag%20metadata%2022-23.txt"
all_22 <- read.csv(url(urlfile1), sep = "\t")
all_23 <- read.csv(url(urlfile2), sep = "\t")
tag_22 <- read.csv(url(urlfile3), sep = "\t")
tag_23 <- read.csv(url(urlfile4), sep = "\t")
all <- rbind(all_22, all_23)
tag <- rbind(tag_22, tag_23)

###library
library(sf)
library(sfheaders)
library(amt)
library(dplyr)
library(ggplot2)
library(tidygraph)
library(ggraph)
library(magrittr)
library(eks)

###changing format of coordinates
all$Longitude <- as.numeric(all$Longitude)
all$Latitude <- as.numeric(all$Latitude)

###remove NAs
all <- all %>% 
  filter(!is.na(UTC_datetime)) %>%
  filter(!is.na(Latitude))

###Convert UTC_datetime to POSIXct. Make sure that this matches the format in 
#the columns otherwise will return NAs
all$ts <- as.POSIXct(all$UTC_datetime, 
                        format = "%Y-%m-%d %H:%M:%S") 

###create column with day
#format UTC-date as a date
all$UTC_date <- as.Date(all$UTC_date)
#create a column for days
all$day <- weekdays(all$UTC_date)

###select columns that are needed
all <- all %>% 
  dplyr::select(Longitude, Latitude, ts, day, UTC_date, device_id, species)

###filtering to remove migration
#making a simple feature
all_sf <- st_as_sf(all, coords=c("Longitude","Latitude"), crs = 4326)
#setting coordinates for dublin polygon
y_coord <- c(53.55, 53.55, 53.29, 53.29, 53.55)
x_coord <- c(-6.30, -6.00, -6.00, -6.30, -6.30)
#bind coordinates
xy <- cbind(x_coord, y_coord)
#create polygon
dubpol <- st_polygon(x = list(xy))
#convert to simple feature
dubpoly <- st_sfc(dubpol, crs = 4326)
#subset data using the dublin polygon
all_sf <- all_sf[dubpoly,]
#convert simple feature back into a normal dataset
all_df <- sf_to_df(all_sf, fill = TRUE)

###remove NAs
all_df <- all_df %>% 
  filter(!is.na(ts))

###check if all observations are complete
all(complete.cases(all_df))

###removing duplicates
#check for duplicated time stamps
any(duplicated(all_df$ts))
#remove duplicates
all_df <- all_df[!duplicated(all_df$ts), ]

###defining weekend and day
workday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
weekend <- c("Saturday","Sunday")

####make track
all_trk <- make_track(all_df, x, y, ts, day = day, date = UTC_date, 
                      id = device_id, species = species,
                      crs = 4326)
#filter for redshank
rk_trk <- all_trk %>% 
  filter(species == "RK")

###dates
date <- c(unique(all_trk$date))

###ids
id <- c(unique(all_trk$id))

###species
species <- c(unique(all_trk$species))

#-----------------------Home Ranges (rk = 44204 as example) using EKS-----------

kde_rk <- all_sf %>%
  filter(species == "RK")

id <- c(unique(kde_rk$device_id))

#empty dataframe
hr_results <- data.frame(day = character(), area = numeric())
#loop repeated for each date
for (j in id) {
  #filter track by id
  hr1 <- kde_rk %>%
    filter(device_id == j)
  date1 <- c(unique(hr1$UTC_date))
for (i in date1) {
  #filter track by date
  hr2 <- hr1 %>%
    filter(UTC_date == i)
  #calculate home range area
  hr3 <- hr2 %>%
    eks::st_kde() %>%
    eks::st_get_contour(., cont=c(95)) %>%
    sf::st_area()
  #extract value from results
  value_hr <- as.numeric(hr3)
  #create dataframe with a day and area column 
  hr4 <- data.frame(date = i, area = value_hr)
  #bind to empty dataframe
  hr_results <- rbind(hr_results, hr4)
}}

#change format of date
hr_results$date <- as.Date(hr_results$date)
#add column with weekday
hr_results$day <- weekdays(hr_results$date)
#Print the resulting dataframe
print(hr_results)





#-----------------------Step Lengths (rk = 44204 as example)--------------------


###timer
start.time <- Sys.time()

###loop to create step lengths for each date
#empty dataframe
sl_results <- data.frame(day = character(), distance = numeric())
#loop repeated for each date
for (k in species) {
  step1 <- all_trk %>%
    filter(species == k)
for (j in id) {
  #filter track by id
  step2 <- all_trk %>%
    filter(id == j)
for (i in date) {
  #filter track by date
  step3 <- all_trk %>%
    filter(date == i)
  #calculate step lengths
  step4 <- step_lengths(step3) %>%
    #define as a data frame
    as.data.frame() %>%
    #change column name
    setNames(c("distance")) %>% 
    #remove NAs
    filter(!is.na(distance))
  #sum up the step lengths
  value_sl <- sum(step4$distance)
  #create dataframe with a day and distance column 
  step5 <- data.frame(date = i, id = j, species = k, distance = value_sl)
  #bind to empty dataframe
  sl_results <- rbind(sl_results, step5)
}}}

#change format of date
sl_results$date <- as.Date(sl_results$date)
#add column with weekday
sl_results$day <- weekdays(sl_results$date)
#creating a column for weekend or not
sl_results$is_weekend <- sl_results$day %in% c("Saturday", "Sunday")
#Print the resulting dataframe
print(sl_results)


###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))

