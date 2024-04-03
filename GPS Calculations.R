#-----------------------Setup---------------------------------------------------

###timer
start.time <- Sys.time()

###uploading files
#reading in urls from github
url1 <- "https://raw.githubusercontent.com/bgr204/Masters/master/all22.txt" 
url2 <- "https://raw.githubusercontent.com/bgr204/Masters/master/all23.txt"
url3 <- "https://raw.githubusercontent.com/bgr204/Masters/master/tag%20metadata%2021-22.txt"
url4 <- "https://raw.githubusercontent.com/bgr204/Masters/master/tag%20metadata%2022-23.txt"
#read url into csv
all_22 <- read.csv(url(url1), sep = "\t")
all_23 <- read.csv(url(url2), sep = "\t")
tag_22 <- read.csv(url(url3), sep = "\t")
tag_23 <- read.csv(url(url4), sep = "\t")
#bind years
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

###Convert UTC_datetime to POSIXct
all$UTC_datetime <- as.POSIXct(all$UTC_datetime, 
                        format = "%Y-%m-%d %H:%M:%S") 

###remove NAs - why are they introduced by the previous code?
all <- all %>% 
  filter(!is.na(UTC_datetime))

###Sub-sample for consistent sampling rate
#premade fucntion from research group
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
#use function
list_all <- trackSubSamp(all)
all_ss <- do.call("rbind", list_all)

###adding catch date to the dataframe
all_ss$catch_date <- with(tag, deploy_date[match(all_ss$device_id, ID)])
#formatting date
all_ss$catch_date <- as.Date(all_ss$catch_date, format = "%d/%m/%Y")
#creating column for day after
all_ss$day_after <- all_ss$catch_date+1
#removing data from catch dates
all_ss <- subset(all_ss, UTC_date != catch_date)
all_ss <- subset(all_ss, UTC_date != day_after)

###create column with day
#format UTC-date as a date
all_ss$UTC_date <- as.Date(all_ss$UTC_date)
#create a column for days
all_ss$day <- weekdays(all_ss$UTC_date)

###select columns that are needed
all_ss <- all_ss %>% 
  dplyr::select(Longitude, Latitude, UTC_datetime, day, UTC_date, device_id, species)

###filtering to remove migration
#making a simple feature
all_sf <- st_as_sf(all_ss, coords=c("Longitude","Latitude"), crs = 4326)
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

###creating objects
workday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
weekend <- c("Saturday","Sunday")
species <- c(unique(all$species))

####make track
all_trk <- make_track(all_df, x, y, UTC_datetime, day = day, date = UTC_date, 
                      id = device_id, species = species,
                      crs = 4326)

###removing days with less than 23 fixes
#all_trk
all_trk <- all_trk %>%
  group_by(id, date) %>%
  mutate(sampling_rate = n()) %>%
  ungroup() %>%
  filter(sampling_rate > 22)
#all_sf
all_sf <- all_sf %>%
  group_by(device_id, UTC_date) %>%
  mutate(sampling_rate = n()) %>%
  ungroup() %>%
  filter(sampling_rate > 22)





#-----------------------Home Ranges (rk = 44204 as example) using EKS-----------


#empty dataframe
hr_results <- data.frame(day = character(), area = numeric())
#loop repeated for each date
for (i in species) {
  hr1 <- all_sf %>%
    filter(species == i)
  id_hr <- c(unique(hr1$device_id))
for (j in id_hr) {
  #filter track by id
  hr2 <- hr1 %>%
    filter(device_id == j)
  date_hr <- c(unique(hr2$UTC_date))
for (k in date_hr) {
  #filter track by date
  hr3 <- hr2 %>%
    filter(UTC_date == k)
  # Skip calculation if number of rows in hr3 is less than 6
  if (nrow(hr3) < 6) {
    next  # Skip to the next iteration
  }
  #calculate home range area
  hr4 <- hr3 %>%
    eks::st_kde() %>%
    eks::st_get_contour(., cont=c(95)) %>%
    sf::st_area()
  #extract value from results
  value_hr <- as.numeric(hr4)
  #create dataframe with a day and area column 
  hr5 <- data.frame(date = k, area = value_hr, id = j, species = i)
  #bind to empty dataframe
  hr_results <- rbind(hr_results, hr5)
}}}

#change format of date
hr_results$date <- as.Date(hr_results$date)
#add column with weekday
hr_results$day <- weekdays(hr_results$date)
#creating a column for weekend or not
hr_results$is_weekend <- hr_results$day %in% c("Saturday", "Sunday")
#Print the resulting dataframe
print(hr_results)
#export to txt file
write.table(hr_results, "C:\\Users\\bgroo\\Desktop\\Masters\\Home_Range.txt", 
            row.names=FALSE, sep = "\t", quote=FALSE)





#-----------------------Step Lengths (rk = 44204 as example)--------------------


###loop to create step lengths for each date
#empty dataframe
sl_results <- data.frame(day = character(), distance = numeric())
#loop repeated for each date
for (i in species) {
  sl1 <- all_trk %>%
    filter(species == i)
  id_sl <- c(unique(sl1$id))
for (j in id_sl) {
  #filter track by id
  sl2 <- all_trk %>%
    filter(id == j)
  date_sl <- c(unique(sl2$date))
for (k in date_sl) {
  #filter track by date
  sl3 <- all_trk %>%
    filter(date == k)
  #calculate step lengths
  sl4 <- step_lengths(sl3) %>%
    #define as a data frame
    as.data.frame() %>%
    #change column name
    setNames(c("distance")) %>% 
    #remove NAs
    filter(!is.na(distance))
  #sum up the step lengths
  value_sl <- sum(sl4$distance)
  #create dataframe with a day and distance column 
  sl5 <- data.frame(date = k, distance = value_sl, id = j, species = i)
  #bind to empty dataframe
  sl_results <- rbind(sl_results, sl5)
}}}

#change format of date
sl_results$date <- as.Date(sl_results$date)
#add column with weekday
sl_results$day <- weekdays(sl_results$date)
#creating a column for weekend or not
sl_results$is_weekend <- sl_results$day %in% c("Saturday", "Sunday")
#Print the resulting dataframe
print(sl_results)

#export to txt file
write.table(sl_results, "C:\\Users\\bgroo\\Desktop\\Masters\\Step_Length.txt", 
            row.names=FALSE, sep = "\t", quote=FALSE)


###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))

