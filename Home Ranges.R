#-----------------------Setup---------------------------------------------------

start.time <- Sys.time()

urlfile1 <- "https://raw.githubusercontent.com/bgr204/Masters/master/all22.txt" 
urlfile2 <- "https://raw.githubusercontent.com/bgr204/Masters/master/all23.txt"
all_22 <- read.csv(url(urlfile1), sep = "\t")
all_23 <- read.csv(url(urlfile2), sep = "\t")
all <- rbind(all_22, all_23)

###library
library(sf)
library(sfheaders)
library(amt)
library(dplyr)
library(ggplot2)
library(tidygraph)
library(ggraph)
library(magrittr)

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
all_trk <- make_track(all_df, x, y, ts, day = day, date = UTC_date, id = device_id, species = species,
                         crs = 4326)



#-----------------------Home Ranges (rk = 44204 as example)---------------------


###Make Track for Redshank = 44204
#filter for rk
rk_trk <- all_trk %>% 
  filter(species == "RK")%>%
  filter(id == 44204)

###make trast
trast <- make_trast(rk_trk, res = 50)

###days of the week
date <- c(unique(rk_trk$date))

###loop to create home range sizes for each day of the week
#empty dataframe
results_df <- data.frame(day = character(), area = numeric())
#loop repeated for each day of the week
for (i in date) {
  #filter track by day of the week
  df1 <- rk_trk %>%
    filter(date == i)
  #calculate home range area
  df2 <- df1 %>%
    hr_akde(model = fit_ctmm(rk_trk, "auto"), 
            levels = c(0.5, 0.95),
            trast = trast) %>%
    hr_area()
  #extract value from results
  value <- df2[[2,3]]
  #create dataframe with a day and area column 
  df3 <- data.frame(date = i, area = value)
  #bind to empty dataframe
  results_df <- rbind(results_df, df3)
}

# Print the resulting dataframe
print(results_df)

results_df$date <- as.Date(results_df$date)
results_df$day <- weekdays(results_df$date)

end.time <- Sys.time()
print(round(end.time-start.time,2))


