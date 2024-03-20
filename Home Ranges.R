#-----------------------Setup---------------------------------------------------

###work directory
setwd("C:/Users/bgroo/Desktop/Dissertation/Data/All")
getwd()

###library
library(sf)
library(sfheaders)
library(amt)
library(dplyr)
library(ggplot2)
library(tidygraph)
library(ggraph)
library(magrittr)

###import data
all_22 <- read.csv("all22.csv")

###remove NAs
all_22 <- all_22 %>% 
  filter(!is.na(UTC_datetime))

###Convert UTC_datetime to POSIXct. Make sure that this matches the format in 
#the columns otherwise will return NAs
all_22$ts <- as.POSIXct(all_22$UTC_datetime, 
                        format = "%Y-%m-%d %H:%M:%S") 

###create column with day
#format UTC-date as a date
all_22$UTC_date <- as.Date(all_22$UTC_date)
#create a column for days
all_22$day <- weekdays(all_22$UTC_date)

###select columns that are needed
all_22 <- all_22 %>% 
  dplyr::select(Longitude, Latitude, ts, device_id, day, species)

###filtering to remove migration
#making a simple feature
all_22_sf <- st_as_sf(all_22, coords=c("Longitude","Latitude"), crs = 4326)
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
all_22_sf <- all_22_sf[dubpoly,]
#convert simple feature back into a normal dataset
all_22_df <- sf_to_df(all_22_sf, fill = TRUE)

###remove NAs
all_22_df <- all_22_df %>% 
  filter(!is.na(ts))

###check if all observations are complete
all(complete.cases(all_22_df))

###removing duplicates
#check for duplicated time stamps
any(duplicated(all_22_df$ts))
#remove duplicates
all_22_df <- all_22_df[!duplicated(all_22_df$ts), ]

###defining weekend and day
workday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
weekend <- c("Saturday","Sunday")

####make track
all_22_trk <- make_track(all_22_df, x, y, ts, id = device_id, day = day, species = species,
                         crs = 4326)



#-----------------------Home Ranges (rk = 44204 as example)---------------------


###Make Track for Redshank = 44204
#filter for rk
rk_trk <- all_22_trk %>% 
  filter(species == "RK")%>%
  filter(id == 44204)

###make trast
trast <- make_trast(rk_trk, res = 50)

###days of the week
dayofweek <- c(unique(rk_trk$day))

###loop to create home range sizes for each day of the week
#empty dataframe
results_df <- data.frame(day = character(), area = numeric())
#loop repeated for each day of the week
for (i in dayofweek) {
  #filter track by day of the week
  df1 <- rk_trk %>%
    filter(day == i)
  #calculate home range area
  df2 <- df1 %>%
    hr_akde(model = fit_ctmm(rk_trk, "auto"), 
            levels = c(0.5, 0.95),
            trast = trast) %>%
    hr_area()
  #extract value from results
  value <- df2[[2,3]]
  #create dataframe with a day and area column
  df3 <- data.frame(day = i, area = value)
  #bind to empty dataframe
  results_df <- rbind(results_df, df3)
}

# Print the resulting dataframe
print(results_df)
