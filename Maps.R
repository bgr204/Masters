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
all2 <- rbind(all_22, all_23)
tag2 <- rbind(tag_22, tag_23)

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
library(osmdata)
library(raster)

###changing format of coordinates
all2$Longitude <- as.numeric(all2$Longitude)
all2$Latitude <- as.numeric(all2$Latitude)

###remove NAs
all2 <- all2 %>% 
  filter(!is.na(UTC_datetime)) %>%
  filter(!is.na(Latitude))

###Convert UTC_datetime to POSIXct
all2$UTC_datetime <- as.POSIXct(all2$UTC_datetime, 
                               format = "%Y-%m-%d %H:%M:%S") 

###remove NAs - why are they introduced by the previous code?
all2 <- all2 %>% 
  filter(!is.na(UTC_datetime))

###adding catch date to the dataframe
all2$catch_date <- with(tag2, deploy_date[match(all2$device_id, ID)])
#formatting date
all2$catch_date <- as.Date(all2$catch_date, format = "%d/%m/%Y")
#creating column for day after
all2$day_after <- all2$catch_date+1
#removing data from catch dates
all2 <- subset(all2, UTC_date != catch_date)
all2 <- subset(all2, UTC_date != day_after)

###create column with day
#format UTC-date as a date
all2$UTC_date <- as.Date(all2$UTC_date)
#create a column for days
all2$day <- weekdays(all2$UTC_date)

###select columns that are needed
all2 <- all2 %>% 
  dplyr::select(Longitude, Latitude, UTC_datetime, day, UTC_date, device_id, species)

###filtering to remove migration
#making a simple feature
all_sf2 <- st_as_sf(all2, coords=c("Longitude","Latitude"), crs = 4326)
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
all_sf2 <- all_sf2[dubpoly,]
#convert simple feature back into a normal dataset
all_df2 <- sf_to_df(all_sf2, fill = TRUE)

###creating objects
workday <- c("Monday","Tuesday","Wednesday","Thursday","Friday")
weekend <- c("Saturday","Sunday")
species <- c(unique(all2$species))

####make track
all_trk2 <- make_track(all_df2, x, y, UTC_datetime, day = day, date = UTC_date, 
                      id = device_id, species = species,
                      crs = 4326)

#-------------------------Distance to roads-------------------------------------

# A 2x2 matrix
dublin_bb <- matrix(data = c(-6.3, -6, 53.29, 53.55),
                    nrow = 2,
                    byrow = TRUE)
# Update column and row names
colnames(dublin_bb) <- c("min", "max")
rownames(dublin_bb) <- c("x", "y")
# Print the matrix to the console
dublin_bb

#adding major roads
dublin_major <- dublin_bb %>%
  opq() %>%
  add_osm_feature(key = "highway", value = "footway") %>%
  osmdata_sf()

highway_values <- c("footway", "service", "steps", "path", "unclassified", "tertiary", "track", "residential", "secondary", "cycleway", "pedestrian", "motorway", "tertiary_link", "construction", "motorway_link")
roads <- dublin_major$osm_lines

all_sf2$nearest_feature <- st_nearest_feature(all_sf2, roads)
all_sf2$road <- with(roads, highway[all_sf2$nearest_feature])
all_sf2$distance <- st_distance(all_sf2, roads)













#creating first plot
street_plot <- ggplot() +
  geom_sf(data = dublin_major$osm_lines,
          inherit.aes = FALSE,
          color = "black",
          size = 0.2)

track_plot <- street_plot + 
  geom_point(data = all_trk,
             mapping = aes(x = x_, y = y_),
             color = "#B8562B", # an orange to match the species' shoulders
             size = 1, 
             inherit.aes = FALSE)
track_plot

###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))

