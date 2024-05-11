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
library(dplyr)
library(ggplot2)
library(tidygraph)
library(ggraph)
library(magrittr)
library(osmdata)
library(raster)
library(beepr)
library(circular)
library(Bessel)
library(fasttime)
library(Gmedian)
library(manipulate)
library(parsedate)
library(ctmm)
library(fitdistrplus)
library(amt)
library(proj4)
library(PROJ)
library(crsmeta)
library(reproj)
library(osmdata)
library(ggspatial)
library(leaflet)
library(rnaturalearth)

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

###create matrix of coordinates for dublin area
dublin_bb <- matrix(data = c(-6.3, -6, 53.29, 53.55),
                    nrow = 2,
                    byrow = TRUE)
# Update column and row names
colnames(dublin_bb) <- c("min", "max")
rownames(dublin_bb) <- c("x", "y")

###creating daylight column
#create function
is_daylight <- function(datetime, latitude, longitude) {
  # Calculate sunrise and sunset times
  times <- suncalc::getSunlightTimes(date = as.Date(datetime), lat = latitude, lon = longitude)
  
  # Determine if it's daylight
  daylight <- datetime >= times$sunrise && datetime <= times$sunset
  
  return(daylight)
}
#apply the function to the dataset
all_sf2$daylight <- mapply(is_daylight, all_trk2$t_, all_trk2$y_, all_trk2$x_)

rk_day <- all_trk2 %>%
  filter(daylight == "TRUE") %>%
  filter(species == "RK")

rk_night <- all_trk2 %>%
  filter(daylight == "FALSE") %>%
  filter(species == "RK")


# Latitude and Longitude of the center point
lat <- 53.42  
lon <- -6.15  

# Create the map
rk_day_plot <- leaflet(data = rk_day) %>%
  addTiles() %>%  # Add default base layer tiles
  
  # Add points from your data frame
  addCircleMarkers(
    lng = ~x_,      # Specify the longitude column
    lat = ~y_,      # Specify the latitude column
    color = "#B8562B",  # The color you mentioned
    radius = 3,        # The size of the markers
    stroke = FALSE,    # No border around the markers
    fillOpacity = 0.8  # Semi-transparent fill
  )
rk_plot <- leaflet() %>%
  addTiles() %>%  
  addCircleMarkers(data = rk_night,
    lng = ~x_,      # Specify the longitude column
    lat = ~y_,      # Specify the latitude column
    color = "green",  # The color you mentioned
    radius = 3,        # The size of the markers
    stroke = FALSE,    # No border around the markers
    fillOpacity = 0.8) %>%  # Semi-transparent fill 
    addCircleMarkers(data = rk_day,
      lng = ~x_,      # Specify the longitude column
      lat = ~y_,      # Specify the latitude column
      color = "red",  # The color you mentioned
      radius = 3,        # The size of the markers
      stroke = FALSE,    # No border around the markers
      fillOpacity = 0.8  # Semi-transparent fill
  )
rk_plot

rk_day_kde <- all_sf2 %>%
  filter(daylight == "TRUE") %>%
  filter(species == "RK") %>%
  eks::st_kde() %>%
  eks::st_get_contour(., cont=c(95)) 

rk_day_plot <- leaflet() %>%
  addTiles() %>%
  addPolygons(data = rk_day_kde, 
              fillColor = "red",
              fillOpacity = 0.2,
              weight = 1)  # Customize these styles as needed

# Display the map
rk_day_plot

rk_kde <- all_sf2 %>%
  eks::st_kde() %>%
  eks::st_get_contour(cont = 95)  # Extract the 95% contour
# Define bounding box using sf bbox
bbox_sf <- st_bbox(c(xmin = -6.3, xmax = -6, ymin = 53.29, ymax = 53.55), crs = st_crs(4326))

# Get OSM tiles at an appropriate zoom level
osm_base <- get_tiles(bbox = bbox_sf, type = "osm", zoom = 12)

# Create a ggplot map with the OSM basemap
rk_ggplot_map <- ggplot() +
  annotation_custom(ggplotGrob(ggmap(osm_base)), 
                    xmin = bbox_sf["xmin"], xmax = bbox_sf["xmax"], 
                    ymin = bbox_sf["ymin"], ymax = bbox_sf["ymax"]) +
  geom_sf(data = rk_kde, fill = "blue", color = "darkblue", size = 0.5, alpha = 0.5) +
  labs(title = "Kernel Density Estimation Contour over Dublin") +
  coord_sf(xlim = c(bbox_sf["xmin"], bbox_sf["xmax"]), ylim = c(bbox_sf["ymin"], bbox_sf["ymax"])) +
  theme_minimal()

# Print the map
print(rk_ggplot_map)
