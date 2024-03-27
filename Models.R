#-------------------------Importing data----------------------------------------

###reading in urls from github
url5 <- "https://raw.githubusercontent.com/bgr204/Masters/master/disturbance.csv" 
url6 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Step_Length.txt"
url7 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Home_Range.txt"

###read url into csv
disturbance <- read.csv(url(url5), sep = ",")
step_length <- read.csv(url(url6), sep = "\t")
home_range <- read.csv(url(url7), sep = "\t")



#-------------------------Library-----------------------------------------------

###library
library(chron)
library(effects)
library(MuMIn)


#-------------------------Formatting--------------------------------------------

###changing column formats
disturbance$human_rate <- as.numeric(disturbance$human_rate)
disturbance$start_time <- chron(times=disturbance$start_time)
disturbance$location_code <- as.factor(disturbance$location_code)


#-------------------------Disturbance Models------------------------------------

disturbance_m1 <- glm(data = disturbance, vigilance~start_time*tide_state*wind*
                        precipitation*cloud*birds_start*human_rate*location_code)
cor(disturbance)
