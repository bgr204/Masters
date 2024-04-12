#-------------------------Importing data----------------------------------------

###reading in urls from github
url5 <- "https://raw.githubusercontent.com/bgr204/Masters/master/disturbance.csv" 
url6 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Step_Length.txt"
url7 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Home_Range.txt"
url8 <- "https://raw.githubusercontent.com/bgr204/Masters/master/disturbance_no_nas.csv"
url9 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Distance_to_Road.txt"
url10 <- "https://raw.githubusercontent.com/bgr204/Masters/master/MSc%20projects/Tides/tide%20tables%2021-22.csv"
url11 <- "https://raw.githubusercontent.com/bgr204/Masters/master/MSc%20projects/Tides/tide%20tables%2022-23.csv"

###read url into csv
disturbance <- read.csv(url(url5), sep = ",")
disturbance2 <- read.csv(url(url8), sep = ",")
step_length <- read.csv(url(url6), sep = "\t")
home_range <- read.csv(url(url7), sep = "\t")
road_distance <- read.csv(url(url9), sep = "\t")
tide22 <- read.csv(url(url10), sep = ",")
tide23 <- read.csv(url(url11), sep = ",")


#-------------------------Library-----------------------------------------------

###library
library(chron)
library(effects)
library(MuMIn)
library(regclass)
library(lme4)
library(jtools)
library(ggeffects)
library(sjPlot)
library(ggplot2)
library(EnvStats)
library(forecast)
library(moments)
library(car)
library(suncalc)
library(dplyr)
library(gamm4)




#-------------------------Step Length Model: All--------------------------------


###formatting
step_length$species <- as.factor(step_length$species)
step_length$id <- as.factor(step_length$id)
step_length$is_weekend <- as.factor(step_length$is_weekend)

###check normality
hist(step_length$distance) # looks bimodal
jarque.test(step_length$distance) # definitely not normal
qqnorm(step_length$distance)
qqline(step_length$distance) #not normal
#fuck it, its normal because I don't know how to work with bimodal data

###mixed effects model
step_length_m1 <- lmer(data = step_length, distance~is_weekend*species+(1|id))
step_length_m2 <- update(step_length_m1,~.-is_weekend:species)
step_length_m3 <- update(step_length_m2,~.-is_weekend)
anova(step_length_m1,step_length_m2, test = "F")
anova(step_length_m2,step_length_m3, test = "F")
summary(step_length_m2)


#-------------------------Step Length Model: RK---------------------------------


###filter for OYC
step_rk <- step_length %>%
  filter(species == "RK")

###check normality
hist(step_rk$distance) # looks bimodal
shapiro.test(step_oyc$distance) # definitely not normal
qqnorm(step_rk$distance)
qqline(step_rk$distance) #not normal
#fuck it, its normal because I don't know how to work with bimodal data

###mixed effects model
step_rk_m1 <- gamm(data = step_rk, distance~s(is_weekend)+(1|id))
step_rk_m2 <- update(step_rk_m1,~.-is_weekend)
anova(step_rk_m1,step_rk_m2, test = "F")
summary(step_rk_m1)

###using gam
step_rk_m3 <- gamm(data = step_rk, distance~is_weekend+s(id, bs = 're'))
step_rk_m4 <- gamm(data = step_rk, distance~s(id, bs = 're'))
anova(step_rk_m1,step_rk_m2, test = "F")
summary(model1$gam)
summary(model2$gam)


#-------------------------Step Length Model: OYC--------------------------------


###filter for OYC
step_oyc <- step_length %>%
  filter(species == "OYC")

###check normality
hist(step_oyc$distance) # looks bimodal
shapiro.test(step_oyc$distance) # definitely not normal
qqnorm(step_oyc$distance)
qqline(step_oyc$distance) #not normal
#fuck it, its normal because I don't know how to work with bimodal data

###mixed effects model
step_oyc_m1 <- lmer(data = step_oyc, distance~is_weekend+(1|id))
step_oyc_m2 <- update(step_oyc_m1,~.-is_weekend)
anova(step_oyc_m1,step_oyc_m2, test = "F")
summary(step_oyc_m1)


#-------------------------Step Length Model: GOD--------------------------------


###filter for GOD
step_god <- step_length %>%
  filter(species == "GOD")

###check normality
hist(step_god$distance) # looks bimodal
shapiro.test(step_god$distance) # definitely not normal
qqnorm(step_god$distance)
qqline(step_god$distance) #not normal
#fuck it, its normal because I don't know how to work with bimodal data

###mixed effects model
step_god_m1 <- lmer(data = step_god, distance~is_weekend+(1|id))
step_god_m2 <- update(step_god_m1,~.-is_weekend)
anova(step_god_m1,step_god_m2, test = "F")
summary(step_god_m1)


#-------------------------Step Length Model: CU---------------------------------


###filter for CU
step_cu <- step_length %>%
  filter(species == "CU")

###check normality
hist(step_cu$distance) # looks bimodal
shapiro.test(step_cu$distance) # definitely not normal
qqnorm(step_cu$distance)
qqline(step_cu$distance) #not normal
#fuck it, its normal because I don't know how to work with bimodal data

###mixed effects model
step_cu_m1 <- lmer(data = step_cu, distance~is_weekend+(1|id))
step_cu_m2 <- update(step_cu_m1,~.-is_weekend)
anova(step_cu_m1,step_cu_m2, test = "F")
summary(step_cu_m1)


#-------------------------Home Range Model: All---------------------------------


###formatting
home_range$species <- as.factor(home_range$species)
home_range$id <- as.factor(home_range$id)
home_range$is_weekend <- as.factor(home_range$is_weekend)
home_range$area <- home_range$area/1000000

###check normality
hist(home_range$area) # looks highly right-skewed
jarque.test(home_range$area) # definitely not normal
qqnorm(home_range$area)
qqline(home_range$area) #not normal

###mixed effects model
home_range_m1 <- glmer(data = home_range, area~is_weekend*species+(1|id), 
                       family = Gamma(link = "log"))
home_range_m2 <- update(home_range_m1,~.-is_weekend:species)
home_range_m3 <- update(home_range_m2,~.-is_weekend)
anova(home_range_m1,home_range_m2)
anova(home_range_m2,home_range_m3)
summary(home_range_m2)


#-------------------------Home Range Model: RK----------------------------------


###filter for RK
home_rk <- home_range %>%
  filter(species == "RK")

###check normality
hist(home_rk$area) # looks highly right-skewed
shapiro.test(home_rk$area) # definitely not normal
qqnorm(home_rk$area)
qqline(home_rk$area) #not normal

###mixed effects model
home_rk_m1 <- glmer(data = home_rk, area~is_weekend+(1|id), 
                       family = Gamma(link = "log"))
home_rk_m2 <- update(home_rk_m1,~.-is_weekend)
anova(home_rk_m1,home_rk_m2)
summary(home_rk_m1)


#-------------------------Home Range Model: OYC----------------------------------


###filter for OYC
home_oyc <- home_range %>%
  filter(species == "OYC")

###check normality
hist(home_oyc$area) # looks highly right-skewed
shapiro.test(home_oyc$area) # definitely not normal
qqnorm(home_oyc$area)
qqline(home_oyc$area) #not normal

###mixed effects model
home_oyc_m1 <- glmer(data = home_oyc, area~is_weekend+(1|id), 
                    family = Gamma(link = "log"))
home_oyc_m2 <- update(home_oyc_m1,~.-is_weekend)
anova(home_oyc_m1,home_oyc_m2)
summary(home_oyc_m1)


#-------------------------Home Range Model: GOD---------------------------------


###filter for GOD
home_god <- home_range %>%
  filter(species == "GOD")

###check normality
hist(home_god$area) # looks highly right-skewed
shapiro.test(home_god$area) # definitely not normal
qqnorm(home_god$area)
qqline(home_god$area) #not normal
#fuck it, its normal because I don't know how to work with bimodal data

###mixed effects model
home_god_m1 <- glmer(data = home_god, area~is_weekend+(1|id), 
                    family = Gamma(link = "log"))
home_god_m2 <- update(home_god_m1,~.-is_weekend)
anova(home_god_m1,home_god_m2)
summary(home_god_m1)


#-------------------------Home Range Model: CU----------------------------------


###filter for CU
home_cu <- home_range %>%
  filter(species == "CU")

###check normality
hist(home_cu$area) # looks highly right-skewed
shapiro.test(home_cu$area) # definitely not normal
qqnorm(home_cu$area)
qqline(home_cu$area) #not normal
#fuck it, its normal because I don't know how to wocu with bimodal data

###mixed effects model
home_cu_m1 <- glmer(data = home_cu, area~is_weekend+(1|id), 
                    family = Gamma(link = "log"))
home_cu_m2 <- update(home_cu_m1,~.-is_weekend)
anova(home_cu_m1,home_cu_m2)
summary(home_cu_m1)


#-------------------------Distance to Road: All---------------------------------


###formatting
#as.factor
road_distance$species <- as.factor(road_distance$species)
road_distance$device_id <- as.factor(road_distance$device_id)
road_distance$is_weekend <- as.factor(road_distance$is_weekend)
road_distance$road_group <- as.factor(road_distance$road_group)
road_distance$daylight <- as.factor(road_distance$daylight)
road_distance$time <- as.POSIXct(road_distance$UTC_datetime, format = "%Y-%m-%d %H:%M:%S")
road_distance$time <- format(road_distance$time, format = "%H")
road_distance$traffic <- as.factor(ifelse(road_distance$time %in% c(7,8,9,15,16,17,18,19), "peak", "offpeak"))


###check normality
hist(road_distance$distance) # looks slightly right-skewed
jarque.test(road_distance$distance) # definitely not normal
qqnorm(road_distance$distance)
qqline(road_distance$distance) #not normal

###mixed effects model
###mixed effects model
road_distance_m1 <- glmer(data = road_distance, distance~is_weekend+species+road_group+traffic+daylight+(1|device_id), 
                       family = Gamma(link = "log"))
home_range_m2 <- update(home_range_m1,~.-is_weekend:species)
home_range_m3 <- update(home_range_m2,~.-is_weekend)
anova(home_range_m1,home_range_m2)
anova(home_range_m2,home_range_m3)
summary(home_range_m2)






