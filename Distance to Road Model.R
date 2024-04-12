#-------------------------Importing data----------------------------------------


###timer
start.time <- Sys.time()

###reading in urls from github
url8<-"https://raw.githubusercontent.com/bgr204/Masters/master/Distance_to_Road.txt"

###read url into csv
road_distance <- read.csv(url(url8), sep = "\t")


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

#-------------------------Formatting--------------------------------------------


###formatting
road_distance$species <- as.factor(road_distance$species)
road_distance$device_id <- as.factor(road_distance$device_id)
road_distance$is_weekend <- as.factor(road_distance$is_weekend)
road_distance$road_group <- as.factor(road_distance$road_group)
road_distance$daylight <- as.factor(road_distance$daylight)
road_distance$time <- as.POSIXct(road_distance$UTC_datetime, 
                                 format = "%Y-%m-%d %H:%M:%S")
road_distance$time <- format(road_distance$time, format = "%H")
road_distance$traffic <- as.factor(ifelse(road_distance$time %in% 
                                   c(7,8,9,15,16,17,18,19), "peak", "offpeak"))


#-------------------------Distance to Road: All---------------------------------


###check normality
hist(road_distance$distance) # looks slightly right-skewed
jarque.test(road_distance$distance) # definitely not normal
qqnorm(road_distance$distance)
qqline(road_distance$distance) #not normal

###mixed effects model
###mixed effects model
road_distance_m1 <- glmer(data = road_distance, distance~is_weekend+species
                          +road_group+traffic+daylight+(1|device_id), 
                          family = Gamma(link = "log"))
road_distance_m2 <- update(road_distance_m1,~.-is_weekend:species)
road_distance_m3 <- update(road_distance_m2,~.-is_weekend)
anova(road_distance_m1,road_distance_m2)
anova(road_distance_m2,road_distance_m3)
summary(road_distance_m2)

###predicted values
road_distance$predicted <- predict(road_distance_m1, newdata = road_distance)

###create plot - weekend effect
ggplot(road_distance, aes(x = is_weekend, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "line", linetype = "dashed", size = 0.75, 
               position = position_dodge(width = 0.75)) +  # Add lines
  labs(title = "Daily Distance", x = "Weekend", y = "Daily Distance (km)")

###create plot - road group effect
ggplot(road_distance, aes(x = road_group, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "line", linetype = "dashed", size = 0.75, 
               position = position_dodge(width = 0.75)) +  # Add lines
  labs(title = "Daily Distance", x = "Road Group", y = "Daily Distance (km)")

###create plot - traffic effect
ggplot(road_distance, aes(x = traffic, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "line", linetype = "dashed", size = 0.75, 
               position = position_dodge(width = 0.75)) +  # Add lines
  labs(title = "Daily Distance", x = "Traffic", y = "Daily Distance (km)")


#-------------------------Distance to Road: RK----------------------------------


###filter for RK
road_rk <- road_distance %>%
  filter(species == "RK")

###check normality
hist(road_rk$distance) # looks highly right-skewed
jarque.test(road_rk$distance) # definitely not normal
qqnorm(road_rk$distance)
qqline(road_rk$distance) #not normal

###mixed effects model
road_rk_m1 <- lmer(data = road_rk, distance~is_weekend+road_group+traffic
                   +daylight+(1|device_id))
Anova(road_rk_m1, type = "III")
summary(road_rk_m1)


#-------------------------Distance to Road: OYC---------------------------------


###filter for OYC
road_oyc <- road_distance %>%
  filter(species == "OYC")

###check normality
hist(road_oyc$distance) # looks highly right-skewed
jarque.test(road_oyc$distance) # definitely not normal
qqnorm(road_oyc$distance)
qqline(road_oyc$distance) #not normal

###mixed effects model
road_oyc_m1 <- glmer(data = road_oyc, distance~is_weekend+road_group+traffic
                     +daylight+(1|device_id), family = Gamma(link = "log"))
road_oyc_m2 <- update(road_oyc_m1,~.-is_weekend)
road_oyc_m3 <- update(road_oyc_m1,~.-road_group)
road_oyc_m4 <- update(road_oyc_m1,~.-traffic)
road_oyc_m5 <- update(road_oyc_m1,~.-daylight)
anova(road_oyc_m1, road_oyc_m2)
anova(road_oyc_m1, road_oyc_m3)
anova(road_oyc_m1, road_oyc_m4)
anova(road_oyc_m1, road_oyc_m5)
summary(road_oyc_m1)


#-------------------------Distance to Road: GOD---------------------------------


###filter for GOD
road_god <- road_distance %>%
  filter(species == "GOD")

###check normality
hist(road_god$distance) # looks highly right-skewed
jarque.test(road_god$distance) # definitely not normal
qqnorm(road_god$distance)
qqline(road_god$distance) #not normal

###mixed effects model
road_god_m1 <- lmer(data = road_god, distance~is_weekend+road_group+traffic
                    +daylight+(1|device_id))
Anova(road_god_m1, type = "III")
summary(road_god_m1)


#-------------------------Distance to Road: CU----------------------------------


###filter for CU
road_cu <- road_distance %>%
  filter(species == "CU")

###check normality
hist(road_cu$distance) # looks highly right-skewed
jarque.test(road_cu$distance) # definitely not normal
qqnorm(road_cu$distance)
qqline(road_cu$distance) #not normal

###mixed effects model
road_cu_m1 <- glmer(data = road_cu, distance~is_weekend+road_group+traffic
                    +daylight+(1|device_id), family = Gamma(link = "log"))
road_cu_m2 <- update(road_cu_m1,~.-is_weekend)
road_cu_m3 <- update(road_cu_m1,~.-road_group)
road_cu_m4 <- update(road_cu_m1,~.-traffic)
road_cu_m5 <- update(road_cu_m1,~.-daylight)
anova(road_cu_m1, road_cu_m2)
anova(road_cu_m1, road_cu_m3)
anova(road_cu_m1, road_cu_m4)
anova(road_cu_m1, road_cu_m5)
summary(road_cu_m1)


#-------------------------Timer and Alarm---------------------------------------


###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))

###alarm
beepr::beep(0.5, 1)
