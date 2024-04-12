#-------------------------Importing data----------------------------------------

###timer
start.time <- Sys.time()

###reading in urls from github
url5 <- "https://raw.githubusercontent.com/bgr204/Masters/master/disturbance.csv" 
url6 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Step_Length.txt"
url7 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Home_Range.txt"
url8 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Distance_to_Road.txt"
url9 <- "https://raw.githubusercontent.com/bgr204/Masters/master/MSc%20projects/Tides/tide%20tables%2021-22.csv"
url10 <- "https://raw.githubusercontent.com/bgr204/Masters/master/MSc%20projects/Tides/tide%20tables%2022-23.csv"

###read url into csv
disturbance <- read.csv(url(url5), sep = ",")
step_length <- read.csv(url(url6), sep = "\t")
home_range <- read.csv(url(url7), sep = "\t")
road_distance <- read.csv(url(url8), sep = "\t")
tide22 <- read.csv(url(url9), sep = ",")
tide23 <- read.csv(url(url10), sep = ",")


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
anova(step_length_m1,step_length_m2)
anova(step_length_m2,step_length_m3)
summary(step_length_m2)

###predicted values
step_length$predicted <- predict(step_length_m1, newdata = step_length)
###create plot
ggplot(step_length, aes(x = is_weekend, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), geom = "point", shape = 18, size = 3, position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), geom = "line", linetype = "dashed", size = 0.75, position = position_dodge(width = 0.75)) +  # Add lines between medians
  labs(title = "Daily Distance", x = "Weekend", y = "Daily Distance (km)")



#-------------------------Step Length Model: RK---------------------------------


###filter for OYC
step_rk <- step_length %>%
  filter(species == "RK")

###check normality
hist(step_rk$distance) # looks bimodal
shapiro.test(step_rk$distance) # definitely not normal
qqnorm(step_rk$distance)
qqline(step_rk$distance) #not normal
#fuck it, its normal because I don't know how to work with bimodal data

###mixed effects model
step_rk_m1 <- lmer(data = step_rk, distance~is_weekend+(1|id))
step_rk_m2 <- update(step_rk_m1,~.-is_weekend)
anova(step_rk_m1,step_rk_m2)
summary(step_rk_m1)

###using gam
step_rk_m3 <- gamm(data = step_rk, distance~is_weekend+s(id, bs = 're'))
step_rk_m4 <- gamm(data = step_rk, distance~s(id, bs = 're'))
anova(step_rk_m3$gam,step_rk_m4$gam)
summary(step_rk_m3$gam)


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
anova(step_oyc_m1,step_oyc_m2)
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
anova(step_god_m1,step_god_m2)
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
anova(step_cu_m1,step_cu_m2)
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

###predicted values
home_range$predicted <- predict(home_range_m1, newdata = home_range)
###create plot
ggplot(home_range, aes(x = is_weekend, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), geom = "point", shape = 18, size = 3, position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), geom = "line", linetype = "dashed", size = 0.75, position = position_dodge(width = 0.75)) +  # Add lines between medians
  labs(title = "Daily Distance", x = "Weekend", y = "Daily Distance (km)")


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
  stat_summary(fun = median, aes(group = species, color = species), geom = "point", shape = 18, size = 3, position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), geom = "line", linetype = "dashed", size = 0.75, position = position_dodge(width = 0.75)) +  # Add lines between medians
  labs(title = "Daily Distance", x = "Weekend", y = "Daily Distance (km)")

###create plot - road group effect
ggplot(road_distance, aes(x = road_group, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), geom = "point", shape = 18, size = 3, position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), geom = "line", linetype = "dashed", size = 0.75, position = position_dodge(width = 0.75)) +  # Add lines between medians
  labs(title = "Daily Distance", x = "Road Group", y = "Daily Distance (km)")

###create plot - traffic effect
ggplot(road_distance, aes(x = traffic, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), geom = "point", shape = 18, size = 3, position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), geom = "line", linetype = "dashed", size = 0.75, position = position_dodge(width = 0.75)) +  # Add lines between medians
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


#-------------------------Disturbance: Vigilance--------------------------------


###formatting
disturbance$vigilance <- as.numeric(disturbance$vigilance)
disturbance$human_rate <- as.numeric(disturbance$human_rate)
disturbance$start_time <- chron(times=disturbance$start_time)
disturbance$location_code <- as.factor(disturbance$location_code)
disturbance$precipitation <- as.numeric(factor(disturbance$precipitation, 
                                               levels = c("None", "Spots","Drizzle",
                                                          "Light showers", "Showers")))-1
disturbance$precipitation <- ifelse(disturbance$precipitation > 0, 1, 0)
disturbance$wind_speed <- as.numeric(factor(disturbance$wind_speed), levels = 
                                       c("0-10","10-20","20-30","30-40","40-50"))
disturbance$birds <- (disturbance$birds_end+disturbance$birds_start)/2
disturbance$vig_prop <- disturbance$vigilance/disturbance$birds
disturbance$vig_prop[is.na(disturbance$vig_prop)] <- 0

###check normality
hist(disturbance$vigilance) #count data
shapiro.test(disturbance$vigilance) # definitely not normal
qqnorm(disturbance$vigilance)
qqline(disturbance$vigilance) #not normal

###mixed effects model
vig_m1 <- glmer(data = disturbance, vigilance~wind_speed+precipitation+birds+
                human_rate+(1|location_code), family = "poisson")
vig_m2 <- update(vig_m1,~.-human_rate)
anova(vig_m1,vig_m2, test = "Chisq")

###plot
ggplot(data = disturbance, aes(x = human_rate, y = vigilance))+
  geom_point() +
  geom_smooth(method = "lm")


#-------------------------Disturbance: Flight-----------------------------------


###check normality
hist(disturbance$flight) #count data
shapiro.test(disturbance$flight) # definitely not normal
qqnorm(disturbance$flight)
qqline(disturbance$flight) #not normal

###mixed effect model
fli_m1 <- glmer(data = disturbance, flight~wind_speed+precipitation+birds+
                human_rate+(1|location_code), family = "poisson")
fli_m2 <- update(fli_m1,~.-human_rate)
anova(fli_m1,fli_m2, test = "Chisq")

###plot
ggplot(data = disturbance, aes(x = human_rate, y = flight))+
  geom_point() +
  geom_smooth(method = "lm")


#-------------------------Disturbance: Walkrun----------------------------------


###check normality
hist(disturbance$walkrun) #count data
shapiro.test(disturbance$walkrun) # definitely not normal
qqnorm(disturbance$walkrun)
qqline(disturbance$walkrun) #not normal

###mixed effect model
wal_m1 <- glmer(data = disturbance, walkrun~wind_speed+precipitation+birds+
                  human_rate+(1|location_code), family = "poisson")
wal_m2 <- update(wal_m1,~.-human_rate)
anova(wal_m1,wal_m2, test = "Chisq")

###plot
ggplot(data = disturbance, aes(x = human_rate, y = walkrun))+
  geom_point() +
  geom_smooth(method = "lm")

#-------------------------Disturbance: Alarm------------------------------------


###check normality
hist(disturbance$alarm) #count data
shapiro.test(disturbance$alarm) # definitely not normal
qqnorm(disturbance$alarm)
qqline(disturbance$alarm) #not normal

#general linear model
ala_m1 <- glmer(data = disturbance, alarm~wind_speed+precipitation+birds+
                  human_rate+(1|location_code), family = "poisson")
ala_m2 <- update(ala_m1,~.-human_rate)
anova(ala_m1,ala_m2, test = "Chisq")

###plot
ggplot(data = disturbance, aes(x = human_rate, y = alarm))+
  geom_point() +
  geom_smooth(method = "lm")

###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))

beepr::beep(0.5, 1)





