#-------------------------Importing data----------------------------------------


###timer
start.time <- Sys.time()

###reading in urls from github
url7<-"https://raw.githubusercontent.com/bgr204/Masters/master/Home_Range.txt"

###read url into csv
home_range <- read.csv(url(url7), sep = "\t")


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
home_range$species <- as.factor(home_range$species)
home_range$id <- as.factor(home_range$id)
home_range$is_weekend <- as.factor(home_range$is_weekend)
home_range$area <- home_range$area/1000000


#-------------------------Home Range Model: All---------------------------------


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
###create plot with raw data
ggplot(home_range, aes(x = is_weekend, y = area, colour = species)) +
  geom_boxplot(outlier.shape = NA) +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "line", linetype = "dashed", size = 0.75, 
               position = position_dodge(width = 0.75)) +  # Add lines
  labs(title = "Daily Distance", x = "Weekend", y = "Daily Distance (km)")+
  ylim(0,5)
###with predicted values
ggplot(home_range, aes(x = is_weekend, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "line", linetype = "dashed", size = 0.75, 
               position = position_dodge(width = 0.75)) +  # Add lines
  labs(title = "Daily Distance", x = "Weekend", y = "Daily Distance (Predicted values)")


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


#-------------------------Home Range Model: OYC---------------------------------


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


#-------------------------Timer and Alarm---------------------------------------


###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))

###alarm
beepr::beep(0.5, 1)

