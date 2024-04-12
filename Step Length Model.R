#-------------------------Importing data----------------------------------------


###timer
start.time <- Sys.time()

###reading in urls from github
url6<-"https://raw.githubusercontent.com/bgr204/Masters/master/Step_Length.txt"

###read url into csv
step_length <- read.csv(url(url6), sep = "\t")


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
step_length$species <- as.factor(step_length$species)
step_length$id <- as.factor(step_length$id)
step_length$is_weekend <- as.factor(step_length$is_weekend)


#-------------------------Step Length Model: All--------------------------------


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
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "line", linetype = "dashed", size = 0.75, 
               position = position_dodge(width = 0.75)) +  # Add lines
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


#-------------------------Timer and Alarm---------------------------------------


###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))

###alarm
beepr::beep(0.5, 1)
