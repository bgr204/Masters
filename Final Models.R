#-------------------------Importing data----------------------------------------


###timer
start.time <- Sys.time()

###reading in urls from github
url5<-"https://raw.githubusercontent.com/bgr204/Masters/master/disturbance.csv" 
url6<-"https://raw.githubusercontent.com/bgr204/Masters/master/Step_Length.txt"
url7<-"https://raw.githubusercontent.com/bgr204/Masters/master/Home_Range.txt"
url8<-"https://raw.githubusercontent.com/bgr204/Masters/master/Distance_to_Road.txt"


###read url into csv
disturbance <- read.csv(url(url5), sep = ",")
step_length <- read.csv(url(url6), sep = "\t")
home_range <- read.csv(url(url7), sep = "\t")
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
library(forestmodel)


#-------------------------Formatting: Disturbance-------------------------------


###formatting
disturbance$vigilance <- log(ifelse(disturbance$vigilance == 0, 0, 
                                    disturbance$vigilance/disturbance$hours))
disturbance$walkrun <- ifelse(disturbance$walkrun == 0, 0, 
                              disturbance$walkrun/disturbance$hours)
disturbance$alarm <- ifelse(disturbance$alarm == 0, 0, 
                            disturbance$alarm/disturbance$hours)
disturbance$flight <- ifelse(disturbance$flight == 0, 0, 
                             disturbance$flight/disturbance$hours)
disturbance$human_rate <- as.numeric(disturbance$human_rate)
disturbance$start_time <- chron(times=disturbance$start_time)
disturbance$location_code <- as.factor(disturbance$location_code)
disturbance$precipitation <- as.numeric(factor(
  disturbance$precipitation, 
  levels = c("None", "Spots","Drizzle", "Light showers", "Showers")))-1
disturbance$precipitation <- ifelse(disturbance$precipitation > 0, 1, 0)
disturbance$wind_speed <- as.numeric(factor(disturbance$wind_speed), levels = 
                                     c("0-10","10-20","20-30","30-40","40-50"))
disturbance$birds <- (disturbance$birds_end+disturbance$birds_start)/2



#-------------------------Disturbance: Vigilance--------------------------------


###check normality
hist_vig <- hist(disturbance$vigilance) #count data

###mixed effects model
vig_m1 <- glmer(data = disturbance, vigilance~wind_speed+precipitation+birds+
                  human+(1|location_code), family = "poisson")
vig_m2 <- update(vig_m1,~.-human)
anova(vig_m1,vig_m2, test = "Chisq")

###plot
ggplot(data = disturbance, aes(x = human, y = vigilance))+
  geom_point() +
  geom_smooth(method = "lm")+
  xlim(0,120)+
  labs(x = "Count of People", y = "Count of Vigilance Behaviour")


#-------------------------Disturbance: Flight-----------------------------------


###check normality
hist_fli <- hist(disturbance$flight) #count data

###mixed effect model
fli_m1 <- glmer(data = disturbance, flight~wind_speed+precipitation+birds+
                  human_rate+(1|location_code), family = "poisson")
fli_m2 <- update(fli_m1,~.-human_rate)
anova(fli_m1,fli_m2, test = "Chisq")

###plot
ggplot(data = disturbance, aes(x = human_rate, y = flight))+
  geom_point() +
  geom_smooth(method = "lm")+
  xlim(0,120)+
  labs(x = "Count of People", y = "Count of Flight Behaviour")


#-------------------------Disturbance: Walkrun----------------------------------


###check normality
hist_wal <- hist(disturbance$walkrun) #count data

###mixed effect model
wal_m1 <- glmer(data = disturbance, walkrun~wind_speed+precipitation+birds+
                  human_rate+(1|location_code), family = "poisson")
wal_m2 <- update(wal_m1,~.-human_rate)
anova(wal_m1,wal_m2, test = "Chisq")

###plot
ggplot(data = disturbance, aes(x = human_rate, y = walkrun))+
  geom_point() +
  geom_smooth(method = "lm")+
  xlim(0,120)+
  labs(x = "Count of People", y = "Count of Walkrun Behaviour")

#-------------------------Disturbance: Alarm------------------------------------


###check normality
hist_ala <- hist(disturbance$alarm) #count data

#general linear model
ala_m1 <- glmer(data = disturbance, alarm~wind_speed+precipitation+birds+
                  human_rate+(1|location_code), family = "poisson")
ala_m2 <- update(ala_m1,~.-human_rate)
anova(ala_m1,ala_m2, test = "Chisq")

###plot
ggplot(data = disturbance, aes(x = human_rate, y = alarm))+
  geom_point() +
  geom_smooth(method = "lm")+
  xlim(0,120)+
  labs(x = "Count of People", y = "Count of Alarm Behaviour")


#-------------------------Formatting: Step Length-------------------------------


###formatting
step_length$species <- as.factor(step_length$species)
step_length$id <- as.factor(step_length$id)
step_length$is_weekend <- as.factor(step_length$is_weekend)


#-------------------------Step Length Model: All--------------------------------


###check normality
hist_step <- hist(step_length$distance) # looks fine, check residuals after

###mixed effects model
step_length_m1 <- lmer(data = step_length, distance~is_weekend*species+(1|id))
step_length_m2 <- update(step_length_m1,~.-is_weekend:species)
step_length_m3 <- update(step_length_m2,~.-is_weekend)
anova(step_length_m1,step_length_m2)
anova(step_length_m2,step_length_m3)
summary(step_length_m2)

###predicted values
step_length$predicted <- predict(step_length_m2, newdata = step_length)
###create plot
ggplot(step_length, aes(x = species, y = predicted, colour = is_weekend)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = is_weekend), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Species", y = "Daily Distance (km)", colour = "Weekend")


#-------------------------Formatting: Home Range--------------------------------


###formatting
home_range$species <- as.factor(home_range$species)
home_range$id <- as.factor(home_range$id)
home_range$is_weekend <- as.factor(home_range$is_weekend)
home_range$area <- home_range$area/1000000


#-------------------------Home Range Model: All---------------------------------


###check normality
hist_home <- hist(home_range$area) # looks highly right-skewed


###mixed effects model
home_range_m1 <- glmer(data = home_range, area~is_weekend*species+(1|id), 
                       family = Gamma(link = "log"))
home_range_m2 <- update(home_range_m1,~.-is_weekend:species)
home_range_m3 <- update(home_range_m2,~.-is_weekend)
anova(home_range_m1,home_range_m2)
anova(home_range_m2,home_range_m3)
summary(home_range_m2)

###predicted values
home_range$predicted <- predict(home_range_m2, newdata = home_range)

###back transform
home_range$predicted <- exp(home_range$predicted)

###create plot with raw data
ggplot(home_range, aes(x = species, y = predicted, colour = is_weekend)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = is_weekend), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75)) +
  labs(x = "Species", y = bquote('Utilisation Distribution  '(km^2)), colour = "Weekend")


#-------------------------Timer-------------------------------------------------


###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))