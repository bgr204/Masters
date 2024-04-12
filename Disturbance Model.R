#-------------------------Importing data----------------------------------------


###timer
start.time <- Sys.time()

###reading in urls from github
url5<-"https://raw.githubusercontent.com/bgr204/Masters/master/disturbance.csv" 

###read url into csv
disturbance <- read.csv(url(url5), sep = ",")


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


#-------------------------Disturbance: Vigilance--------------------------------


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


#-------------------------Timer and Alarm---------------------------------------


###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))

###alarm
beepr::beep(0.5, 1)

