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
disturbance$vigilance <- as.numeric(disturbance$vigilance)
disturbance$walkrun <- as.numeric(disturbance$walkrun)
disturbance$flight <- as.numeric(disturbance$flight)
disturbance$alarm <- as.numeric(disturbance$alarm)
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

###check normality in residuals
plot(resid(step_length_m2))

###predicted values
step_length$predicted <- predict(step_length_m2, newdata = step_length)
###create plot
step_plot <- ggplot(step_length, aes(x = species, y = predicted, colour = is_weekend)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = is_weekend), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Species", y = "Daily Distance Traveled (km)", colour = "Time of Week")+
  scale_colour_discrete(labels = c("Weekday", "Weekend"))


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

###check normality in residuals
plot(resid(home_range_m2))

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
  labs(x = "Species", y = bquote('Daily Utilisation Distribution  '(km^2)), 
       colour = "Time of Week")+
  scale_colour_discrete(labels = c("Weekday", "Weekend"))


#-------------------------Formatting: Distance to Road--------------------------


###formatting
road_distance$species <- as.factor(road_distance$species)
road_distance$device_id <- as.factor(road_distance$device_id)
road_distance$is_weekend <- as.factor(road_distance$is_weekend)
road_distance$daylight <- factor(road_distance$daylight, 
                                 levels = c("TRUE","FALSE"))
road_distance$time <- as.POSIXct(road_distance$UTC_datetime, 
                                 format = "%Y-%m-%d %H:%M:%S")
road_distance$time <- format(road_distance$time, format = "%H")
road_distance$traffic <- as.factor(ifelse(road_distance$time %in% 
                                    c(7,8,9,15,16,17,18,19), "peak", "offpeak"))
road_distance$road_group <- ifelse(road_distance$road_group == "motorway", 
                                   "major_road",road_distance$road_group)
road_distance$road_group <- factor(road_distance$road_group, 
                                   levels = c("footpath","minor_road",
                                              "major_road"))



#-------------------------Distance to Road Model: RK----------------------------


###filter for RK
road_rk <- road_distance %>%
  filter(species == "RK")

###check normality
hist_rk <- hist(road_rk$distance)

###mixed effects model
road_rk_m1 <- lmer(data = road_rk, distance~is_weekend+road_group+traffic
                   +daylight+road_group:is_weekend+road_group:traffic
                   +road_group:daylight+(1|device_id), 
                   na.action = "na.fail")

###dredge global model
dd_rk <- dredge(road_rk_m1)

###extract best model
road_rk_best <- get.models(dd_rk, 1)[[1]]

###check normality in residuals
plot(resid(road_rk_best))

###plot confidence intervals
plot_model(road_rk_best, vline.color = "black", 
           order.terms = c(4,3,5,1,2,6,7,10,11,8,9), show.values = TRUE, 
           value.offset = .3, axis.labels = c("Weekend x Major Road",
           "Weekend x Minor Road","Peak Traffic x Major Road",
           "Peak Traffic x Minor Road","Daytime x Major Road",
           "Daytime x Minor Road","Weekend","Daytime","Peak Traffic",
           "Minor Road","Major Road"), title = "")

###plotting effect of Road Type and Daytime
#predicted values
road_rk$predicted <- predict(road_rk_best, newdata = road_rk)
###create plot
rk_plot_1 <- ggplot(road_rk, aes(x = road_group, y = predicted, colour = daylight)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = daylight), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Road Group", y = "Distance to Road (m)", colour = "Time of Day")+
  scale_x_discrete(labels = c("Footpath","Minor Road","Major Road"))+
  scale_colour_discrete(labels = c("Day", "Night"))

###plotting effect of traffic
ggplot(road_rk, aes(x = traffic, y = predicted, colour = traffic)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = traffic), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Traffic", y = "Distance to Road (m)")+
  scale_x_discrete(labels = c("Off-Peak","Peak"))+
  theme(legend.position = "none")+
  theme(text = element_text(size = 12))


#-------------------------Distance to Road: OYC---------------------------------


###filter for OYC
road_oyc <- road_distance %>%
  filter(species == "OYC")

###check normality
hist_oyc <- hist(road_oyc$distance)

###mixed effects model
road_oyc_m1 <- lmer(data = road_oyc, distance~is_weekend+road_group+traffic
                    +daylight+road_group:is_weekend+road_group:traffic
                    +road_group:daylight+(1|device_id), 
                    na.action = "na.fail")

###dredge global model
dd_oyc <- dredge(road_oyc_m1)

###extract best model
road_oyc_best <- get.models(dd_oyc, 1)[[1]]

###check normality in residuals
plot(resid(road_oyc_best))

###plot confidence intervals
plot_model(road_oyc_best, vline.color = "black", 
           order.terms = c(4,3,5,1,2,6,7,10,11,8,9), show.values = TRUE, 
           value.offset = .3, axis.labels = c("Weekend x Major Road",
           "Weekend x Minor Road","Peak Traffic x Major Road",
           "Peak Traffic x Minor Road","Daytime x Major Road",
           "Daytime x Minor Road","Weekend","Daytime","Peak Traffic",
           "Minor Road","Major Road"), title = "")

###plotting effect of Road Type and Daytime
#predicted values
road_oyc$predicted <- predict(road_oyc_best, newdata = road_oyc)
###create plot
ggplot(road_oyc, aes(x = road_group, y = predicted, colour = daylight)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = daylight), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Road Group", y = "Distance to Road (m)", colour = "Time of Day")+
  scale_x_discrete(labels = c("Footpath","Minor Road","Major Road"))+
  scale_colour_discrete(labels = c("Day", "Night"))

###plotting effect of weekend
ggplot(road_oyc, aes(x = is_weekend, y = predicted, colour = is_weekend)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = is_weekend), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Road Group", y = "Distance to Road (m)", colour = "Time of Week")+
  scale_x_discrete(labels = c("Weekday","Weekend"))+
  theme(legend.position = "none")



#-------------------------Distance to Road: GOD---------------------------------


###filter for GOD
road_god <- road_distance %>%
  filter(species == "GOD")

###check normality
hist_god <- hist(road_god$distance)

###mixed effects model
road_god_m1 <- lmer(data = road_god, distance~is_weekend+road_group+traffic
                    +daylight+road_group:is_weekend+road_group:traffic
                    +road_group:daylight+(1|device_id), 
                    na.action = "na.fail")

###dredge global model
dd_god <- dredge(road_god_m1)

###extract best model
road_god_best <- get.models(dd_god, 1)[[1]]

###check normality in residuals
plot(resid(road_god_best))

###plot confidence intervals
plot_model(road_god_best, vline.color = "black", 
           order.terms = c(4,3,5,1,2,6,7,10,11,8,9), show.values = TRUE, 
           value.offset = .3, axis.labels = c("Weekend x Major Road",
           "Weekend x Minor Road","Peak Traffic x Major Road",
           "Peak Traffic x Minor Road","Daytime x Major Road",
           "Daytime x Minor Road","Weekend","Daytime","Peak Traffic",
           "Minor Road","Major Road"), title = "")

###plotting effect of Road Type and Traffic
#predicted values
road_god$predicted <- predict(road_god_best, newdata = road_god)
###create plot
ggplot(road_god, aes(x = road_group, y = predicted, colour = traffic)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = traffic), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Road Group", y = "Distance to Road (m)", colour = "Traffic")+
  scale_x_discrete(labels = c("Footpath","Minor Road","Major Road"))+
  scale_colour_discrete(labels = c("Off-Peak", "Peak"))

###plotting effect of Road Type and Daytime
ggplot(road_god, aes(x = road_group, y = predicted, colour = daylight)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = daylight), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Road Group", y = "Distance to Road (m)", colour = "Time of Day")+
  scale_x_discrete(labels = c("Footpath","Minor Road","Major Road"))+
  scale_colour_discrete(labels = c("Day", "Night"))



#-------------------------Distance to Road: CU----------------------------------


###filter for CU
road_cu <- road_distance %>%
  filter(species == "CU")

###check normality
hist_cu <- hist(road_cu$distance)

###mixed effects model
road_cu_m1 <- lmer(data = road_cu, distance~is_weekend+road_group+traffic
                   +daylight+road_group:is_weekend+road_group:traffic
                   +road_group:daylight+(1|device_id), 
                   na.action = "na.fail")


###dredge global model
dd_cu <- dredge(road_cu_m1)

###extract best model
road_cu_best <- get.models(dd_cu, 1)[[1]]

###check normality in residuals
plot(resid(road_cu_best))

###plot confidence intervals
plot_model(road_cu_best, vline.color = "black", 
           order.terms = c(4,3,5,1,2,6,7,10,11,8,9), show.values = TRUE, 
           value.offset = .3, axis.labels = c("Weekend x Major Road",
           "Weekend x Minor Road","Peak Traffic x Major Road",
           "Peak Traffic x Minor Road","Daytime x Major Road",
           "Daytime x Minor Road","Weekend","Daytime","Peak Traffic",
           "Minor Road","Major Road"), title = "")

###plotting effect of Road Type and Traffic
#predicted values
road_cu$predicted <- predict(road_cu_best, newdata = road_cu)
###create plot
ggplot(road_cu, aes(x = road_group, y = predicted, colour = traffic)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = traffic), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Road Group", y = "Distance to Road (m)", colour = "Traffic")+
  scale_x_discrete(labels = c("Footpath","Minor Road","Major Road"))+
  scale_colour_discrete(labels = c("Off-Peak", "Peak"))

###plotting effect of Road Type and Daytime
ggplot(road_cu, aes(x = road_group, y = predicted, colour = daylight)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = daylight), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Road Group", y = "Distance to Road (m)", colour = "Time of Day")+
  scale_x_discrete(labels = c("Footpath","Minor Road","Major Road"))+
  scale_colour_discrete(labels = c("Day", "Night"))

###plotting effect of Road Type and Weekend
ggplot(road_cu, aes(x = road_group, y = predicted, colour = is_weekend)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = is_weekend), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Road Group", y = "Distance to Road (m)", colour = "Time of Week")+
  scale_x_discrete(labels = c("Footpath","Minor Road","Major Road"))+
  scale_colour_discrete(labels = c("Weekday", "Weekend"))


#-------------------------Timer-------------------------------------------------


###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))


#-------------------------Exporting Plots---------------------------------------


# Save the plot with specified parameters
ggsave("rk_plot_1.png", plot = rk_plot_1,
       width = 18, height = 14,  # Legal paper size in inches
       units = "cm", dpi = 300,  # Resolution
       device = "png",
       scale = 1,  # Scale factor for font size
       limitsize = FALSE)

ggsave("rk_plot_1.png", plot = rk_plot_1,
       width = 18, height = 14,  # Legal paper size in inches
       units = "cm", dpi = 300,  # Resolution
       device = "png",
       scale = 1,  # Scale factor for font size
       limitsize = FALSE)

