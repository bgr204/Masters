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
road_distance$daylight <- factor(road_distance$daylight, levels = c("TRUE","FALSE"))
road_distance$time <- as.POSIXct(road_distance$UTC_datetime, 
                                 format = "%Y-%m-%d %H:%M:%S")
road_distance$time <- format(road_distance$time, format = "%H")
road_distance$traffic <- as.factor(ifelse(road_distance$time %in% 
                                   c(7,8,9,15,16,17,18,19), "peak", "offpeak"))
road_distance$road_group <- ifelse(road_distance$road_group == "motorway", "major_road",road_distance$road_group)
road_distance$road_group <- factor(road_distance$road_group, levels = c("footpath","minor_road","major_road"))


#-------------------------Distance to Road: All---------------------------------


###check normality
hist(road_distance$distance) # looks slightly right-skewed

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
  labs(title = "Weekend", x = "Weekend", y = "Distance to Road (predicted)")

###create plot - road group effect
ggplot(road_distance, aes(x = road_group, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  labs(title = "Road Group", x = "Road Group", y = "Distance to Road (predicted)")

###create plot - traffic effect
ggplot(road_distance, aes(x = traffic, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), 
               geom = "line", linetype = "dashed", size = 0.75, 
               position = position_dodge(width = 0.75)) +  # Add lines
  labs(title = "Traffic", x = "Traffic", y = "Distance to Road (predicted)")


#-------------------------Distance to Road: RK----------------------------------


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
ggplot(road_rk, aes(x = road_group, y = predicted, colour = daylight)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = daylight), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Road Group", y = "Distance to Road (m)", colour = "Daytime")+
  scale_x_discrete(labels = c("Footpath","Minor Road","Major Road"))

###plotting effect of traffic
ggplot(road_rk, aes(x = traffic, y = predicted, colour = traffic)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = traffic), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Road Group", y = "Distance to Road (m)")+
  scale_x_discrete(labels = c("Off-Peak","Peak"))+
  theme(legend.position = "none")


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
  labs(x = "Road Group", y = "Distance to Road (m)", colour = "Daytime")+
  scale_x_discrete(labels = c("Footpath","Minor Road","Major Road"))

###plotting effect of daylight
ggplot(road_god, aes(x = daylight, y = predicted, colour = daylight)) +
  geom_boxplot() +
  stat_summary(fun = median, aes(group = daylight), 
               geom = "point", shape = 18, size = 3, 
               position = position_dodge(width = 0.75))+
  labs(x = "Road Group", y = "Distance to Road (m)", colour = "Daytime")+
  scale_x_discrete(labels = c("Nighttime","Daytime"))+
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


#-------------------------Timer and Alarm---------------------------------------


###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))

