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
library(parameters)
library(gridExtra)
library(grid)


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
disturbance$estuary <- ifelse(disturbance$location_code %in% c("BRA","BU","SP","KG","RK","SC","PLC","RP","PB"), "BAL",
                              ifelse(disturbance$location_code %in% c("MI","MNF","MCG","CC","MBC","PH","FB"), "MAL", "ROG"))



#-------------------------Disturbance: Vigilance--------------------------------


###check normality
hist_vig <- hist(disturbance$vigilance) #count data

###mixed effects model
vig_m1 <- glm(data = disturbance, vigilance~wind_speed+precipitation+birds+
                  human*estuary, family = "poisson")
vig_m2 <- update(vig_m1,~.-human:estuary)
anova(vig_m1,vig_m2, test = "Chisq")
summary(vig_m1)
performance::performance(vig_m1)

###plot
vig_plot <- ggplot(data = disturbance, aes(x = human, y = vigilance))+
  geom_point() +
  geom_smooth(method = "lm")+
  xlim(0,120)+
  labs(x = "Count of People", y = "Count of Vigilance Behaviour")


#-------------------------Disturbance: Flight-----------------------------------


###check normality
hist_fli <- hist(disturbance$flight) #count data

###mixed effect model
fli_m1 <- glm(data = disturbance, flight~wind_speed+precipitation+birds+
                  human*estuary, family = "poisson")
fli_m2 <- update(fli_m1,~.-human:estuary)
anova(fli_m1,fli_m2, test = "Chisq")
summary(fli_m1)
performance::performance(fli_m1)

###plot
fli_plot <- ggplot(data = disturbance, aes(x = human, y = flight))+
  geom_point() +
  geom_smooth(method = "lm")+
  xlim(0,120)+
  labs(x = "Count of People", y = "Count of Flight Behaviour")


#-------------------------Disturbance: Walkrun----------------------------------


###check normality
hist_wal <- hist(disturbance$walkrun) #count data

###mixed effect model
wal_m1 <- glm(data = disturbance, walkrun~wind_speed+precipitation+birds+
                  human*estuary, family = "poisson")
wal_m2 <- update(wal_m1,~.-human:estuary)
wal_m3 <- update(wal_m2,~.-human)
anova(wal_m1,wal_m2, test = "Chisq")
anova(wal_m2, wal_m3, test = "Chisq")
summary(wal_m2)
performance::performance(wal_m1)

###plot
wal_plot <- ggplot(data = disturbance, aes(x = human, y = walkrun))+
  geom_point() +
  geom_smooth(method = "lm")+
  xlim(0,120)+
  labs(x = "Count of People", y = "Count of Walkrun Behaviour")

#-------------------------Disturbance: Alarm------------------------------------


###check normality
hist_ala <- hist(disturbance$alarm) #count data

#general linear model
ala_m1 <- glm(data = disturbance, alarm~wind_speed+precipitation+birds+
                  human*estuary, family = "poisson")
ala_m2 <- update(ala_m1,~.-human:estuary)
anova(ala_m1,ala_m2, test = "Chisq")
summary(ala_m1)
performance::performance(ala_m1)

###plot
ala_plot <- ggplot(data = disturbance, aes(x = human, y = alarm))+
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
step_length_m4 <- update(step_length_m2,~.-species)
anova(step_length_m1,step_length_m2)
anova(step_length_m2,step_length_m3)
anova(step_length_m2, step_length_m4)
summary(step_length_m2)

###check normality in residuals
plot(resid(step_length_m2))

# Extract parameters
param_step <- parameters(step_length_m2)

# Create data frame for estimates and confidence intervals
estim_step <- tibble(
  estimate = c(param_step$Coefficient[1], param_step$Coefficient[3:5] + param_step$Coefficient[1], 
               param_step$Coefficient[1]+ param_step$Coefficient[2], param_step$Coefficient[3:5] + param_step$Coefficient[1] + param_step$Coefficient[2]),
  ci_low = c(param_step$CI_low[1], param_step$CI_low[3:5] + param_step$CI_low[1],
             param_step$CI_low[1]+ param_step$CI_low[2], param_step$CI_low[3:5] + param_step$CI_low[1] + param_step$CI_low[2]),
  ci_high = c(param_step$CI_high[1], param_step$CI_high[3:5] + param_step$CI_high[1],
              param_step$CI_high[1] + param_step$CI_high[2], param_step$CI_high[3:5] + param_step$CI_high[1] + param_step$CI_high[2]),
  week = rep(c("weekday", "weekend"), each = 4),
  species = rep(c("CU", "GOD", "OYC", "RK"), 2)
)

# Convert ci_low and ci_high to numeric
estim_step <- estim_step %>%
  mutate(ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))


###plot
step_plot <- ggplot(data = estim_step, aes(x = species, y = estimate, colour = week))+
  geom_pointrange(data = estim_step, aes(ymin = ci_low, ymax = ci_high, ),
                  position = position_dodge(width = 0.75), linewidth  = 1, size = 1)+
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
home_range_m4 <- update(home_range_m2,~.-species)
anova(home_range_m1,home_range_m2)
anova(home_range_m2,home_range_m3)
anova(home_range_m2,home_range_m4)
summary(home_range_m2)

###check normality in residuals
plot(resid(home_range_m2))

# Extract parameters
param_home <- parameters(home_range_m2)

# Create data frame for estimates and confidence intervals
estim_home <- tibble(
  estimate = exp(c(param_home$Coefficient[1], param_home$Coefficient[3:5] + param_home$Coefficient[1], 
               param_home$Coefficient[1] + param_home$Coefficient[2], param_home$Coefficient[3:5] + param_home$Coefficient[1] + param_home$Coefficient[2])),
  ci_low = exp(c(param_home$CI_low[1], param_home$CI_low[3:5] + param_home$CI_low[1],
             param_home$CI_low[1] + param_home$CI_low[2], param_home$CI_low[3:5] + param_home$CI_low[1] + param_home$CI_low[2])),
  ci_high = exp(c(param_home$CI_high[1], param_home$CI_high[3:5] + param_home$CI_high[1],
              param_home$CI_high[1] + param_home$CI_high[2], param_home$CI_high[3:5] + param_home$CI_high[1] + param_home$CI_high[2])),
  week = rep(c("weekday", "weekend"), each = 4),
  species = rep(c("CU", "GOD", "OYC", "RK"), 2)
)

# Convert ci_low and ci_high to numeric
estim_home <- estim_home %>%
  mutate(ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))


###plot
home_plot <- ggplot(data = estim_home, aes(x = species, y = estimate, colour = week))+
  geom_pointrange(data = estim_home, aes(ymin = ci_low, ymax = ci_high, ),
                  position = position_dodge(width = 0.75), linewidth  = 1, size = 1)+
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



#-------------------------Distance to Road: RK----------------------------


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
rk_plot_model <- plot_model(road_rk_best, vline.color = "black", 
           order.terms = c(3,4,5,1,2,6,7,10,11,8,9), show.values = TRUE, 
           value.offset = .3, axis.labels = c("Weekend x Major Road",
           "Weekend x Minor Road","Peak Traffic x Major Road",
           "Peak Traffic x Minor Road","Daytime x Major Road",
           "Daytime x Minor Road","Weekend","Daytime","Peak Traffic",
           "Major Road","Minor Road"), title = "")

# Extract parameters
param_road_rk1 <- parameters(road_rk_best)

# Create data frame for estimates and confidence intervals
estim_road_rk1 <- tibble(
  estimate = c(param_road_rk1$Coefficient[1], param_road_rk1$Coefficient[1] + param_road_rk1$Coefficient[4:5], param_road_rk1$Coefficient[1] + param_road_rk1$Coefficient[2], param_road_rk1$Coefficient[1] + param_road_rk1$Coefficient[2] + param_road_rk1$Coefficient[4]+ param_road_rk1$Coefficient[7], param_road_rk1$Coefficient[1] + param_road_rk1$Coefficient[2] + param_road_rk1$Coefficient[5]+ param_road_rk1$Coefficient[8]),
  ci_low = c(param_road_rk1$CI_low[1], param_road_rk1$CI_low[1] + param_road_rk1$CI_low[4:5], param_road_rk1$CI_low[1] + param_road_rk1$CI_low[2], param_road_rk1$CI_low[1] + param_road_rk1$CI_low[2] + param_road_rk1$CI_low[4]+ param_road_rk1$CI_low[7], param_road_rk1$CI_low[1] + param_road_rk1$CI_low[2] + param_road_rk1$CI_low[5]+ param_road_rk1$CI_low[8]),
  ci_high = c(param_road_rk1$CI_high[1], param_road_rk1$CI_high[1] + param_road_rk1$CI_high[4:5], param_road_rk1$CI_high[1] + param_road_rk1$CI_high[2], param_road_rk1$CI_high[1] + param_road_rk1$CI_high[2] + param_road_rk1$CI_high[4]+ param_road_rk1$CI_high[7], param_road_rk1$CI_high[1] + param_road_rk1$CI_high[2] + param_road_rk1$CI_high[5]+ param_road_rk1$CI_high[8]),
  daytime = rep(c("Day", "Night"), each = 3),
  road = factor(rep(c("footpath", "minor_road", "major_road"), 2), levels = c("footpath","minor_road","major_road"))
)

# Convert ci_low and ci_high to numeric
estim_road_rk1 <- estim_road_rk1 %>%
  mutate(ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))

###plot
rk_plot_day <- ggplot(data = estim_road_rk1, aes(x = road, y = estimate, colour = daytime))+
  geom_pointrange(data = estim_road_rk1, aes(ymin = ci_low, ymax = ci_high, ),
                  position = position_dodge(width = 0.75), linewidth  = 1, size = 1)+
  labs(x = "Road Group", y = "Distance to Road (m)", 
       colour = "Time of Day")+
  scale_x_discrete(labels = c("Footpath", "Minor Road", "Major Road"))+
  scale_colour_manual(values = c("orange", "#708090"))

# Create data frame for estimates and confidence intervals
estim_road_rk2 <- tibble(
  estimate = c(param_road_rk1$Coefficient[1], param_road_rk1$Coefficient[1] + param_road_rk1$Coefficient[6]),
  ci_low = c(param_road_rk1$CI_low[1], param_road_rk1$CI_low[1] + param_road_rk1$CI_low[6]),
  ci_high = c(param_road_rk1$CI_high[1], param_road_rk1$CI_high[1] + param_road_rk1$CI_high[6]),
  traffic = c("offpeak", "peak"),
)

# Convert ci_low and ci_high to numeric
estim_road_rk2 <- estim_road_rk2 %>%
  mutate(ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))

###plot
rk_plot_traf <- ggplot(data = estim_road_rk2, aes(x = traffic, y = estimate, colour = traffic))+
  geom_pointrange(data = estim_road_rk2, aes(ymin = ci_low, ymax = ci_high, ),
                  position = position_dodge(width = 0.75), linewidth  = 1, size = 1)+
  labs(x = "Traffic", y = "Distance to Road (m)")+
  scale_x_discrete(labels = c("Off-Peak", "Peak"))+
  theme(legend.position = "none")+
  scale_colour_manual(values = c("chartreuse3",  "purple"))


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
oyc_plot_model <- plot_model(road_oyc_best, vline.color = "black", 
           order.terms = c(3,4,5,1,2,6,7,10,11,8,9), show.values = TRUE, 
           value.offset = .3, axis.labels = c("Weekend x Major Road",
           "Weekend x Minor Road","Peak Traffic x Major Road",
           "Peak Traffic x Minor Road","Daytime x Major Road",
           "Daytime x Minor Road","Weekend","Daytime","Peak Traffic",
           "Major Road","Minor Road"), title = "")

# Extract parameters
param_road_oyc1 <- parameters(road_oyc_best)

# Create data frame for estimates and confidence intervals
estim_road_oyc1 <- tibble(
  estimate = c(param_road_oyc1$Coefficient[1], param_road_oyc1$Coefficient[1] + param_road_oyc1$Coefficient[4:5], param_road_oyc1$Coefficient[1] + param_road_oyc1$Coefficient[2], param_road_oyc1$Coefficient[1] + param_road_oyc1$Coefficient[2] + param_road_oyc1$Coefficient[4]+ param_road_oyc1$Coefficient[7], param_road_oyc1$Coefficient[1] + param_road_oyc1$Coefficient[2] + param_road_oyc1$Coefficient[5]+ param_road_oyc1$Coefficient[8]),
  ci_low = c(param_road_oyc1$CI_low[1], param_road_oyc1$CI_low[1] + param_road_oyc1$CI_low[4:5], param_road_oyc1$CI_low[1] + param_road_oyc1$CI_low[2], param_road_oyc1$CI_low[1] + param_road_oyc1$CI_low[2] + param_road_oyc1$CI_low[4]+ param_road_oyc1$CI_low[7], param_road_oyc1$CI_low[1] + param_road_oyc1$CI_low[2] + param_road_oyc1$CI_low[5]+ param_road_oyc1$CI_low[8]),
  ci_high = c(param_road_oyc1$CI_high[1], param_road_oyc1$CI_high[1] + param_road_oyc1$CI_high[4:5], param_road_oyc1$CI_high[1] + param_road_oyc1$CI_high[2], param_road_oyc1$CI_high[1] + param_road_oyc1$CI_high[2] + param_road_oyc1$CI_high[4]+ param_road_oyc1$CI_high[7], param_road_oyc1$CI_high[1] + param_road_oyc1$CI_high[2] + param_road_oyc1$CI_high[5]+ param_road_oyc1$CI_high[8]),
  daytime = rep(c("Day", "Night"), each = 3),
  road = factor(rep(c("footpath", "minor_road", "major_road"), 2), levels = c("footpath","minor_road","major_road"))
)

# Convert ci_low and ci_high to numeric
estim_road_oyc1 <- estim_road_oyc1 %>%
  mutate(ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))

###plot
oyc_plot_day <- ggplot(data = estim_road_oyc1, aes(x = road, y = estimate, colour = daytime))+
  geom_pointrange(data = estim_road_oyc1, aes(ymin = ci_low, ymax = ci_high, ),
                  position = position_dodge(width = 0.75), linewidth  = 1, size = 1)+
  labs(x = "Road Group", y = "Distance to Road (m)", 
       colour = "Time of Day")+
  scale_x_discrete(labels = c("Footpath", "Minor Road", "Major Road"))+
  scale_colour_manual(values = c("orange", "#708090"))

# Create data frame for estimates and confidence intervals
estim_road_oyc2 <- tibble(
  estimate = c(param_road_oyc1$Coefficient[1], param_road_oyc1$Coefficient[1] + param_road_oyc1$Coefficient[3]),
  ci_low = c(param_road_oyc1$CI_low[1], param_road_oyc1$CI_low[1] + param_road_oyc1$CI_low[3]),
  ci_high = c(param_road_oyc1$CI_high[1], param_road_oyc1$CI_high[1] + param_road_oyc1$CI_high[3]),
  weekend = c("weekday", "weekend"),
)

# Convert ci_low and ci_high to numeric
estim_road_oyc2 <- estim_road_oyc2 %>%
  mutate(ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))

###plot
oyc_plot_week <- ggplot(data = estim_road_oyc2, aes(x = weekend, y = estimate, colour = weekend))+
  geom_pointrange(data = estim_road_oyc2, aes(ymin = ci_low, ymax = ci_high, ),
                  position = position_dodge(width = 0.75), linewidth  = 1, size = 1)+
  labs(x = "Time of Week", y = "Distance to Road (m)")+
  scale_x_discrete(labels = c("Weekday", "Weekend"))+
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
god_plot_model <- plot_model(road_god_best, vline.color = "black", 
           order.terms = c(3,4,5,1,2,6,7,10,11,8,9), show.values = TRUE, 
           value.offset = .3, axis.labels = c("Weekend x Major Road",
           "Weekend x Minor Road","Peak Traffic x Major Road",
           "Peak Traffic x Minor Road","Daytime x Major Road",
           "Daytime x Minor Road","Weekend","Daytime","Peak Traffic",
           "Major Road","Minor Road"), title = "")

# Extract parameters
param_road_god1 <- parameters(road_god_best)

# Create data frame for estimates and confidence intervals
estim_road_god1 <- tibble(
  estimate = c(param_road_god1$Coefficient[1], param_road_god1$Coefficient[1] + param_road_god1$Coefficient[4:5], param_road_god1$Coefficient[1] + param_road_god1$Coefficient[6], param_road_god1$Coefficient[1] + param_road_god1$Coefficient[6] + param_road_god1$Coefficient[4]+ param_road_god1$Coefficient[11], param_road_god1$Coefficient[1] + param_road_god1$Coefficient[6] + param_road_god1$Coefficient[5]+ param_road_god1$Coefficient[12]),
  ci_low = c(param_road_god1$CI_low[1], param_road_god1$CI_low[1] + param_road_god1$CI_low[4:5], param_road_god1$CI_low[1] + param_road_god1$CI_low[6], param_road_god1$CI_low[1] + param_road_god1$CI_low[6] + param_road_god1$CI_low[4]+ param_road_god1$CI_low[11], param_road_god1$CI_low[1] + param_road_god1$CI_low[6] + param_road_god1$CI_low[5]+ param_road_god1$CI_low[12]),
  ci_high = c(param_road_god1$CI_high[1], param_road_god1$CI_high[1] + param_road_god1$CI_high[4:5], param_road_god1$CI_high[1] + param_road_god1$CI_high[6], param_road_god1$CI_high[1] + param_road_god1$CI_high[6] + param_road_god1$CI_high[4]+ param_road_god1$CI_high[11], param_road_god1$CI_high[1] + param_road_god1$CI_high[6] + param_road_god1$CI_high[5]+ param_road_god1$CI_high[12]),
  traffic = rep(c("Off-peak", "Peak"), each = 3),
  road = factor(rep(c("footpath", "minor_road", "major_road"), 2), levels = c("footpath","minor_road","major_road"))
)

# Convert ci_low and ci_high to numeric
estim_road_god1 <- estim_road_god1 %>%
  mutate(ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))

###plot
god_plot_traf <- ggplot(data = estim_road_god1, aes(x = road, y = estimate, colour = traffic))+
  geom_pointrange(data = estim_road_god1, aes(ymin = ci_low, ymax = ci_high, ),
                  position = position_dodge(width = 0.75), linewidth  = 1, size = 1)+
  labs(x = "Road Group", y = "Distance to Road (m)", 
       colour = "Traffic")+
  scale_x_discrete(labels = c("Footpath", "Minor Road", "Major Road"))+
  scale_colour_manual(values = c("chartreuse3",  "purple"))

# Create data frame for estimates and confidence intervals
estim_road_god2 <- tibble(
  estimate = c(param_road_god1$Coefficient[1], param_road_god1$Coefficient[1] + param_road_god1$Coefficient[4:5], param_road_god1$Coefficient[1] + param_road_god1$Coefficient[2], param_road_god1$Coefficient[1] + param_road_god1$Coefficient[2] + param_road_god1$Coefficient[4]+ param_road_god1$Coefficient[7], param_road_god1$Coefficient[1] + param_road_god1$Coefficient[2] + param_road_god1$Coefficient[5]+ param_road_god1$Coefficient[8]),
  ci_low = c(param_road_god1$CI_low[1], param_road_god1$CI_low[1] + param_road_god1$CI_low[4:5], param_road_god1$CI_low[1] + param_road_god1$CI_low[2], param_road_god1$CI_low[1] + param_road_god1$CI_low[2] + param_road_god1$CI_low[4]+ param_road_god1$CI_low[7], param_road_god1$CI_low[1] + param_road_god1$CI_low[2] + param_road_god1$CI_low[5]+ param_road_god1$CI_low[8]),
  ci_high = c(param_road_god1$CI_high[1], param_road_god1$CI_high[1] + param_road_god1$CI_high[4:5], param_road_god1$CI_high[1] + param_road_god1$CI_high[2], param_road_god1$CI_high[1] + param_road_god1$CI_high[2] + param_road_god1$CI_high[4]+ param_road_god1$CI_high[7], param_road_god1$CI_high[1] + param_road_god1$CI_high[2] + param_road_god1$CI_high[5]+ param_road_god1$CI_high[8]),
  daytime = rep(c("Day", "Night"), each = 3),
  road = factor(rep(c("footpath", "minor_road", "major_road"), 2), levels = c("footpath","minor_road","major_road"))
)

# Convert ci_low and ci_high to numeric
estim_road_god2 <- estim_road_god2 %>%
  mutate(ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))

###plot
god_plot_day <- ggplot(data = estim_road_god2, aes(x = road, y = estimate, colour = daytime))+
  geom_pointrange(data = estim_road_god2, aes(ymin = ci_low, ymax = ci_high, ),
                  position = position_dodge(width = 0.75), linewidth  = 1, size = 1)+
  labs(x = "Road Group", y = "Distance to Road (m)", 
       colour = "Time of Day")+
  scale_x_discrete(labels = c("Footpath", "Minor Road", "Major Road"))+
  scale_colour_manual(values = c("orange", "#708090"))


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
cu_plot_model <- plot_model(road_cu_best, vline.color = "black", 
           order.terms = c(3,4,5,1,2,6,7,10,11,8,9), show.values = TRUE, 
           value.offset = .3, axis.labels = c("Weekend x Major Road",
           "Weekend x Minor Road","Peak Traffic x Major Road",
           "Peak Traffic x Minor Road","Daytime x Major Road",
           "Daytime x Minor Road","Weekend","Daytime","Peak Traffic",
           "Major Road","Minor Road"), title = "")

# Extract parameters
param_road_cu1 <- parameters(road_cu_best)

# Create data frame for estimates and confidence intervals
estim_road_cu1 <- tibble(
  estimate = c(param_road_cu1$Coefficient[1], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[4:5], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[6], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[6] + param_road_cu1$Coefficient[4]+ param_road_cu1$Coefficient[11], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[6] + param_road_cu1$Coefficient[5]+ param_road_cu1$Coefficient[12]),
  ci_low = c(param_road_cu1$CI_low[1], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[4:5], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[6], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[6] + param_road_cu1$CI_low[4]+ param_road_cu1$CI_low[11], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[6] + param_road_cu1$CI_low[5]+ param_road_cu1$CI_low[12]),
  ci_high = c(param_road_cu1$CI_high[1], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[4:5], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[6], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[6] + param_road_cu1$CI_high[4]+ param_road_cu1$CI_high[11], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[6] + param_road_cu1$CI_high[5]+ param_road_cu1$CI_high[12]),
  traffic = rep(c("Off-peak", "Peak"), each = 3),
  road = factor(rep(c("footpath", "minor_road", "major_road"), 2), levels = c("footpath","minor_road","major_road"))
)

# Convert ci_low and ci_high to numeric
estim_road_cu1 <- estim_road_cu1 %>%
  mutate(ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))

###plot
cu_plot_traf <- ggplot(data = estim_road_cu1, aes(x = road, y = estimate, colour = traffic))+
  geom_pointrange(data = estim_road_cu1, aes(ymin = ci_low, ymax = ci_high, ),
                  position = position_dodge(width = 0.75), linewidth  = 1, size = 1)+
  labs(x = "Road Group", y = "Distance to Road (m)", 
       colour = "Traffic")+
  scale_x_discrete(labels = c("Footpath", "Minor Road", "Major Road"))+
  scale_colour_manual(values = c("chartreuse3",  "purple"))

# Create data frame for estimates and confidence intervals
estim_road_cu2 <- tibble(
  estimate = c(param_road_cu1$Coefficient[1], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[4:5], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[2], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[2] + param_road_cu1$Coefficient[4]+ param_road_cu1$Coefficient[7], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[2] + param_road_cu1$Coefficient[5]+ param_road_cu1$Coefficient[8]),
  ci_low = c(param_road_cu1$CI_low[1], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[4:5], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[2], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[2] + param_road_cu1$CI_low[4]+ param_road_cu1$CI_low[7], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[2] + param_road_cu1$CI_low[5]+ param_road_cu1$CI_low[8]),
  ci_high = c(param_road_cu1$CI_high[1], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[4:5], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[2], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[2] + param_road_cu1$CI_high[4]+ param_road_cu1$CI_high[7], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[2] + param_road_cu1$CI_high[5]+ param_road_cu1$CI_high[8]),
  daytime = rep(c("Day", "Night"), each = 3),
  road = factor(rep(c("footpath", "minor_road", "major_road"), 2), levels = c("footpath","minor_road","major_road"))
)

# Convert ci_low and ci_high to numeric
estim_road_cu2 <- estim_road_cu2 %>%
  mutate(ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))

###plot
cu_plot_day <- ggplot(data = estim_road_cu2, aes(x = road, y = estimate, colour = daytime))+
  geom_pointrange(data = estim_road_cu2, aes(ymin = ci_low, ymax = ci_high, ),
                  position = position_dodge(width = 0.75), linewidth  = 1, size = 1)+
  labs(x = "Road Group", y = "Distance to Road (m)", 
       colour = "Time of Day")+
  scale_x_discrete(labels = c("Footpath", "Minor Road", "Major Road"))+
  scale_colour_manual(values = c("orange", "#708090"))

# Create data frame for estimates and confidence intervals
estim_road_cu3 <- tibble(
  estimate = c(param_road_cu1$Coefficient[1], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[4:5], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[3], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[3] + param_road_cu1$Coefficient[4]+ param_road_cu1$Coefficient[9], param_road_cu1$Coefficient[1] + param_road_cu1$Coefficient[3] + param_road_cu1$Coefficient[5]+ param_road_cu1$Coefficient[10]),
  ci_low = c(param_road_cu1$CI_low[1], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[4:5], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[3], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[3] + param_road_cu1$CI_low[4]+ param_road_cu1$CI_low[9], param_road_cu1$CI_low[1] + param_road_cu1$CI_low[3] + param_road_cu1$CI_low[5]+ param_road_cu1$CI_low[10]),
  ci_high = c(param_road_cu1$CI_high[1], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[4:5], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[3], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[3] + param_road_cu1$CI_high[4]+ param_road_cu1$CI_high[9], param_road_cu1$CI_high[1] + param_road_cu1$CI_high[3] + param_road_cu1$CI_high[5]+ param_road_cu1$CI_high[10]),
  weekend = rep(c("weekday", "weekend"), each = 3),
  road = factor(rep(c("footpath", "minor_road", "major_road"), 2), levels = c("footpath","minor_road","major_road"))
)

# Convert ci_low and ci_high to numeric
estim_road_cu3 <- estim_road_cu3 %>%
  mutate(ci_low = as.numeric(ci_low),
         ci_high = as.numeric(ci_high))

###plot
cu_plot_week <- ggplot(data = estim_road_cu3, aes(x = road, y = estimate, colour = weekend))+
  geom_pointrange(data = estim_road_cu3, aes(ymin = ci_low, ymax = ci_high, ),
                  position = position_dodge(width = 0.75), linewidth  = 1, size = 1)+
  labs(x = "Road Group", y = "Distance to Road (m)", 
       colour = "Time of Week")+
  scale_colour_discrete(labels = c("Weekday", "Weekend"))+
  scale_x_discrete(labels = c("Footpath", "Minor Road", "Major Road"))


#-------------------------Timer-------------------------------------------------


###timer
end.time <- Sys.time()
print(round(end.time-start.time,2))


#-------------------------Exporting Plots---------------------------------------


###step length plot
ggsave("step_plot.png", plot = step_plot,
       width = 17.5, height = 12,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)

###home range plot
ggsave("home_plot.png", plot = home_plot,
       width = 17.5, height = 12,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)

###distance to road plots: RK
ggsave("rk_plot_model.png", plot = rk_plot_model,
       width = 17.5, height = 12,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)
ggsave("rk_plot_day.png", plot = rk_plot_day,
       width = 11, height = 8,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)
ggsave("rk_plot_traf.png", plot = rk_plot_traf,
       width = 5, height = 8,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)

###distance to road plots: OYC
ggsave("oyc_plot_model.png", plot = oyc_plot_model,
       width = 17.5, height = 12,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)
ggsave("oyc_plot_week.png", plot = oyc_plot_week,
       width = 5, height = 8,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)
ggsave("oyc_plot_day.png", plot = oyc_plot_day,
       width = 11, height = 8,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)


###distance to road plots: GOD
ggsave("god_plot_model.png", plot = god_plot_model,
       width = 17.5, height = 12,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)
ggsave("god_plot_day.png", plot = god_plot_day,
       width = 17.5, height = 8,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)
ggsave("god_plot_traf.png", plot = god_plot_traf,
       width = 17.5, height = 8,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)

###distance to road plots: CU
ggsave("cu_plot_model.png", plot = cu_plot_model,
       width = 17.5, height = 12,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)
ggsave("cu_plot_day.png", plot = cu_plot_day,
       width = 17.5, height = 8,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)
ggsave("cu_plot_traf.png", plot = cu_plot_traf,
       width = 17.5, height = 8,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)
ggsave("cu_plot_week.png", plot = cu_plot_week,
       width = 17.5, height = 8,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)

ggsave("dist_plot.png", plot = ,
       width = 17.5, height = 17.5,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)

plots_grob <- arrangeGrob(
  vig_plot, ala_plot, wal_plot, fli_plot, 
  ncol = 2, nrow = 2
)

# Adding text and circles directly onto the grob using 'grobTree' for better control
final_grob <- grobTree(
  plots_grob,
  grid.text("A", x = 0.45, y = 0.95, gp = gpar(col = "red", fontsize = 15)), # Top-left
  grid.text("B", x = 0.95, y = 0.95, gp = gpar(col = "red", fontsize = 15)), # Top-right
  grid.text("C", x = 0.45, y = 0.45, gp = gpar(col = "red", fontsize = 15)), # Bottom-left
  grid.text("D", x = 0.95, y = 0.45, gp = gpar(col = "red", fontsize = 15)), # Bottom-right
  grid.circle(x = 0.45, y = 0.95, r = 0.025, gp = gpar(fill = "transparent", col = "red", lwd = 1.5)),
  grid.circle(x = 0.95, y = 0.95, r = 0.025, gp = gpar(fill = "transparent", col = "red", lwd = 1.5)),
  grid.circle(x = 0.45, y = 0.45, r = 0.025, gp = gpar(fill = "transparent", col = "red", lwd = 1.5)),
  grid.circle(x = 0.95, y = 0.45, r = 0.025, gp = gpar(fill = "transparent", col = "red", lwd = 1.5))
)

# To display the plot
grid.draw(final_grob)

# To save the plot to a file
ggsave("dist_plot.png", plot = final_grob,
       width = 17.5, height = 17.5,  
       units = "cm", dpi = 300, 
       device = "png",
       scale = 1, 
       limitsize = FALSE)

