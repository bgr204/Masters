#-------------------------Importing data----------------------------------------

###reading in urls from github
url5 <- "https://raw.githubusercontent.com/bgr204/Masters/master/disturbance.csv" 
url6 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Step_Length.txt"
url7 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Home_Range.txt"
url8 <- "https://raw.githubusercontent.com/bgr204/Masters/master/disturbance_no_nas.csv"

###read url into csv
disturbance <- read.csv(url(url5), sep = ",")
disturbance2 <- read.csv(url(url8), sep = ",")
step_length <- read.csv(url(url6), sep = "\t")
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
library(glmmTMB)


#-------------------------Formatting--------------------------------------------

###changing column formats
disturbance$human_rate <- as.numeric(disturbance$human_rate)
disturbance$start_time <- chron(times=disturbance$start_time)
disturbance$location_code <- as.factor(disturbance$location_code)
disturbance$precipitation <- as.numeric(factor(disturbance$precipitation, 
                                        levels = c("None", "Spots","Drizzle",
                                        "Light showers", "Showers")))-1
disturbance$precipitation <- ifelse(disturbance$precipitation > 0, 1, 0)
disturbance$wind_speed <- as.numeric(factor(disturbance$wind_speed), levels = 
                                     c("0-10","10-20","20-30","30-40","40-50"))



#-------------------------Disturbance Models------------------------------------

###disturbance model with random effects
#with human_rate
disturbance_m1 <- glmer(data = disturbance, vigilance~wind_speed+
                        precipitation+start_time+human_rate+(1|location_code), 
                        family = "poisson")
#without human_rate
disturbance_m2 <- update(disturbance_m1,~.-human_rate)
#anova test
anova(disturbance_m1, disturbance_m2, test = "Chisq")

summary(disturbance_m1)

# Create ggeffects object
effect_disturbance <- ggpredict(disturbance_m1, terms = c("human_rate", "precipitation", 
                                              "start_time", "wind_speed"))

# Plot marginal effects
plot(effect_disturbance)




#-------------------------Step Length Model-------------------------------------

step_length$species <- as.factor(step_length$species)
step_length$id <- as.factor(step_length$id)
step_length$is_weekend <- as.factor(step_length$is_weekend)
step_length$date <- as.Date(step_length$date)

step_m1 <- lmer(data = step_length, distance~is_weekend*species+(1|id))
step_m2 <- lmer(data = step_length, distance~is_weekend+species+(1|id))
step_m3 <- update(step_m2,~.-is_weekend)
anova(step_m1, step_m2, test = "F")
anova(step_m2, step_m3, test = "F")

summary(step_m1)

# Create ggeffects object
effect_step <- ggpredict(step_m1, terms = c("is_weekend", "species"))

# Plot marginal effects
plot(effect_step)




#-------------------------Home Range Model--------------------------------------

home_range$species <- as.factor(home_range$species)
home_range$id <- as.factor(home_range$id)
home_range$is_weekend <- as.factor(home_range$is_weekend)

home_m1 <- lmer(data = home_range, area~is_weekend*species+(1|id))
home_m2 <- lmer(data = home_range, area~is_weekend+species+(1|id))
home_m3 <- update(home_m2,~.-is_weekend)
anova(home_m1, home_m2, test = "F")
anova(home_m2, home_m3, test = "F")

summary(home_m1)

# Create ggeffects object
effect_home <- ggpredict(home_m1, terms = c("is_weekend", "species"))

# Plot marginal effects
plot(effect_home)


