#-------------------------Importing data----------------------------------------

###reading in urls from github
url5 <- "https://raw.githubusercontent.com/bgr204/Masters/master/disturbance.csv" 
url6 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Step_Length.txt"
url7 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Home_Range.txt"

###read url into csv
disturbance <- read.csv(url(url5), sep = ",")
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


#-------------------------Formatting--------------------------------------------

###changing column formats
disturbance$human_rate <- as.numeric(disturbance$human_rate)
disturbance$start_time <- chron(times=disturbance$start_time)
disturbance$location_code <- as.factor(disturbance$location_code)
disturbance$precipitation <- as.numeric(factor(disturbance$precipitation, 
                                        levels = c("None","Spots","Drizzle",
                                        "Light showers", "Showers")))-1
disturbance$wind_speed <- as.numeric(factor(disturbance$wind_speed), levels = 
                                     c("0-10","10-20","20-30","30-40","40-50"))



#-------------------------Disturbance Models------------------------------------

###checking for collinearity in weather data
#creating a matrix with weather data in
weather_matrix <- disturbance[,c("cloud","precipitation","wind_speed")]
#correlation test for collinearity
cor(weather_matrix)

###disturbance model without random effects
#with human_rate
disturbance_m1 <- glm(data = disturbance, vigilance~wind_speed+
                      precipitation+human_rate+start_time+location_code, 
                      family = "poisson")
#without human_rate
disturbance_m2 <- update(disturbance_m1,~.-human_rate)
#anova test
anova(disturbance_m1, disturbance_m2, test = "Chisq")

###disturbance model with random effects
#with human_rate
disturbance_m3 <- glmer(data = disturbance, vigilance~wind_speed+
                        precipitation+start_time+human_rate+(1|location_code), 
                      family = "poisson")
#without human_rate
disturbance_m4 <- update(disturbance_m3,~.-human_rate)
#anova test
anova(disturbance_m3, disturbance_m4, test = "Chisq")

# Create ggeffects object
effect <- ggpredict(step_m1, terms = c("is_weekend", "species"))

# Plot marginal effects
plot(effect)




#-------------------------Step Length Model-------------------------------------

step_length$date <- as.factor(step_length$date)
step_length$species <- as.factor(step_length$species)
step_length$id <- as.factor(step_length$id)
step_length$is_weekend <- as.factor(step_length$is_weekend)

step_m1 <- lmer(data = step_length, distance~is_weekend*species+(1|id))
step_m2 <- update(step_m1,~.-is_weekend)
anova(step_m1, step_m2, test = "F")

summary(step_m1)

# Create ggeffects object
effect <- ggpredict(step_m1, terms = c("is_weekend", "species"))

# Plot marginal effects
plot(effect)




#-------------------------Home Range Model--------------------------------------

home_range$date <- as.factor(step_length$date)
home_range$species <- as.factor(step_length$species)
home_range$id <- as.factor(step_length$id)
home_range$is_weekend <- as.factor(step_length$is_weekend)

home_range$date <- as.Date(home_range$date)

home_m1 <- lmer(data = home_range, area~is_weekend*species+(1|id))
home_m2 <- lmer(data = home_range, area~species+date+(1|id))
anova(home_m1, home_m2, test = "F")

summary(home_m1)

# Create ggeffects object
effect <- ggpredict(home_m1, terms = c("is_weekend", "species"))

# Plot marginal effects
plot(effect)


