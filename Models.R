###importing data
urlfile3 <- "https://raw.githubusercontent.com/bgr204/Masters/master/disturbance.csv" 
urlfile4 <- "https://raw.githubusercontent.com/bgr204/Masters/master/disturbance_no_nas.csv"
disturbance <- read.csv(url(urlfile3), sep = ",")
disturbance_no_nas <- read.csv(url(urlfile4), sep = ",")

###library
library(chron)
library(effects)
library(MuMIn)

###changing column formats
disturbance$human_rate <- as.numeric(disturbance$human_rate)
disturbance_no_nas$human_rate <- as.numeric(disturbance_no_nas$human_rate)
disturbance_no_nas$start_time <- chron(times=disturbance_no_nas$start_time)
disturbance_no_nas$location_code <- as.factor(disturbance_no_nas$location_code)

###very simple models
vig_1 <- glm(vigilance~human_rate, data = disturbance, family = poisson)
vig_2 <- update(vig_1,~.-human_rate, family = poisson)
anova(vig_1, vig_2, test = "Chisq")

walk_1 <- glm(walkrun~human_rate, data = disturbance, family = poisson)
walk_2 <- update(walk_1,~.-human_rate, family = poisson)
anova(walk_1, walk_2, test = "Chisq")

flight_1 <- glm(flight~human_rate, data = disturbance, family = poisson)
flight_2 <- update(flight_1,~.-human_rate, family = poisson)
anova(flight_1, flight_2, test = "Chisq")

alarm_1 <- glm(alarm~human_rate, data = disturbance, family = poisson)
alarm_2 <- update(alarm_1,~.-human_rate, family = poisson)
anova(alarm_1, alarm_2, test = "Chisq")

###plotting effects of simple models
plot(allEffects(vig_1))
plot(allEffects(walk_1))
plot(allEffects(flight_1))
plot(allEffects(alarm_1))

###global model
vig_global <- glm(vigilance~human_rate+tide_state+start_time+location_code+wind+cloud,
                  family = poisson, na.action = "na.fail", data = disturbance_no_nas)

###dredge
vig_dredge <- dredge(global.model= vig_global)
vig_top <- get.models(vig_dredge, subset = 1)[[1]]
summary(vig_top)
