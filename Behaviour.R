setwd("C:/Users/bgroo/Desktop/Dissertation/Data")
getwd()
library(chron)

disturbance <- read.csv("disturbance.csv")
disturbance$human_rate <- as.numeric(disturbance$human_rate)
disturbance_no_nas <- read.csv("disturbance_no_nas.csv")
disturbance_no_nas$human_rate <- as.numeric(disturbance_no_nas$human_rate)
disturbance_no_nas$start_time <- chron(times=disturbance_no_nas$start_time)



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

library(effects)
plot(allEffects(vig_1))
plot(allEffects(walk_1))
plot(allEffects(flight_1))
plot(allEffects(alarm_1))


vig_global <- glm(vigilance~human_rate+tide_state+start_time,
                  family = poisson, na.action = "na.fail", data = disturbance_no_nas)

library(MuMIn)
vig_dredge <- dredge(global.model= vig_global)
vig_top <- get.models(vig_dredge, subset = 1)[[1]]
summary(vig_top)