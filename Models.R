#-------------------------Importing data----------------------------------------

###reading in urls from github
url5 <- "https://raw.githubusercontent.com/bgr204/Masters/master/disturbance.csv" 
url6 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Step_Length.txt"
url7 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Home_Range.txt"
url8 <- "https://raw.githubusercontent.com/bgr204/Masters/master/disturbance_no_nas.csv"
url9 <- "https://raw.githubusercontent.com/bgr204/Masters/master/Distance_to_Road.txt"

###read url into csv
disturbance <- read.csv(url(url5), sep = ",")
disturbance2 <- read.csv(url(url8), sep = ",")
step_length <- read.csv(url(url6), sep = "\t")
home_range <- read.csv(url(url7), sep = "\t")
road_distance <- read.csv(url(url9), sep = "\t")



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



#-------------------------Optimal Lambda Function-------------------------------

find_optimal_lambda <- function(data) {
  
  # Sequence of lambda values to test
  lambda_seq <- seq(-2, 2, by = 0.1)
  
  # Function to compute skewness
  compute_skewness <- function(x) {
    mean((x - mean(x))^3) / (sd(x)^3)
  }
  
  # Container for skewness values
  skewness_values <- numeric(length(lambda_seq))
  
  # Perform Box-Cox transformation for each lambda value
  for (i in seq_along(lambda_seq)) {
    transformed_data <- BoxCox(data, lambda_seq[i])
    skewness_values[i] <- compute_skewness(transformed_data)
  }
  
  # Find the lambda value with the smallest skewness
  optimal_lambda <- lambda_seq[which.min(abs(skewness_values))]
  
  # Apply Box-Cox transformation with optimal lambda
  data <- BoxCox(data, optimal_lambda)

}


#-------------------------Disturbance Models------------------------------------

###disturbance model for vigilance
#without location
vig_m1 <- glm(data = disturbance, vigilance~wind_speed+
                precipitation+start_time+human_rate, 
              family = "poisson")
vig_m2 <- update(vig_m1,~.-human_rate)
anova(vig_m1, vig_m2, test = "Chisq")
summary(vig_m1)
ggplot(disturbance, aes(x = human_rate, y = vigilance))+
  geom_point()+
  geom_smooth(method = "lm")
#with location
vig_m3 <- glmer(data = disturbance, vigilance~wind_speed+
                        precipitation+start_time+human_rate+(1|location_code), 
                        family = "poisson")
vig_m4 <- update(vig_m3,~.-human_rate)
anova(vig_m3, vig_m4, test = "Chisq")
summary(vig_m3)
ggplot(disturbance, aes(x = human_rate, y = vigilance, colour = location_code))+
  geom_point()+
  geom_line()


###disturbance model for flight
#without location
fli_m1 <- glm(data = disturbance, flight~wind_speed+
                precipitation+start_time+human_rate, 
              family = "poisson")
fli_m2 <- update(fli_m1,~.-human_rate)
anova(fli_m1, fli_m2, test = "Chisq")
summary(fli_m1)
ggplot(disturbance, aes(x = human_rate, y = flight))+
  geom_point()+
  geom_smooth(method = "lm")
#with location
fli_m3 <- glmer(data = disturbance, flight~wind_speed+
                  precipitation+start_time+human_rate+(1|location_code), 
                family = "poisson")
fli_m4 <- update(fli_m3,~.-human_rate)
anova(fli_m3, fli_m4, test = "Chisq")
summary(fli_m3)
ggplot(disturbance, aes(x = human_rate, y = flight, colour = location_code))+
  geom_point()+
  geom_line()





#-------------------------Step Length Model-------------------------------------

step_length$species <- as.factor(step_length$species)
step_length$id <- as.factor(step_length$id)
step_length$is_weekend <- as.factor(step_length$is_weekend)


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

step_length$predicted <- predict(step_m2, newdata = step_length)

ggplot(step_length, aes(x = is_weekend, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), geom = "point", shape = 18, size = 3, position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), geom = "line", linetype = "dashed", size = 0.75, position = position_dodge(width = 0.75)) +  # Add lines between medians
  labs(title = "Daily Distance", x = "Weekend", y = "Daily Distance (km)")



#-------------------------Home Range Model--------------------------------------

home_range$species <- as.factor(home_range$species)
home_range$id <- as.factor(home_range$id)
home_range$is_weekend <- as.factor(home_range$is_weekend)
home_range$area <- home_range$area/1000000

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

# Generate predictions
home_range$predicted <- predict(home_m2, newdata = home_range)

# Create plot
ggplot(home_range, aes(x = is_weekend, y = predicted, colour = species)) +
  geom_boxplot() +  # Add actual data points
  stat_summary(fun = median, aes(group = species, color = species), geom = "point", shape = 18, size = 4, position = position_dodge(width = 0.75)) +  # Add median points
  stat_summary(fun = median, aes(group = species, color = species), geom = "line", linetype = "dashed", position = position_dodge(width = 0.75)) +  # Add lines between medians
  labs(title = "Home Range", x = "Weekend", y = "Area of Home Range (km^2)")


#-------------------------Distance to Road Models-------------------------------

#change format of date
road_distance$UTC_date <- as.Date(road_distance$UTC_date)
#add column with weekday
road_distance$day <- weekdays(road_distance$UTC_date)
#creating a column for weekend or not
road_distance$is_weekend <- road_distance$day %in% c("Saturday", "Sunday")

road_distance$species <- as.factor(road_distance$species)
road_distance$device_id <- as.factor(road_distance$device_id)
road_distance$is_weekend <- as.factor(road_distance$is_weekend)
road_distance$road <- as.factor(road_distance$road)
road_distance$log_road <- sqrt(road_distance$distance)

# Sequence of lambda values to test
lambda_seq <- seq(-2, 2, by = 0.1)

# Function to compute skewness
compute_skewness <- function(x) {
  mean((x - mean(x))^3) / (sd(x)^3)
}

# Container for skewness values
skewness_values <- numeric(length(lambda_seq))

# Perform Box-Cox transformation for each lambda value
for (i in seq_along(lambda_seq)) {
  transformed_data <- BoxCox(road_distance$distance, lambda_seq[i])
  skewness_values[i] <- compute_skewness(transformed_data)
}

# Plot skewness values for different lambda values
plot(lambda_seq, skewness_values, type = "l", xlab = "Lambda", ylab = "Skewness")

# Find the lambda value with the smallest skewness
optimal_lambda <- lambda_seq[which.min(abs(skewness_values))]
cat("Optimal lambda:", optimal_lambda, "\n")

# Apply Box-Cox transformation with optimal lambda
road_distance$tf_distance <- BoxCox(road_distance$distance, optimal_lambda)

# Plot histogram of transformed data
hist(road_distance$tf_distance, breaks = 100)

road_m1 <- lmer(data = road_distance, tf_distance~is_weekend*species+road+(1|device_id))
road_m2 <- lmer(data = road_distance, tf_distance~is_weekend+species+road+(1|device_id))
road_m3 <- update(road_m2,~.-is_weekend)
anova()

road_distance$tf_distance <- find_optimal_lambda(road_distance$distance)
