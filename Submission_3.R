# Third Submission
#   Score: 0.88428

# Code for Analzing Bike Sharing Dataset

library(tidyverse)
library(DataExplorer)
library(caret)
library(vroom)
library(gbm)
# For big datasets
library(lubridate)
# Day-Time library

# Read in the data
system.time(bike.train <- read.csv("../Downloads/train.csv"))
system.time(bike.train <- vroom("../Downloads/train.csv"))

bike.test <- vroom("../Downloads/test.csv")

bike <- bind_rows(train = bike.train, test = bike.test, .id = "id")
# Create an ID column!
# bike %>% filter(id == "train")
# Some variables (like "casual") are only in "train," so it wouldn't work with 
#   base R (rbind), and creates tons of NAs. 

# Drop casual and registered
bike <- bike %>% select(-casual, -registered)
names(bike)

# Exploratory Plots
qplot(1:nrow(bike), bike$count, geom="point")
# Time might be a factor (later one, there are more bikes). Guess: The two dips
#   are winter, the peaks are Summer

ggplot(data=bike, aes(x=datetime, y=count, color=as.factor(season))) +
  geom_point()
# Now we can be sure of the pattern (1 is Spring)
ggplot(data=bike, aes(x=datetime, y=count, color=as.factor(month(datetime)))) +
  geom_point()

# Feature Engineering
bike$month <- month(bike$datetime) %>% as.factor()
# Coming up with other variables based on what they give us! 
bike$season <- as.factor(bike$season)
bike$hour <- substr(bike$datetime, 12, 13)
bike$hour <- as.factor(bike$hour)

plot_missing(bike)
plot_correlation(bike, type="continuous")
# The "count" variable is all missing, but that's the one we are the most 
#   interested in

plot_correlation(bike, type="continuous",
                 cor_args=list(use='pairwise.complete.obs'))



# There are two variables that are measuring the exact same thing (confounding)

ggplot(data=bike, aes(x=season, y=count)) +
  geom_boxplot()
# It doesn't make sense for these to be categorical
#   Encoding: make tons of dummies (less stable calculations, a con)

# Dummy Variable Encoding - one-hot encoding

dummyVars(count~season, data=bike, sep = "_") %>% 
  predict(bike) %>% as.data.frame() %>% 
  bind_cols(bike %>% select(-season), .)

# Remember, you need one less category 

# Target Encoding instead? Only do this with categorical! 
#   Creates the average for each of the seasons?

lm(count ~ season, data=bike) %>% predict()
# Not predicting for all
dim(bike)

bike$season <- lm(count ~ season, data=bike) %>% 
  predict(., newdata=bike %>% select(-count))

# Works
length(lm(count ~ season, data=bike) %>% 
         predict(., newdata=bike %>% select(-count)))

table(bike$season)

## Spend more time cleaning
# But, fit some models
# (caret library)

bike.model <- train(form=count~season+atemp+humidity+hour,
                    data=bike %>% filter(id == "train"),
                    method="gbm",
                    tuneLength=5,
                    trControl=trainControl(
                      method="repeatedcv",
                      number=10,
                      repeats=2))

# "ranger" (kind of random forest) or "lm" or "neuralnet":: cool, cool, cool! 
#   (literally, 51 methods!)
# Use just "train" because "test" doesn't have "count"
# tuneLength is the quantity of knobs (how many models???)
# "Train": use data to estimate parameters
# Cross_validation is default of trControl, it separates dataset in two and one 
#   predicts the other

plot(bike.model)

# How to know the best model? Huh?
bike.model$bestTune
predicted <- predict(bike.model, newdata=bike %>% filter(id == "test"))
# It knows automatically which one is the independent variable (count) because 
#   of the past model

# AHHH
# submission <- data.frame(datetime=bike %>% filter(id=="test" %>% pull(datetime)), count=count1)


bikeT <- bike %>% filter(id == "test")

my_submission <- data_frame('datetime' = bikeT$datetime, 'count' = predicted)


my_submission$datetime <- format(my_submission$datetime, usetz=TRUE)
my_submission$datetime <-substring(my_submission$datetime,1,19)

my_submission$count[my_submission$count < 0] <- 1
my_submission$count[my_submission$count > 1000] <- 1000

write.table(my_submission, file = "Bike3.csv", sep = ",",row.names=FALSE, col.names=c("datetime", "count"))

