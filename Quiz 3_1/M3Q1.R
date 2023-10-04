#Module 3, Quiz 1

rm(list=ls()) #clean env
setwd("~/Documents/UNCW/Fall 2023/BAN 502 Predictive Analytics/Module 3/Quiz 1")

library(tidyverse)
library(tidymodels)
library(lubridate)

bike <- read.csv("bike_cleaned.csv")
bike = bike %>% mutate(dteday = mdy(dteday))
bike = bike %>% mutate_if(is.character, as_factor)
bike = bike %>% mutate(hr = as_factor(hr))

# Question 1: Split the data into training and testing sets. 
# Your training set should have 70% of the data. Use a random number (set.seed) of 1234. 
# Your split should be stratified by the “count” variable

set.seed(1234)
bike_split = initial_split(bike, prop = 0.70, strata = count)
train = training(bike_split)
test = testing(bike_split)

# How many rows of data are in the training set? --->12,163


# Question 2 Stratifying the split by the “count” variable serves what purpose?
# B. Stratifying by “count” ensures that “count” is similarly represented in both the training and testing sets


# Question 3: Build a linear regression model (using the training set) to 
#predict “count” using the variables “season”, “mnth”, “hr”, “holiday”, and “weekday”, “temp”, and “weathersit”.

# What is the adjusted R-squared value (to four digits) of the resulting model? --->0.6209
   
bike_recipe = recipe(count ~ season + mnth + hr + holiday + weekday + temp + weathersit, train)

lm_model = 
  linear_reg() %>% 
  set_engine("lm") 

lm_wflow = 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(bike_recipe)

lm_fit = fit(lm_wflow, train)
summary(lm_fit$fit$fit$fit)
#confint(lm_fit$fit$fit$fit)

# Question 4: Use the predict functions to make predictions (using your model from Question 3) on 
#the training set. Hint: Be sure to store the predictions in an object, 
#perhaps named “predict_train” or similar. Develop a histogram of the predictions 
#(Hint: The predictions are likely stored in a variable called “.pred” in your predictions object).

predict_train <- lm_fit %>% predict(train) %>% bind_cols(train) #%>% metrics(truth = count, estimate = .pred)
predict_train_2 <- lm_fit %>% predict(train) %>% bind_cols(train) %>% metrics(truth = count, estimate = .pred)



p1 <- ggplot(predict_train, aes(x=.pred)) + 
  geom_histogram( binwidth=10, fill="#69b3a2", color="#e9ecef", alpha=0.9) +
  theme_bw()
p1

mean(predict_train$.pred, na.rm=TRUE) #189.47
max(predict_train$.pred, na.rm=TRUE) #594.065

# Select the statements below that are likely true about the distribution of predictions?
  # C. Some predictions for the number of rides in an hour are negative



# Question 5: Determine the performance of your model on the testing set.
# 
# What is the R-squared value (to four decimal places) of your model on the testing set? 
#REMINDER: DO NOT build a model on the testing set. Use your model that was developed on the training set.

#---> 0.632

lm_wflow_test = 
  workflow() %>% 
  add_model(lm_model) %>% 
  add_recipe(bike_recipe)

lm_fit_test = fit(lm_wflow, test)
summary(lm_fit_test$fit$fit$fit)
