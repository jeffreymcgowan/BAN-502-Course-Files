#Module 4, Trees

rm(list=ls()) #clean env

# Helper packages
library(tidyverse)
library(tidymodels)
library(dplyr)       # for data wrangling
library(ggplot2)     # for awesome plotting

# Modeling packages
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application
library(rpart.plot)  # for plotting decision trees
library(RColorBrewer)
library(rattle)
library(skimr)      #alternative way to view dataset summaries
library(UpSetR)     #visualizing missingness

#####################################################

heart <- read_csv("heart_disease.csv")

#####################################################

skim(heart)

# Then carefully convert the “sex”, “ChestPainType”, “RestingECG”, “ExerciseAngina”, 
# “ST_Slope”, and “HeartDisease” variables to factors. 
# Recode the levels of the “HeartDisease” variable from “0” to “No” and “1” to “Yes”.

heart = heart %>% mutate(Sex = as_factor(Sex)) %>% 
  mutate(ChestPainType = as_factor(ChestPainType)) %>%
  mutate(RestingECG = as_factor(RestingECG)) %>% 
  mutate(ExerciseAngina = as_factor(ExerciseAngina)) %>% 
  mutate(ST_Slope = as_factor(ST_Slope)) %>% 
  mutate(HeartDisease = as_factor(HeartDisease)) %>% 
  mutate(HeartDisease = fct_recode(HeartDisease, "No" = "0", "Yes" = "1" ))
  
# Question 1: Split the data into training and testing sets. 
# Your training set should have 70% of the data. Use a random number (set.seed) of 12345. 
# Stratify your split by the response variable “HeartDisease”.
# How many rows are in the training set?
#------->642

set.seed(12345)
heart_split = initial_split(heart, prop = 0.70, strata = HeartDisease)
train = training(heart_split)
test = testing(heart_split)


# Question 2: Create a classification tree to predict “HeartDisease” in 
# the training set (using all of the other variables as predictors). Plot the tree. 
# You do not need to manually tune the complexity parameter (i.e., it’s OK to allow R to try 
#                                                            different cp values on its own). 
# Do not use k-folds at this point.
# 
# The first split in the tree is a split on which variable? --------> C. STSLOPEUP

heart_recipe = recipe(HeartDisease ~ Sex + ChestPainType + RestingECG + ExerciseAngina + ST_Slope, train)

heart_model = decision_tree() %>% 
  set_engine("rpart", model = TRUE) %>% #don't forget the model = TRUE flag
  set_mode("classification")

heart_wflow = 
  workflow() %>% 
  add_model(heart_model) %>% 
  add_recipe(heart_recipe)

heart_fit = fit(heart_wflow, train)

#look at the tree's fit
heart_fit %>%
  pull_workflow_fit() %>%
  pluck("fit")  

#extract the tree's fit from the fit object
tree = heart_fit %>% 
  pull_workflow_fit() %>% 
  pluck("fit")

#plot the tree
rpart.plot(tree)

#alternative
fancyRpartPlot(tree) 


# Question 3: Examine the complexity parameter (cp) values tried by R.
# 
# Which cp value is optimal (recall that the optimal cp corresponds to the minimized “xerror” value)? 
#   Report your answer to two decimal places.

#Look at the "rpart" complexity parameter "cp".    
heart_fit$fit$fit$fit$cptable #----------->0.01

# Question 4: Use a tuning grid (as we did in the Titanic problem) to allow R to try 
# 25 different values for the complexity parameter (cp). R will select reasonable values. 
# Use 5-fold k-fold cross-validation (don’t forget to set up your folds). 
# Use a seed of 123 when setting up your folds.
# 
# Hint: You can reuse the vast majority of the code that I provided for you. 
# Be careful to change names and you should be “good to go”. Note: This model 
# took about two minutes to run on my computer. Your run time will vary by your 
# computational power :) Plot the relationship between the complexity parameter (cp) 
# and model performance (given by accuracy and by ROC AUC). I have provided code in 
# the lectures that use the “collect_metrics” functions to help you do this.
# 
# From this plot, what is the accuracy of the model (to two decimal places) if a cp 
# value of 0.1 is selected? You will need to “eyeball” this answer. I have included a 
# bit of a tolerance in the answer on Canvas. As long as you are “close” to the correct 
# accuracy, you will see your answer marked as correct.

set.seed(123)
folds = vfold_cv(train, v = 5)

heart_recipe = recipe(HeartDisease ~ Sex + ChestPainType + RestingECG + ExerciseAngina + ST_Slope, train) %>%
  step_dummy(all_nominal(),-all_outcomes())

heart_model = decision_tree(cost_complexity = tune()) %>% 
  set_engine("rpart", model = TRUE) %>% #don't forget the model = TRUE flag
  set_mode("classification")

heart_grid = grid_regular(cost_complexity(),
                         levels = 25) #try 25 sensible values for cp

heart_wflow = 
  workflow() %>% 
  add_model(heart_model) %>% 
  add_recipe(heart_recipe)

heart_res = 
  heart_wflow %>% 
  tune_grid(
    resamples = folds,
    grid = heart_grid
  )

heart_res %>%
  collect_metrics() %>%
  ggplot(aes(cost_complexity, mean)) +
  geom_line(linewidth = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) 

#----------->.7775

# End Question 4 ######################################################

# Question 5: Which cp value (to four decimal places) yields the “optimal” accuracy value?

#-------->0.0000

heart_grid2 = expand.grid(cost_complexity = seq(0.000,0.010,by=0.001))

heart_wflow2 = 
  workflow() %>% 
  add_model(heart_model) %>% 
  add_recipe(heart_recipe)

heart_res2 = 
  heart_wflow2 %>% 
  tune_grid(
    resamples = folds,
    grid = heart_grid2
  )

heart_res2 %>%
  collect_metrics() %>%
  ggplot(aes(cost_complexity, mean)) +
  geom_line(linewidth = 1.5, alpha = 0.6) +
  geom_point(size = 2) +
  facet_wrap(~ .metric, scales = "free", nrow = 2) 







best_tree2 = heart_res2 %>%
  select_best("accuracy")

best_tree2

final_wf = 
  heart_wflow2 %>% 
  finalize_workflow(best_tree)

final_fit = fit(final_wf, train)

tree = final_fit %>% 
  pull_workflow_fit() %>% 
  pluck("fit")

rpart.plot(tree)

fancyRpartPlot(tree, tweak = 1.1) 

final_fit$fit$fit$fit$cptable #----------->0.36

treepred = predict(final_fit, train, type = "class")
head(treepred)
  
confusionMatrix(treepred$.pred_class,train$HeartDisease,positive="Yes") #predictions first then actual  


# Question 6: Plot the tree that corresponds to the cp value from Question 5. Don’t forget to finalize 
# your workflow and generate your final fit before trying to plot.
# How would you classify a patient that is “Male” with an “ST_Slope” that is “Flat”?

rpart.plot(tree)#------------>???

# Question 7: What is the accuracy (on the training set) of the “tree” that you generated in Question 6? 
# Take your time and think about how to determine this value. Report your answer to four decimal places.

#----------->0.8583

# Question 8 What is the sensitivity of your model from Question 6 (on the training set)? 
# Report your answer to four decimal places.

#----------->0.8761

# Question 9 What is the naive accuracy of your model from Question 6 (on the training set)? 
# Report your answer to four decimal places.

#----------->0.5530
 
# Question 10 What is the accuracy of your model from Question 6 on the testing set 
# (to four decimal places)?

treepred = predict(final_fit, test, type = "class")
head(treepred)

confusionMatrix(treepred$.pred_class,test$HeartDisease,positive="Yes")

#----------->0.8406

  
