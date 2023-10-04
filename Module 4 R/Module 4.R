rm(list=ls()) #clean env

# Helper packages
library(tidyverse)
library(dplyr)       # for data wrangling
library(ggplot2)     # for awesome plotting

# Modeling packages
library(rpart)       # direct engine for decision tree application
library(caret)       # meta engine for decision tree application

# Model interpretability packages
library(rpart.plot)  # for plotting decision trees
library(vip)         # for feature importance
library(pdp)         # for feature effects
library(VIM)
library(mice)
library(naniar)     #visualizing missingness
library(skimr)      #alternative way to view dataset summaries
library(UpSetR)     #visualizing missingness

#####################################################

grades <- read_csv("class-grades.csv")

#####################################################
#1
str(grades)
summary(grades)
skim(grades)

#2
gg_miss_case(grades) #x axis is number of missing values in each row (case)
gg_miss_var(grades)
vim_plot = aggr(grades, numbers = TRUE, prop = c(TRUE, FALSE),cex.axis=.7)
vis_miss(grades) #from the naniar package

# Task 1: For which variables is there missing data? ---> Tutorial, MidTerm, TakeHome, and Final
   
# Task 2: Use the VIM package to visualize missingness. Are there any students 
#that are missing multiple pieces of data? ----> Yes,1

   
# Task 3: Use row-wise deletion of missing values to create a new data frame. 
#How many rows remain in this data frame? ----> 89

grades2 <- grades %>% drop_na() 
skim(grades2)

# Task 4: Use column-wise deletion of missing values to create a new data frame 
#(from the original data frame not from the data frame created in Task 3). How 
#many columns remain in this data frame? ---> 2

grades3 <- grades %>% select_if(~ !any(is.na(.)))
skim(grades3)

# Task 5: Which approach (Task 3 or Task 4) seems preferable for this dataset? 
#Briefly discuss your answer. 

#---> Task 3 is much less destructive and retains the most data overall. The dataset us still usable after step 3, while in step 4 it's useless. 

#Task 6 Use the “mice” package to impute the missing data in this dataset. 
#Set “m = 5” and “method =”pmm"" in the imputation. You MUST use a random 
#number seed of 123 for this imputation. What is the mean value for the 
#“Final” variable after imputation? ---> 68.33 

set.seed(123) #sets seed for random number generator
grades_imputed = mice(grades, m=5, method='pmm', printFlag=FALSE)
#m is the number of imputations, 5 is a reasonable value as a default
#pmm is "predictive mean matching" = imputation method for numeric data

summary(grades_imputed)
grades_complete = complete(grades_imputed) 
summary(grades_complete)

