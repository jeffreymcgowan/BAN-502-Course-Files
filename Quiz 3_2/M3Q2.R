#Module 3, Quiz 2

rm(list=ls()) #clean env
setwd("~/Documents/UNCW/Fall 2023/BAN 502 Predictive Analytics/Module 3/Quiz 2")

library(tidyverse)
library(tidymodels)
library(lubridate)
library(e1071)
library(ROCR)

#####################################################

#bring in data
parole <- read_csv("parole.csv")

#formatting fields 

parole_1 <- parole




parole_1 <- parole_1 %>% mutate(male = as_factor(male))
parole_1 <- parole_1 %>% mutate(male = fct_recode(male, "male" = "1", "female" = "0")) 
  
parole_1 <- parole_1 %>% mutate(race = as_factor(race))
parole_1 <- parole_1 %>% mutate(race = fct_recode(race, "white" = "1", "other" = "2")) 

parole_1 <- parole_1 %>% mutate(multiple.offenses = as_factor(multiple.offenses))
parole_1 <- parole_1 %>% mutate(multiple.offenses = fct_recode(multiple.offenses, 
                                                                "multiple" = "1", 
                                                                "other" = "0"))

parole_1 <- parole_1 %>% mutate(state = as_factor(state))
parole_1 <- parole_1 %>% mutate(state = fct_recode(state, 
                                "other" = "1", 
                                "KY" = "2", 
                                "LA" = "3",
                                "VA" = "4"))

parole_1 <- parole_1 %>% mutate(crime = as_factor(crime))
parole_1 <- parole_1 %>% mutate(crime = fct_recode(crime, 
                                                   "other" = "1", 
                                                   "larceny" = "2", 
                                                   "drug" = "3", 
                                                   "driving" = "4"))

parole_1 <- parole_1 %>% mutate(violator = as_factor(violator))
parole_1 <- parole_1 %>% mutate(violator = fct_recode(violator, 
                                                   "yes" = "1", 
                                                   "no" = "0"))

# parole_1$crime <- as.factor(parole_1$crime)
# parole_1$violator <- as.factor(parole_1$violator)
# parole_1$multiple.offenses <- as.factor(parole_1$multiple.offenses)
# parole_1$state <- as.factor(parole_1$state)

# parole_1$male <- recode_factor(parole_1$male, 
#                                "1" = "male",
#                                "0" = "female")
  
# parole_1$race <- recode_factor(parole_1$race, 
#                                "1" = "white",
#                                "2" = "other")

# parole_1$multiple.offenses <- recode_factor(parole_1$multiple.offenses,
#                                 "1" = "multiple", 
#                                 "0" = "other")

# parole_1$violator <- recode_factor(parole_1$violator,
#                                 "1" = "yes", 
#                                 "0" = "no")

# parole_1$state <- recode_factor(parole_1$state, 
#                                 "1" = "other", "2" = "KY", "3" = "LA", "4" = "VA")

# parole_1$crime <- recode_factor(parole_1$crime,
#                                 "1" = "other", "2" = "larceny", "3" = "drug", "4" = "driving")
  
#####################################################

nrow(parole_1[parole_1$violator == 'yes', ])
#Question 1 --> 78


# Question 2: Split the data into training and testing sets. 
# Your training set should have 70% of the data. Use a random number (set.seed) of 12345. 
# Be sure that the split is stratified by “violator”.
# 
# Before proceeding, let’s take a moment to talk about the ordering of the levels 
# (categories) in the response variable. The command below shows us the levels of 
# the response variable. We should expect them to be “No” and then “Yes” (in that order).

set.seed(12345)
parole_split = initial_split(parole_1, prop = 0.70, strata = violator)
train = training(parole_split)
test = testing(parole_split)

train = train %>% 
  mutate(violator = fct_relevel(violator, c("no","yes"))) 

################################################################################

#Q3 <- false

q3 <- train
q3$cnt <- ifelse(q3$violator == 'yes', 1,0)

q3 <- q3 %>% 
  select("male", "violator", "cnt") %>% 
  rename(gender = "male") %>% 
  group_by(gender) %>%
  summarise(
    
    total = n(),
    vltd = sum(cnt),
    prcnt = vltd / total
  )



################################################################################

#Q4 <- True/False: The violation rate is considerably higher in Louisiana than in the other states.

q4 <- train
q4$cnt <- ifelse(q4$violator == 'yes', 1,0)

q4 <- q4 %>% 
  select("state", "violator", "cnt") %>% 
  group_by(state) %>%
  summarise(
    
    total = n(),
    vltd = sum(cnt),
    prcnt = vltd / total
  )

#----> TRUE

################################################################################

#Q5 <- True/False: The violation rate appears slightly higher among 
#parolees with shorter “max_sentence” values.

#---> TRUE

q5 <- train
q5$cnt <- ifelse(q5$violator == 'yes', 1,0)

q5 <- q5 %>% 
  select("max.sentence", "violator", "cnt") %>% 
  group_by(max.sentence) %>%
  summarise(
    
    total = n(),
    vltd = sum(cnt),
    prcnt = vltd / total
  )

#ggplot(q5, aes(x=prcnt)) + geom_histogram()

################################################################################

# Question 6: Create a logistic regression model using the “state” variable to predict “violator”. 
# Which state is the base level in the model summary?
#   
# D. Other

state_model = 
  logistic_reg(mode = "classification") %>% #note the use of logistic_reg and mode = "classification"
  set_engine("glm") #standard logistic regression engine is glm

state_recipe = recipe(violator ~ state, train)

logreg_wf_state = workflow() %>%
  add_recipe(state_recipe) %>% 
  add_model(state_model)

state_fit = fit(logreg_wf_state, train)

summary(state_fit$fit$fit$fit)

################################################################################

# Question 7 To two decimal places, what is the AIC of the model with “state” to predict “violator”? 

#-->278.95

################################################################################

# Question 8 Create a logistic regression model using the training set to predict “violator” 
# using the variables: “state”, “multiple.offenses”, and “race”.

v_model = 
  logistic_reg(mode = "classification") %>% #note the use of logistic_reg and mode = "classification"
  set_engine("glm") #standard logistic regression engine is glm

v_recipe = recipe(violator ~ state + multiple.offenses + race, train)

logreg_wf_v = workflow() %>%
  add_recipe(v_recipe) %>% 
  add_model(v_model)

v_fit = fit(logreg_wf_v, train)

summary(v_fit$fit$fit$fit)

# Which variables are significant in the resulting model (select all that are significant)? 
# A. state
# B. multiple.offenses
# C. race




################################################################################

# Question 9: Use your model from Question 8 to determine the probability (to two decimal places) 
#that the following parolee will violate parole: 
#The parolee is in Louisiana, has multiple offenses, and is white. 

newdata = data.frame(state = "LA", multiple.offenses = "multiple", race = "white")

predictions = predict(v_fit, newdata, type="prob") #develop predicted probabilities
head(predictions)

# .pred_completed .pred_violated
#      0.669          0.331

################################################################################

#Question 10: Continuing to use your model from Question 8, develop an ROC curve and determine 
# the probability threshold that best balances specificity and sensitivity (on the training set). Be sure to be careful with the predict function syntax.
# What is the value of this threshold (to four decimal places)?

a_predictions = predict(v_fit, train, type="prob")[,2] #develop predicted probabilities
b_predictions = predict(v_fit, train, type="prob")[2]

ROCRpred = prediction(b_predictions, train$violator) 

###You shouldn't need to ever change the next two lines:
ROCRperf = performance(ROCRpred, "tpr", "fpr")
plot(ROCRperf, colorize=TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

as.numeric(performance(ROCRpred, "auc")@y.values)

# ----->0.8460121

################################################################################

# Question 11: Continuing to use your model from Question 8, 
# what is the model’s accuracy (on the training set) given the cutoff from Question 10? 
# Report the accuracy to three decimal places. HINT: Use the threshold value out to all 
# of its reported decimal places to ensure that your answer matches the solution
#--->0.8407643

#Calculate accuracy  
t1 = table(train$violator,b_predictions > 0.2015788)
t1

(t1[1,1]+t1[2,2])/nrow(train)
 
################################################################################

# Question 12 Continuing to use the model from Question 8, what is the sensitivity of the 
# model on the training set (to three decimal places)?

opt.cut = function(perf, pred){
  cut.ind = mapply(FUN=function(x, y, p){
    d = (x - 0)^2 + (y-1)^2
    ind = which(d == min(d))
    c(sensitivity = y[[ind]], specificity = 1-x[[ind]], 
      cutoff = p[[ind]])
  }, perf@x.values, perf@y.values, pred@cutoffs)
}
print(opt.cut(ROCRperf, ROCRpred))

# [,1]
# sensitivity 0.7222222
# specificity 0.8369305
# cutoff      0.2015788

 
################################################################################  
# Question 13: For the model from Question 8, which probability 
#threshold results in the best accuracy (on the training set)?

#------>C and D

# A. 0.2
tA = table(train$violator,b_predictions > 0.2)
tA

(tA[1,1]+tA[2,2])/nrow(train)

# B. 0.3
tB = table(train$violator,b_predictions > 0.3)
tB

(tB[1,1]+tB[2,2])/nrow(train)

# C. 0.4
tC = table(train$violator,b_predictions > 0.4)
tC

(tC[1,1]+tC[2,2])/nrow(train)

# D. 0.5
tD = table(train$violator,b_predictions > 0.5)
tD
(tD[1,1]+tD[2,2])/nrow(train)





################################################################################

# Question 14: Use your probability threshold from Question 13 to determine the accuracy of the model 
# on the testing set (to three decimal places).

################################################################################

test_predictions = predict(v_fit, test, type="prob")[2] #develop predicted probabilities

tTest = table(test$violator,test_predictions > 0.5)
tTest
(tTest[1,1]+tTest[2,2])/nrow(test)

#0.1029412
# 
# Description of dataset:
#   
# male: 1 if the parolee is male, 0 if female
# race: 1 if the parolee is white, 2 otherwise
# age: the parolee ís age (in years) when he or she was released from prison
# state: a code for the parolee ís state. 2 is Kentucky, 3 is Louisiana, 4 is Virginia, and 1 is any other state. The three states were selected due to having a high representation in the dataset.
# time.served: the number of months the parolee served in prison (limited by the inclusion criteria to not exceed 6 months).
# max.sentence: the maximum sentence length for all charges, in months (limited by the inclusion criteria to not exceed 18 months).
# multiple.offenses: 1 if the parolee was incarcerated for multiple offenses, 0 otherwise.
# crime: a code for the parolee ís main crime leading to incarceration. 2 is larceny, 3 is drug-related crime, 4 is driving-related crime, and 1 is any other crime.
# violator: 1 if the parolee violated the parole, and 0 if the parolee completed the parole without violation.
# 
# 
