#-------------------------------------------------------------------------------
# Spring 2017 MATH 218 Statistical Learning Final Project
# Due Tuesday 2017/5/23 12:00pm
#
# Team Members: Aayam Poudel and David Valentin
# Kaggle Competition Name: Rossman Store Sales
# Kaggle Competition URL: https://www.kaggle.com/c/rossmann-store-sales
#-------------------------------------------------------------------------------

# 1. Load All Necessary Packages ------------------------------------------
library(tidyverse)
library(broom)
library(lubridate)
#sexy themes 
install.packages('ggthemes')
library(ggthemes)
library(glmnet)
library(bisoreg)



# 2. Load Data Files & Data Cleaning --------------------------------------
#Aayam's working directory
setwd("~/Desktop/Junior Spring/Statistical Learning/Homework/Final/Team_F")
#David's working directory
#setwd("/TeamF")

train <- read_csv("./Files/train.csv")
test <- read_csv("./Files/test.csv")
sample_submissions <- read_csv("Files/sample_submission.csv")
#supplemetal information about the stores:
store <- read_csv("./Files/store.csv")

#since the store info is relevant to both train and test datasets let's join them to the datasets
train <- left_join(train,store, by="Store")
test <- left_join(test,store,by="Store")

#Since the dataset is too big, we couldn't do proper conputations so we decided to make a subset of it
train <- train %>%
  sample_frac(0.1)

modified_train <- train %>% 
  mutate(Year=year(Date), Month=month(Date), DayOfMonth=day(Date)) %>%
  select(-Date, -Promo2SinceWeek, -PromoInterval, -Promo2SinceYear, -CompetitionOpenSinceMonth, -CompetitionOpenSinceYear) %>% 
  mutate(StateHoliday = ifelse(is.na(StateHoliday), 0, StateHoliday)) %>% 
  mutate(CompetitionDistance = ifelse(is.na(CompetitionDistance), 0, CompetitionDistance)) %>% 
  group_by(Store) %>% 
  mutate(avg_sales_by_storetype = mean(Sales)) %>% 
  mutate(avg_customers_by_storetype = mean(Customers)) %>% 
  mutate(avg_distance_by_storetype = mean(CompetitionDistance)) %>% 
  mutate(total_num_promotions = sum(Promo2)) %>% 
  mutate(Sales_Per_Customer = signif(Sales/Customers, 5)) %>% 
  mutate(Sales_Per_Customer = ifelse(is.na(Sales_Per_Customer), 0, Sales_Per_Customer)) 


modified_test <- test %>%
  mutate(Year=year(Date), Month=month(Date), DayOfMonth=day(Date)) %>%
  select(-Date, -Promo2SinceWeek, -PromoInterval, -Promo2SinceYear, -CompetitionOpenSinceMonth, -CompetitionOpenSinceYear) %>% 
  mutate(CompetitionDistance = ifelse(is.na(CompetitionDistance), 0, CompetitionDistance)) %>% 
  mutate(StateHoliday = ifelse(is.na(StateHoliday), 0, StateHoliday)) %>% 
  group_by(Store) %>%
  mutate(avg_distance_by_storetype = mean(CompetitionDistance)) %>% 
  mutate(total_num_promotions = sum(Promo2))



# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------




# USE DAVID'S STUFF





# 4. Cross-Validation of Final Model --------------------------------------






# 5. Create Submission ----------------------------------------------------





# 6. Extras ---------------------------------------------------------------

# Section A
#splines


# Section B
#loess


# Section C
#ridge
model_formula <- Sales ~ Store + DayOfWeek + Open + StateHoliday + SchoolHoliday + CompetitionDistance + Promo2 + Year + Month + DayOfMonth + avg_distance_by_storetype + total_num_promotions

X <- model.matrix(model_formula, data = modified_train)[, -1]
y <- modified_train$Sales

n_folds <- 5
#Cross validation for ridge regression
#lambda_values <- 10^seq(from = 2, to = 4, length = 2500)
lambda_values <- 10^seq(from = -2, to = 4, length = 2500)
# Using built-in function, we don't need to use our own for-loops. Note
# alpha=0 for ridge regression
cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = n_folds)

lambda_star_ridge <- cv_ridge %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_ridge

#Creating submissions 




# Section D
#lasso

model_formula <- Sales ~ Store + DayOfWeek + Open + StateHoliday + SchoolHoliday + CompetitionDistance + Promo2 + Year + Month + DayOfMonth + avg_distance_by_storetype + total_num_promotions

X <- model.matrix(model_formula, data = modified_train)[, -1]
y <- modified_train$Sales

n_folds <- 5
#Cross validation for ridge regression
#lambda_values <- 10^seq(from = 2, to = 4, length = 2500)
lambda_values <- 10^seq(from = -2, to = 4, length = 2500)
# Using built-in function, we don't need to use our own for-loops. Note
# alpha=1 for lasso
cv_lasso <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = n_folds)

lambda_star_lasso <- cv_lasso %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_lasso



