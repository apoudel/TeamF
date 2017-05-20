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
#install.packages('ggthemes')
library(ggthemes)
library(glmnet)
install.packages('bisoreg')
library(bisoreg)


# 2. Load Data Files & Data Cleaning --------------------------------------

#The working directory. Make sure to modify it when you run your code
setwd("~/Desktop/Junior Spring/Statistical Learning/Homework/Final/Team_F")

train <- read_csv("Files/train.csv")
test <- read_csv("Files/test.csv")
sample_submissions <- read_csv("Files/sample_submission.csv")
#supplemetal information about the stores:
store <- read_csv("Files/store.csv")

#since the store info is relevant to both train and test datasets let's join them to the datasets
train <- left_join(train,store, by="Store")
test <- left_join(test,store,by="Store")

#Since the dataset is too big, I couldn't do proper conputations so I am going to select a subset
train <- train %>%
  sample_frac(0.1) %>% 
  mutate(Year=year(Date), Month=month(Date), DayOfMonth=day(Date))

test <- test %>%
  mutate(Year=year(Date), Month=month(Date), DayOfMonth=day(Date))


# 3. Top 4-5 Visualizations/Tables of EDA ---------------------------------

#The first way of doing exploratory data analysis is looking at the data. Just looking at a dataset might not give
#us too much information about the data, but it provides us with some information.
View(train)

#3a EDA on day of week and month
#group data by day
group_by_day <- train %>%
  group_by(DayOfWeek) %>%
  summarise(TotalSales = mean(Sales))

# Lets take a look at the mean of the sales by day of week!
dayofweek_sales <- ggplot(group_by_day, aes(y=TotalSales, x=DayOfWeek)) +
  geom_bar(stat="identity") +
  xlab("Day of Week") + 
  ylab("Total Number of Sales") + 
  ggtitle("Mean Number of Sales by Customers") + theme_economist()

#group data by month
group_by_month <- train %>%
  group_by(Month) %>%
  summarise(TotalSales = mean(Sales))


# Lets take a look at the mean of the sales by day of week!
month_sales <- ggplot(group_by_month, aes(y=TotalSales, x=Month, fill="red")) +
  geom_bar(stat="identity") +
  xlab("Month") + 
  ylab("Total Number of Sales") + 
  ggtitle("Mean Number of Sales by Customers") + theme_economist()

#3b. EDA on Promo
#Let's look at the correlation between Sales and whether there was a promo event or not.
promo_vs_sales <- ggplot(train, aes(x=Promo, y=Sales, group=Promo)) +
  geom_boxplot() +
  scale_y_log10() +
  ylab("Sales") + 
  xlab("Non-Promotion VS Promotion") + theme_economist()

promo_vs_sales

#3c. EDA on Store Type 
store_vs_sales <- ggplot(train, aes(x=StoreType, y=Sales, group=StoreType)) +
  geom_boxplot() +
  scale_y_log10() +
  ylab("Sales") + 
  xlab("Store type") + theme_economist()

store_vs_sales

#3d.by assortment
store_vs_assortment <- ggplot(train, aes(x=Assortment, y=Sales, group=Assortment)) +
  geom_boxplot() +
  ylab("Sales") + 
  xlab("Assortment") + theme_economist()

store_vs_assortment


# 4. Cross-Validation of Final Model --------------------------------------

# Estimated score





# 5. Create Submission ----------------------------------------------------


# Submitting the above submission.csv file to Kaggle we get a Categorization
# Accuracy Score of 0.7655, which is close to the estimate score above






# 6. Extras ---------------------------------------------------------------




# Things left to do ------------------

# Use appopriate variables for the models you make
# CV the models
#Tidy up how to deal with NA's -> Use average 





















# Section A
train_fill <- train %>% mutate(CompetitionDistance2 = ifelse(is.na(CompetitionDistance), 0, CompetitionDistance))
test_fill <- test %>% mutate(CompetitionDistance2 = ifelse(is.na(CompetitionDistance), 0, CompetitionDistance))

train_na_removed <- train


df <- 10
model_spline <- smooth.spline(x=train_fill$CompetitionDistance2, y=train_fill$Sales, df=df)

spline_predictions <- model_spline %>%
  predict(x=test_fill$CompetitionDistance2) %>% 
  tbl_df() %>%
  transmute(CompetitionDistance2 = x, Sales = y)

test_fill_2 <- test_fill %>%
  select(Id, CompetitionDistance2)

spline_submission_example <- left_join(test_fill_2, spline_predictions, by="CompetitionDistance2") 


spline_submission_example <- spline_submission_example %>% distinct(Id, Sales)

# Section B
model_SL <- lm(Sales~Store+Promo+StateHoliday+SchoolHoliday+DayOfWeek, data=train)

fitted_values <- augment(model_SL, newdata = test)

submission_example <- fitted_values %>%
  mutate (Sales = .fitted) %>%
  select(Id, Sales)

#Regression doesn't work well as we are just trying to draw a line

# Section C
#loess

span <- 0.8
model_loess <- loess(Sales ~ CompetitionDistance, train, span=span)

predictions_loess <- predict(model_loess, newdata=test$CompetitionDistance) %>%
  tbl_df()

submission_loess <- data.frame(test$Id,predictions_loess$value) %>%
  transmute(Id=test.Id,Sales=predictions_loess.value)

submission_loess %>% 
  readr::write_csv("Files/submission_loess.csv")

#Section D
#Ridge regressiom
model_formula <- Sales ~ Store + CompetitionDistance2

X <- model.matrix(model_formula, data = train_fill)[, -1]
y <- train_fill$Sales

model_ridge <- glmnet(X, y, alpha = 0, lambda = 3)

model_formula <- ~ Store + CompetitionDistance2

test_X <- model.matrix(model_formula, data = test_fill)[, -1]

predictions <- model_ridge %>% 
  predict(newx=test_X, s=3) %>% 
  as.vector()

sample_submissions %>% 
  mutate(Sales = as.vector(predictions))

n_folds <- 5

#Cross validation for ridge regression
#lambda_values <- 10^seq(from = 2, to = 4, length = 2500)
lambda_values <- 10^seq(from = 1, to = 4, length = 500)
# Using built-in function, we don't need to use our own for-loops. Note
# alpha=0 for ridge regression
cv_ridge <- cv.glmnet(X, y, alpha = 0, lambda=lambda_values, nfolds = n_folds)

lambda_star_ridge <- cv_ridge %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_ridge




#Section E
#LASSO
model_formula <- Sales ~ Store + CompetitionDistance2

X <- model.matrix(model_formula, data = train_fill)[, -1]
y <- train_fill$Sales

model_ridge <- glmnet(X, y, alpha = 1, lambda = 3)

model_formula <- ~ Store + CompetitionDistance2

test_X <- model.matrix(model_formula, data = test_fill)[, -1]

predictions <- model_ridge %>% 
  predict(newx=test_X, s=3) %>% 
  as.vector()
#Cross validation for lasso
#lambda_values <- 10^seq(from = 2, to = 4, length = 2500)
lambda_values <- 10^seq(from = 0.1, to = 4, length = 500)
# Using built-in function, we don't need to use our own for-loops. Note
# alpha=0 for ridge regression
cv_ridge <- cv.glmnet(X, y, alpha = 1, lambda=lambda_values, nfolds = n_folds)

lambda_star_ridge <- cv_ridge %>% 
  glance() %>% 
  .[["lambda.min"]]
lambda_star_ridge



sample_submissions %>% 
  mutate(Sales = as.vector(predictions))



