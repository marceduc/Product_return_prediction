## import Dataset
train <- read.csv("BADS_WS1718_known.csv", sep = ",", stringsAsFactors = FALSE, header = TRUE)
test <- read.csv("BADS_WS1718_class_20180115.csv", sep = ",", stringsAsFactors = FALSE, header = TRUE)

# intialisation of the last column
test$return = rep(NA, nrow(test))

# Cleaning of delivery_date
train$delivery_date[train$delivery_date == "?"] <- NA
train$missing_delivery = ifelse(is.na(train$delivery_date), 1, 0)
train$delivery_date <- ifelse(train$delivery_date < train$order_date, NA, train$delivery_date)
summary(as.Date(train$delivery_date))
summary(as.Date(train$order_date))


test$delivery_date[test$delivery_date == "?"] <- NA
test$missing_delivery = ifelse(is.na(test$delivery_date), 1, 0)
test$delivery_date <- ifelse(test$delivery_date < test$order_date, NA, test$delivery_date)
summary(as.Date(test$delivery_date))
summary(as.Date(test$order_date))

# cleaning user_dob and use mean of user_dob for ?
summary(as.Date(train$user_dob))
train$user_dob <- ifelse(train$user_dob == "?", "1964-12-30", train$user_dob)

summary(as.Date(test$user_dob))
test$user_dob <- ifelse(test$user_dob == "?", "1964-12-30", test$user_dob)

# calculating number of days between order_date and delivery_date and safe it as a new
# variable
train$date_diff <- as.Date(train$delivery_date) - as.Date(train$order_date)

test$date_diff <- as.Date(test$delivery_date) - as.Date(test$order_date)

# claculate the mean delivery time and assign to NA
mean(train$date_diff, na.rm = TRUE)
train$date_diff[is.na(train$date_dif)] <- 11

mean(test$date_diff, na.rm = TRUE)
test$date_diff[is.na(test$date_dif)] <- 11


# calculate the age of the user at the time of the order of the item and safe as new
# variable
age <- (as.Date(train$order_date) - as.Date(train$user_dob))/365
train$user_age <- age

age1 <- (as.Date(test$order_date) - as.Date(test$user_dob))/365
test$user_age <- age1

# assign weekdays to the order date
if (!require("lubridate")) install.packages("lubridate")
library("lubridate")
train$order_weekday = wday(train$order_date)
test$order_weekday = wday(test$order_date)


# create variable registration time until today
train$registration_time <- (Sys.Date() - as.Date(train$user_reg_date))/365
train$registration_time <- train$registration_time

test$registration_time <- (Sys.Date() - as.Date(test$user_reg_date))/365
test$registration_time <- test$registration_time

# categorize the item price in 'very cheap =1','cheap =2', 'normal=3', 'expensive=4' and
# 'very expensive=5'
summary(train$item_price)
train$price_categorize[train$item_price <= 34.9] <- 1
train$price_categorize[train$item_price > 34.9 & train$item_price <= 59.9] <- 2
train$price_categorize[train$item_price > 59.9 & train$item_price <= 70.28] <- 3
train$price_categorize[train$item_price > 70.28 & train$item_price <= 89.9] <- 4
train$price_categorize[train$item_price > 89.9] <- 5
summary(as.factor(train$price_categorize))

summary(test$item_price)
test$price_categorize[test$item_price <= 34.9] <- 1
test$price_categorize[test$item_price > 34.9 & test$item_price <= 59.9] <- 2
test$price_categorize[test$item_price > 59.9 & test$item_price <= 70.28] <- 3
test$price_categorize[test$item_price > 70.28 & test$item_price <= 89.9] <- 4
test$price_categorize[test$item_price > 89.9] <- 5
summary(as.factor(test$price_categorize))


# clean item_color and assign the color black istead of '?'
train$item_color <- ifelse(train$item_color == "?", "black", train$item_color)
train$item_color <- ifelse(train$item_color == "brwon", "brown", train$item_color)
train$item_color <- ifelse(train$item_color == "blau", "blue", train$item_color)
train$item_color <- ifelse(train$item_color == "oliv", "olive", train$item_color)
train$item_color


# cleaning column item_size
levels(factor(train$item_size))
train$item_size <- ifelse(train$item_size == "s", "S", train$item_size)
train$item_size <- ifelse(train$item_size == "m", "M", train$item_size)
train$item_size <- ifelse(train$item_size == "l", "L", train$item_size)
train$item_size <- ifelse(train$item_size == "xs", "XS", train$item_size)
train$item_size <- ifelse(train$item_size == "xl", "XL", train$item_size)
train$item_size <- ifelse(train$item_size == "xxl", "XXL", train$item_size)
train$item_size <- ifelse(train$item_size == "xxxl", "XXXL", train$item_size)
train$item_size <- ifelse(train$item_size == "unsized", "L", train$item_size)

# load packages
if (!require("caTools")) install.packages("caTools")
library("caTools")
if (!require("hmeasure")) install.packages("hmeasure")
library("hmeasure")
if (!require("caret")) install.packages("caret")
library("caret")
if (!require("e1071")) install.packages("e1071")
library("e1071")
if (!require("glmnet")) install.packages("glmnet")
library("glmnet")

# split data into test and training set
library(caret)
set.seed(420)
inTrainingSet <- createDataPartition(train$return, p = 0.75, list = FALSE)
training.set = train[inTrainingSet, ]
test.set = train[-inTrainingSet, ]

# train model on training set and predict test set
logit = glm(return ~ date_diff + user_age + registration_time + order_weekday + price_categorize, 
    data = training.set, family = binomial(link = "logit"))
y_hat = round(predict(logit, newdata = test.set, type = "response"), 2)

# calculate AUCs
library(pROC)
print(auc(test.set$return, y_hat))
