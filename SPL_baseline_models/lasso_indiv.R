# by carolin kunze, 552038, carolin.kunze@cms.hu-berlin.de
#setwd("/Users/caro/Uni/WiSe 1718/Business Analytics and Data Science/Assignment_BADS_WS1718")

# load library
if (!require("lubridate")) install.packages("lubridate")
library("lubridate")

# data cleaning
clean = function(dataset) {
    
    dataset$delivery_date[dataset$delivery_date == "?"] <- NA
    # dataset$missing_delivery = ifelse(is.na(dataset$delivery_date),1,0)
    
    # add delivery period
    dataset$delivery_date = as.Date(dataset$delivery_date)
    dataset$order_date = as.Date(dataset$order_date)
    dataset$delivery_period = as.numeric(dataset$delivery_date - dataset$order_date)
    dataset$delivery_period[dataset$delivery_period < 0] = NA
    dataset$delivery_period[is.na(dataset$delivery_period)] = mean(na.omit(dataset$delivery_period))
    dataset$delivery_period = floor(dataset$delivery_period)
    
    # add order weekday
    dataset$order_weekday = wday(dataset$order_date)
    
    # item size
    dataset$item_size[dataset$item_size == "unsized"] = NA
    
    dataset$item_size = gsub("xs|s|XS|S", "5000", dataset$item_size)
    dataset$item_size = gsub("M|m", "6000", dataset$item_size)
    dataset$item_size = gsub("xl|xxl|XXXL|XL|XXL|xxxl|L|l", "7000", dataset$item_size)
    dataset$item_size = gsub("[0-9][0-9][//+]", "7000", dataset$item_size)
    dataset$item_size = gsub("[0-9][//+]", "7000", dataset$item_size)
    
    dataset$item_size = as.integer(dataset$item_size)
    dataset$item_size[dataset$item_size <= 8] = 5000
    dataset$item_size[dataset$item_size > 8 & dataset$item_size <= 12] = 6000
    dataset$item_size[dataset$item_size > 12 & dataset$item_size <= 24] = 7000
    dataset$item_size[dataset$item_size > 24 & dataset$item_size <= 40] = 5000
    dataset$item_size[dataset$item_size > 40 & dataset$item_size <= 46] = 6000
    dataset$item_size[dataset$item_size > 46 & dataset$item_size <= 60] = 7000
    dataset$item_size[dataset$item_size > 60 & dataset$item_size <= 180] = 5000
    dataset$item_size[dataset$item_size > 3000 & dataset$item_size <= 4000] = 5000
    dataset$item_size[dataset$item_size > 2000 & dataset$item_size <= 3000] = 6000
    dataset$item_size[dataset$item_size > 4000 & dataset$item_size <= 4100] = 7000
    
    dataset$item_size[is.na(dataset$item_size)] = 6000
    dataset$item_size[dataset$item_size == 5000] = 1
    dataset$item_size[dataset$item_size == 6000] = 2
    dataset$item_size[dataset$item_size == 7000] = 3
    
    # item color
    dataset$item_color[dataset$item_color == "?"] = NA
    
    # categorized item_price
    bins = c(0, 35, 60, 90, 200, 1000)
    dataset$categorized_item_price = .bincode(dataset$item_price, bins, right = TRUE, include.lowest = TRUE)
    
    # registration time until now
    dataset$user_reg_date = as.Date(dataset$user_reg_date)
    dataset$registration_time_until_now = as.numeric(Sys.Date() - dataset$user_reg_date)
    dataset$registration_time_until_now = dataset$registration_time_until_now - min(dataset$registration_time_until_now)
    registration_bins = c(0, 240, 415, 775)
    dataset$registration_time_until_now = .bincode(dataset$registration_time_until_now, registration_bins, 
        right = TRUE, include.lowest = TRUE)
    
    # registration
    dataset$registration_time_until_order = dataset$order_date - dataset$user_reg_date
    
    # user_dob
    dataset$user_age = floor(as.numeric(Sys.Date() - as.Date(dataset$user_dob))/365)
    dataset$user_age[is.na(dataset$user_age)] = floor(mean(na.omit(dataset$user_age)))
    
    # user title
    dataset$user_title[dataset$user_title == "not reported"] = NA
    
    return(dataset)
}

# loading and cleaning train data
trainset = read.csv("BADS_WS1718_known.csv", sep = ",", header = TRUE)
trainset = clean(trainset)
head(trainset)

# loading and cleaning test data
dirty_testset = read.csv("BADS_WS1718_class_20180115.csv", sep = ",", header = TRUE)
testset = clean(dirty_testset)
head(testset)

# split sample into 75% train and 25% validation set
library(caret)
set.seed(420)
inTrainingSet <- createDataPartition(trainset$return, p = 0.75, list = FALSE)
train1 = trainset[inTrainingSet, ]
train2 = trainset[-inTrainingSet, ]

# transform data into model matrices for prediction model
x1 = model.matrix(return ~ categorized_item_price + item_size + registration_time_until_now + 
    user_age + order_date + registration_time_until_order + order_weekday, train1)
x2 = model.matrix(return ~ categorized_item_price + item_size + registration_time_until_now + 
    user_age + order_date + registration_time_until_order + order_weekday, train2)
y1 = train1$return
y2 = train2$return

# load packages
if (!require("caret")) install.packages("caret")
library("caret")
if (!require("e1071")) install.packages("e1071")
library("e1071")
if (!require("glmnet")) install.packages("glmnet")
library("glmnet")

# lasso - regularized logistic regression
lasso = glmnet(x = x1, y = y1, family = "binomial", standardize = TRUE, alpha = 1, nlambda = 10)


# predict on validation set
pred.lasso1 = predict(lasso, newx = x1, s = 0.01, type = "response")
pred.lasso2 = predict(lasso, newx = x2, s = 0.01, type = "response")

# AUC as comparison to other prediction models
print(auc(y2, pred.lasso2))
