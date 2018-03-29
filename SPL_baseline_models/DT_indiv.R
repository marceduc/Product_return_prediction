library(lubridate)
library(lattice)
library(ggplot2)
library(caret)
library(rpart)
# define data set name
filename_known = "BADS_WS1718_known.csv"
filename_class = "BADS_WS1718_class_20180115.csv"

load_data = function(filename) {
    
    
    rawdata = read.csv(filename, na.strings = "?")
    if (length(names(rawdata)) != 14) {
        rawdata$return = rep(NA, nrow(rawdata))
        
    }
    
    # missing_delivery = ifelse(is.na(rawdata$delivery_date),1,0)
    rawdata$delivery_date[rawdata$delivery_date == "1990-12-31"] = NA
    order_date = ymd(as.character(rawdata$order_date))
    delivery_date = ymd(as.character(rawdata$delivery_date))
    user_dob = ymd(as.character(rawdata$user_dob))
    user_reg_date = ymd(as.character(rawdata$user_reg_date))
    age = year(user_reg_date) - year(user_dob)
    
    del_time = order_date - delivery_date
    avg_del_time = as.integer(mean(del_time, na.rm = TRUE))
    delivery_date[is.na(delivery_date)] = order_date + days(avg_del_time)
    
    ord_wdy = as.factor(wday(order_date, label = TRUE))
    del_wdy = as.factor(wday(delivery_date, label = TRUE))
    ord_month = as.factor(month(order_date, label = TRUE))
    del_month = as.factor(month(delivery_date, label = TRUE))
    
    
    analy_set  = data.frame(item_color = rawdata$item_color, price = rawdata$item_price, title = rawdata$user_title, 
        state  = rawdata$user_state, age, del_time, ord_wdy, del_wdy, ord_month, del_month, 
        return = rawdata$return)
    analy_set
    
}

analy_set = load_data(filename_known)

# split data in training and test set

set.seed(420)
inTrainingSet <- createDataPartition(analy_set$return, p = 0.75, list = FALSE)
train = analy_set[inTrainingSet, ]
test = analy_set[-inTrainingSet, ]

# remove unknown item_color levels
test$item_color[!(test$item_color %in% levels(train$item_color))] = NA


library(rpart)
library(pROC)
# train decision tree and predict the test set
tree       = rpart(return ~ ., data = train, method = "class")
prediction = predict(tree, newdata = test, type = "prob")

# calculate AUC
print(auc(test$return, prediction[, 2]))

