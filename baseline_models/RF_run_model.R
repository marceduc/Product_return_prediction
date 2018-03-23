library("randomForest")

# read prepared dataset
df2 = read.csv("full_prepared.csv")

# select relevant variables for model training
relevant_columns = c("item_price", "user_title", "user_state", "return", "age_group", "shipment_time", 
    "cust_item_count", "size_dupl", "color_dupl", "item_max_price", "discount")
data = df2[, relevant_columns]

# impute missing values with median/ mean, this is not in the prepare script since other
# models might deal with missing data
data$shipment_time[is.na(data$shipment_time)] = median(data$shipment_time, na.rm = T)

# get original labeled data set
known = data[!is.na(data$return), ]

# split data in training and test set
library(caret)
set.seed(420)
inTrainingSet <- createDataPartition(known$return, p = 0.75, list = FALSE)
train = known[inTrainingSet, ]
test = known[-inTrainingSet, ]

# train random forest and make predictions on test set
rf_model = randomForest(factor(return) ~ ., data = train, ntree = 500)
prediction2 = predict(rf_model, test, type = "Prob")

# calculate AUC
library(pROC)
print(auc(test$return, prediction2[, 2]))
