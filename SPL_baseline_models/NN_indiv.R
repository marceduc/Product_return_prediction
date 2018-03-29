# read prepared data set
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

# train neural network and make predictions on test set
library("nnet")
normalizer.train = caret::preProcess(train, method = c("center", "scale"))
normalizer.val = caret::preProcess(test, method = c("center", "scale"))
nn.train = predict(normalizer.train, newdata = train)
nn.val = predict(normalizer.val, newdata = test)
nn.train$return = train$return
nn.val$return = test$return
neuralnet = nnet(return ~ ., data = nn.train, trace = FALSE, size = 3)
y_hat = predict(neuralnet, newdata = nn.val, type = "raw")

# calculate AUC
library(pROC)
print(auc(test$return, y_hat[, 1]))


