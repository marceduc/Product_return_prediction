
library(klaR)
library(caret)
source("color.R")
rm(list = ls())
source("input_functions.R")

df = load_clean()
df = create_features(df)

# remove id and date columns except order_item_id
names(df)[names(df) == "order_item_id"] = "ID"
df                                      = df[!grepl("id", names(df))]
df                                      = df[, sapply(df, class) != "Date"]


# move return column to the front of the dataframe
return = df$return
df     = df[, -which(names(df) == "return")]
df     = cbind(return, df)
rm(return)

# split data into class, train and test set
known = df[!is.na(df$return), ]
class = df[is.na(df$return), ]
rm(df)

# split data into training and test set
set.seed(420)
inTrainingSet = createDataPartition(known$return, p = 0.75, list = FALSE)
tr            = known[inTrainingSet, ]
ts            = known[-inTrainingSet, ]
rm(known)


# add fold column to training set
k = 5
set.seed(123)
folds = folds = cut(1:nrow(tr), k, labels = F)
folds = sample(folds)
tr    = cbind(folds, tr)
rm(folds)

# get factor features
factor_feats = names(tr)[which(unlist(sapply(tr, class)) == "factor")]

# create subset of training set to estimate WOE
set.seed(100)
woe.idx   = createDataPartition(y = tr$return, p = 0.25, list = FALSE)

# decode return level 1 as 'level 1' which will be used as good risk
tr$return = factor(tr$return, levels = c("1", "0"))

# calculate class woes
woe.object = woe(return ~ ., data = tr[woe.idx, ], zeroadj = 0.5)
# change to original format of return
tr$return  = as.integer(as.character(tr$return))

# apply woe to all datasets
datasets = list(class, tr, ts)
ds       = lapply(datasets, function(x) predict(woe.object, newdata = x, replace = T))

# save prepared data to avoid processing time
write.csv(ds[[1]], "base_class3.csv", row.names = F)
write.csv(ds[[3]], "base_test3.csv", row.names = F)
write.csv(ds[[2]], "base_train3.csv", row.names = F)