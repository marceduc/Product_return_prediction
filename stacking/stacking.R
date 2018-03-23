library("randomForest")
library("pROC")
library("rpart")
library("glmnet")
library("caret")
library("nnet")

tr = read.csv("base_train3.csv")
ts = read.csv("base_test3.csv")

# add for every model an empty column to tr_meta and ts_meta data frame
models  = c("RF", "DT", "Logit", "regLogit", "NN")
tr_meta = tr[1:4]
ts_meta = ts[1:3]

for (i in 1:length(models)) {
    tr_meta                         = cbind(tr_meta, rep(NA, nrow(tr)))
    names(tr_meta)[length(tr_meta)] = models[i]
    
    ts_meta                         = cbind(ts_meta, rep(NA, nrow(ts)))
    names(ts_meta)[length(ts_meta)] = models[i]
}

# use k-fold to get model predictions for the training data
k = 1:max(tr$folds)
#get indicies of non predictors
X_cols = !(names(tr) %in% c("ID", "folds", "return"))

for (i in k) {
    print(i)
    # split tr into train and validation set
    idx.val     = which(tr$folds == i, arr.ind = TRUE)
    inner_train = tr[-idx.val, ]
    val         = tr[idx.val, ]
    
    # train RF
    rf_model = randomForest(inner_train[, X_cols], factor(inner_train[, "return"]), ntree = 1500, 
        mtry = 2)
    boo = data.frame(ID = val$ID, y_hat = predict(rf_model, type = "Prob", newdata = val)[, 
        2])
    #add predictions to meta df
    tr_meta[match(boo[, "ID"], tr_meta[, "ID"]), "RF"] = boo$y_hat
    
    # train DT
    DT = rpart(return ~ . - ID - folds, data = inner_train, method = "class")
    boo = data.frame(ID = val$ID, y_hat = predict(DT, newdata = val)[, 2])
    #add predictions to meta df
    tr_meta[match(boo[, "ID"], tr_meta[, "ID"]), "DT"] = boo$y_hat
    
    # train Logit
    Logit = glm(return ~ . - folds - ID, data = inner_train, family = binomial(link = "logit"))
    boo   = data.frame(ID = val$ID, y_hat = predict(Logit, newdata = val, type = "response"))
    #add predictions to meta df
    tr_meta[match(boo[, "ID"], tr_meta[, "ID"]), "Logit"] = boo$y_hat
    
    # train regLogit
    x.train  = model.matrix(return ~ . - folds - ID, inner_train)
    x.val    = model.matrix(return ~ . - folds - ID, val)
    regLogit = glmnet(x = x.train, y = inner_train$return, family = "binomial", standardize = TRUE, 
        alpha = 1, nlambda = 2)
    y_hat    = predict(regLogit, newx = x.val, s = 0.01, type = "response")
    #add predictions to meta df
    tr_meta[match(val$ID, tr_meta[, "ID"]), "regLogit"] = y_hat
    
    # train NN
    normalizer.train = caret::preProcess(inner_train[, -1:-3], method = c("center", "scale"))
    normalizer.val   = caret::preProcess(val[, -1:-3], method = c("center", "scale"))
    nn.train         = predict(normalizer.train, newdata = inner_train[, -1:-3])
    nn.val           = predict(normalizer.val, newdata = val[, -1:-3])
    nn.train         = cbind(nn.train, inner_train[, 1:3])
    nn.val           = cbind(nn.val, val[, 1:3])
    neuralnet        = nnet(return ~ . - folds - ID, data = nn.train, trace = FALSE, size = 4, decay = 0.1, 
        maxNWts = 200)
    boo              = data.frame(ID = nn.val$ID, y_hat = predict(neuralnet, newdata = nn.val, type = "raw"))
    #add predictions to meta df
    tr_meta[match(boo[, "ID"], tr_meta[, "ID"]), "NN"] = boo$y_hat
}

# Fit each base model to the full training dataset and make predictions on the test
# dataset.
#RF
rf_model        = randomForest(tr[, X_cols], factor(tr[, "return"]), ntree = 1500, mtry = 2)
ts_meta[, "RF"] = predict(rf_model, type = "Prob", newdata = ts)[, 2]
#DT
DT              = rpart(return ~ . - ID - folds, data = tr, method = "class")
ts_meta[, "DT"] = predict(DT, newdata = data.frame(ts, folds = rep(1, nrow(ts))))[, 2]
#Logit
Logit              = glm(return ~ . - folds - ID, data = tr, family = binomial(link = "logit"))
ts_meta[, "Logit"] = round(predict(Logit, newdata = data.frame(ts, folds = rep(1, nrow(ts))), 
    type = "response"), 2)

#regLogit
x.train               = model.matrix(return ~ . - folds - ID, tr)
x.ts                  = model.matrix(return ~ . - ID, ts)
regLogit              = glmnet(x = x.train, y = tr$return, family = "binomial", standardize = TRUE, alpha = 1, 
    nlambda = 2)
ts_meta[, "regLogit"] = as.numeric(predict(regLogit, newx = x.ts, s = 0.01, type = "response"))

normalizer.train = caret::preProcess(tr[, -1:-3], method = c("center", "scale"))
normalizer.ts    = caret::preProcess(ts[, -1:-2], method = c("center", "scale"))
nn.train         = predict(normalizer.train, newdata = tr[, -1:-3])
nn.ts            = predict(normalizer.ts, newdata = ts[, -1:-2])
nn.train         = cbind(nn.train, tr[, 1:3])
nn.ts            = cbind(nn.ts, ts[, 1:2])
neuralnet        = nnet(return ~ . - folds - ID, data = nn.train, trace = FALSE, size = 4, decay = 0.1, 
    maxNWts = 200)
ts_meta[, "NN"]  = as.numeric(predict(neuralnet, newdata = data.frame(nn.ts, folds = rep(1, 
    nrow(nn.ts))), type = "raw"))

# train logit model as second stage model based on tr_meta data set
meta_mod = glm(return ~ RF + NN, data = tr_meta, family = binomial(link = "logit"))
# make second stage model predictions for the test set
y_hat = round(predict(meta_mod, newdata = data.frame(ts_meta, folds = rep(1, nrow(ts_meta))), 
    type = "response"), 2)

# get AUC results of the models
Tuned_AUCs = c(auc(ts_meta$return, ts_meta$DT), auc(ts_meta$return, ts_meta$RF), auc(ts_meta$return, 
    ts_meta$Logit), auc(ts_meta$return, ts_meta$regLogit), auc(ts_meta$return, ts_meta$NN))


########## Cost Sensitivity
cost_tr = function(pred.prob, true_y, price, threshold = seq(0, 1, 0.01)) {
    # calculates costs based on false positive and false negaitve counts for every threshold in the vector threshold
    
    cost = vector(length = 0)
    for (tr in threshold) {
        
        # make predictions based on threshold value
        pred.cl = ifelse(pred.prob >= tr, 1, 0)
        FN.idx  = which(pred.cl == 0 & true_y == 1)
        FP.idx  = which(pred.cl == 1 & true_y == 0)
        # calculate costs
        FN.cost = sum(-0.5 * price[FN.idx])
        FP.cost = sum(2.5 * (-3 - 0.1 * price[FP.idx]))
        total   = FN.cost + FP.cost
        cost    = c(cost, total)
    }
    cost
}

# use CV to get get meta_model predictions for training data
meta_tr_yhats = vector(length = nrow(tr_meta))
for (i in k) {
    
    print(i)
    # split tr into train and validation set
    idx.val     = which(tr_meta$folds == i, arr.ind = TRUE)
    inner_train = tr_meta[-idx.val, ]
    val         = tr_meta[idx.val, ]
    #train and predict meta model
    meta_mod = glm(return ~ RF + NN, data = inner_train, family = binomial(link = "logit"))
    meta_tr_yhats[idx.val] = round(predict(meta_mod, newdata = data.frame(val, folds = rep(1, 
        nrow(val))), type = "response"), 2)
    
}


# calculate training set cost matrix for all models and thresolds on a range from [0,1]
threshold = seq(0, 1, 0.01)
costs     = apply(cbind(tr_meta[, models], meta_tr_yhats), 2, cost_tr, true_y = tr_meta$return, 
    price = tr_meta$item_price, threshold = threshold)

# returns lowest costs per model
apply(costs, 2, max)
# returns threshold in % for lowest cost
apply(costs, 2, which.max)
opt_th = apply(costs, 2, which.max)

# calculate test set cost matrix for all models and thresolds on a range from [0,1]
costs_ts = apply(cbind(ts_meta[, models], Stacking = y_hat), 2, cost_tr, true_y = ts_meta$return, 
    price = ts_meta$item_price, threshold = threshold)

# write.csv(opt_th, 'opt.model.thresholds.csv')

# create table with the test set costs per model using the optimal threshold per model
naive_cost_ts = cost_tr(0, ts$return, ts$item_price, 0.5)
ts_cost_pred  = c(Naive = naive_cost_ts, costs_ts[39, "DT"], costs_ts[42, "RF"], costs_ts[45, 
    "Logit"], costs_ts[45, "regLogit"], costs_ts[43, "NN"], costs_ts[39, "Stacking"])

# divide cost table by observation count to get average costs per prediction
ts_avg_cost = ts_cost_pred/nrow(ts) * -1
ts_avg_cost
write.csv2(ts_avg_cost, 'ts_avg_cost.csv')


########## predict unlabeled data
class = read.csv("base_class3.csv")

# create second stage input for unlabeled data
normalizer.class = caret::preProcess(class[, -1:-2], method = c("center", "scale"))
nn.class = predict(normalizer.class, newdata = ts[, -1:-2])
nn.class = cbind(nn.ts, ts[, 1:2])

NN = as.numeric(predict(neuralnet, newdata = data.frame(nn.class, folds = rep(1, nrow(nn.ts))), 
    type = "raw"))
RF = round(predict(rf_model, type = "Prob", newdata = class[2:ncol(class)])[, 2], 2)
# add empty columns for model predictions, not used in the second stage model

class_meta = data.frame(return = class$return, ID = class$ID, item_price = class$item_price, 
    RF, DT = rep(NA, nrow(class)), Logit = rep(NA, nrow(class)), regLogit = rep(NA, nrow(class)), 
    NN)

# make final probability predictions
class_y_hat = round(predict(meta_mod, newdata = data.frame(class_meta, folds = rep(1, nrow(class_meta))), 
    type = "response"), 2)

# create final class predictions based on optimal threshold
res = data.frame(order_item_id = class$ID, return = ifelse(class_y_hat >= 0.39, 1, 0))

write.csv(res, 'predictions.csv')

########## Model assesment

# train each model with default parameters on the training set and make test set
# predictions
UT_rf_model     = randomForest(tr[, X_cols], factor(tr[, "return"]))
UT_RF_res       = predict(UT_rf_model, type = "Prob", newdata = ts)[, 2]
UT_regLogit     = glmnet(x = x.train, y = tr$return, family = "binomial", standardize = TRUE)
UT_res_regLogit = as.numeric(predict(UT_regLogit, newx = x.ts, s = 0.01, type = "response"))
UT_neuralnet    = nnet(return ~ . - folds - ID, data = nn.train, trace = FALSE, size = 3)
UT_res_NN       = as.numeric(predict(UT_neuralnet, newdata = data.frame(nn.ts, folds = rep(1, nrow(nn.ts))), 
    type = "raw"))

# calculate AUCs for the untuned models
UnTuned_AUCs = c(auc(ts_meta$return, ts_meta$DT), auc(ts_meta$return, UT_RF_res), auc(ts_meta$return, 
    ts_meta$Logit), auc(ts_meta$return, UT_res_regLogit), auc(ts_meta$return, UT_res_NN))

# create table with AUCs over the model building process
cbind(Tuned_AUCs, UnTuned_AUCs)

#create dataframe with NAs for baseline results
AUC_results = data.frame(Small_Feat_set = rep(NA, 5), Full_Feat_Set = UnTuned_AUCs, Parameter_Tuned = Tuned_AUCs, 
    Stacking = c(NA, auc(ts_meta$return, y_hat), NA, NA, auc(ts_meta$return, y_hat)))

row.names(AUC_results) = c("DT", "RF", "Logit", "regLogit", "NN")

AUC_results
write.csv(AUC_results,'Final_aucs_retuned.csv')
# to-do: add results from base line models manually to table

