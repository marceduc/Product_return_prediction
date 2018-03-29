#Load Necessary libraries
library(randomForest)
library(forecast)
library(ggplot2)
library(reshape2)
library(pROC)

# load training data
tr = read.csv("base_train3.csv")

# get out-of-bag error for different number of trees
# define max number of trees
maxtree  = 2500 
# col idx of irrelevant columns
X_cols   = !(names(tr) %in% c("ID", "folds", "return")) 
# train random forest
rf_model = randomForest(tr[, X_cols], factor(tr[, "return"]), ntree = maxtree)
# get OOB Error
OOB_err  = rf_model$err.rate[, 1] 

# write.csv(OOB_err, paste0('OBB_err n=75k ntry=',maxtree,'new.csv'))
# OOB_err = read.csv("OBB_err n=75k ntry=2500.csv")[, 2]

#use moving average of order 100 to smooth OOB Err
ma_full = ma(OOB_err, 100) 
OOB_df  = data.frame(OOB_Err = ma_full, nTree = 1:maxtree)  
# plot OOB over number of trees built
ggplot(OOB_df, aes(x = nTree, y = OOB_Err)) + geom_line() + geom_hline(yintercept = min(OOB_df$OOB_Err, 
    na.rm = T), color = "red", linetype = "dashed") + geom_hline(yintercept = min(OOB_df$OOB_Err, 
    na.rm = T) + 0.001, color = "red", linetype = "dashed") + theme_bw() + xlab("Number of Trees") + 
    ylab("Out-of-Bag Error")


# tune maximal number of fetures per tree (= mtry)

ntree = 100 #set number of trees for cross validation
floor(sqrt(ncol(tr[, X_cols]))) # get default mtry for comparison
mtrys = 1:20 #define min max mtry to test
k     = 1:max(tr$folds) #get k for k Folds

# use kFolds to determin beste mtry value
auc_mat = matrix(nrow = length(mtrys), ncol = max(k)) #
for (v in mtrys) {
    
    for (i in k) {
        print(paste("mtry:", v, "fold:", i))
        # split tr into train and validation set
        idx.val     = which(tr$folds == i, arr.ind = TRUE)
        inner_train = tr[-idx.val, ]
        val         = tr[idx.val, ]
        #train model with training set
        rf_model = randomForest(inner_train[, X_cols], factor(inner_train[, "return"]), ntree = ntree, 
            mtry = v)
        #predict labels of validation set
        y_hat = predict(rf_model, val, type = "Prob")
        #add AUCs to matrix
        auc_mat[v, i] = auc(val$return, y_hat[, 2])
        
    }
}

# boxplot of AUC results over 
# clean data for plot
mtry_df          = melt(as.data.frame(t(auc_mat)))
mtry_df$variable = as.character(mtry_df$variable)
names(mtry_df)   = c("Maximum number of features", "AUC")
mtry_df$`Maximum number of features` = as.factor(as.numeric(substring(mtry_df$`Maximum number of features`, 
    2, nchar(mtry_df$`Maximum number of features`))))
# plot results
ggplot(mtry_df, aes(x = `Maximum number of features`, y = AUC)) + geom_boxplot() + theme_bw()

# plot average AUC values over mtry
mtry_df2 = data.frame(AUC = rowMeans(auc_mat), mtry = seq(1, 20, 1))
ggplot(mtry_df2, aes(x = mtry, y = AUC)) + geom_line()

# write.csv(auc_mat, 'AUC_mtry_CV.csv') auc_mat =read.csv('AUC_mtry_CV.csv')

# get Gini Mean Decrease to evaluate Feature Importance
imp            = data.frame(Feature = row.names(importance(rf_model)),
                 Category = rep("", nrow(importance(rf_model))), 
                 MeanDecreaseGini = importance(rf_model))
imp            = imp[order(imp$MeanDecreaseGini, decreasing = T), ]
row.names(imp) = seq(1, nrow(imp), 1)
#save table with Feature importances
write.csv(imp, "Feat_importance.csv")

