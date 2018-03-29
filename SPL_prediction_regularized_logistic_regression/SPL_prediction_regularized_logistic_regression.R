# Regularized Logistic Regression

# set working directory
setwd("./")

# load libraries
if (!require("glmnet")) install.packages("glmnet")
library("glmnet")
if (!require("hmeasure")) install.packages("hmeasure")
library(hmeasure)
if (!require("xtable")) install.packages("xtable")
library(xtable)

# load our feature adjusted training data
tr = read.csv("base_train3.csv")

# cross validation and preparation for models
cv.generateModel = function(dataset, parameters) {
    # iterate over number of folds
    k = max(dataset$folds)
    for (i in 1:k) {
        # Split data into training and validation by using the folds
        idx.val     = which(dataset$folds == i, arr.ind = TRUE)
        cv.train    = dataset[-idx.val, ]
        cv.val      = dataset[idx.val, ]
        # transform data into model matrices
        x.train     = model.matrix(return ~ . - ID - folds, cv.train)
        x.val       = model.matrix(return ~ . - ID - folds, cv.val)
        # call training function for regularized logistic regression
        do.regLogit(x.train, x.val, cv.train$return, cv.val, i, parameters)
    }
}

# train the regularized logistic regression model by conducting parameter tuning on alpha as the
# parameter for lasso, ridge or elastic net and lambda as the shrinkage penalty
do.regLogit = function(x, x.val, y, cv.val, i, parameters) {
    # iterate over parameter grid
    for (n in 1:nrow(parameters)) {
        regLogit = glmnet(x = x, y = y, family = "binomial", standardize = TRUE, alpha = parameters$alpha[n], 
            nlambda = parameters$nlambda[n])
        predict_validate_regLogit(regLogit, x.val, cv.val$return, n, i)
    }
}

# predict and validate regularized logistic regression model
predict_validate_regLogit = function(regLogit, x.val, predict.target, n, i) {
    # predict yhat
    yhat.regLogit           = predict(regLogit, newx = x.val, s = 0.01, type = "response")
    # compute AUC
    h                       = HMeasure(predict.target, yhat.regLogit)
    print(h$metrics["AUC"])
    regLogit_results[i, n]  <<- h$metrics["AUC"]
}

# create results table for prediction accuracies
create_results_table = function(results, parameters) {
    results_transposed                              = data.frame(t(results))
    results_transposed$average                      = rowMeans(results_transposed)
    results_table                                   = cbind(parameters, results_transposed[, max(tr$folds) + 1])
    colnames(results_table)[length(parameters) + 1] = "AUC"
    # find highest accuracy over all parameter combinations
    max_accuracy    = max(results_table[, length(parameters) + 1])
    print(paste0("best accuracy: ", round(max_accuracy, 6)))
    # find best value for lambda
    best_lambda     = results_table$nlambda[which(results_table[, "AUC"] == max(results_table[, "AUC"]))]
    print(paste0("best lambda: ", best_lambda))
    # find best alpha
    best_alpha      = results_table$alpha[which(results_table[, "AUC"] == max(results_table[, "AUC"]))]
    print(paste0("best alpha: ", best_alpha))
    
    return(results_table)
}
# save results as tex table
save_table = function(table, caption, filename) {
    print.xtable(xtable(table, caption = caption), "latex", filename, size = "\\fontsize{9pt}{10pt}\\selectfont", 
        booktabs = TRUE)
}

# set number of folds
k = max(tr$folds)

# test for lambda 1 up to 500 and alphas 0 to 1 by using grid search
regLogit.params         = expand.grid(nlambda = c(seq(1, 10, 1), 20, 50, 100, 200, 500), alpha = seq(0, 1, 0.2))
regLogit_results        = as.data.frame(matrix(0, nrow = k, ncol = nrow(regLogit.params)))
cv.generateModel(tr, regLogit.params)
regLogit_results
regLogit_results_table  = create_results_table(regLogit_results, regLogit.params)
regLogit_results_table
save_table(regLogit_results_table, "Reguralized Logistic Regression Accuracy Results for Lambda Values 1 to 500", 
    "regLogit_results.tex") 
