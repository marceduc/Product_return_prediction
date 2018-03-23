# Neural Network

# set working directory
setwd("./")

# load libraries
if (!require("nnet")) install.packages("nnet")
library("nnet")
if (!require("caret")) install.packages("caret")
library("caret")
if (!require("hmeasure")) install.packages("hmeasure")
library(hmeasure)
if (!require("xtable")) install.packages("xtable")
library(xtable)
if (!require("NeuralNetTools")) install.packages("NeuralNetTools")
library("NeuralNetTools")

# load our feature adjusted training data
tr = read.csv("base_train3.csv")

# cross validation and preparation for models
cv.generateModel = function(dataset, parameters) {
    # iterate over number of folds
    k = max(dataset$folds)
    for (i in 1:k) {
        # Split data into training and validation by using the folds
        idx.val             = which(dataset$folds == i, arr.ind = TRUE)
        cv.train            = dataset[-idx.val, ]
        cv.val              = dataset[idx.val, ]
        # normalize the data (excluding target variable 'return', 'folds' and 'ID')
        normalizer.train    = caret::preProcess(cv.train[, -1:-3], method = c("center", "scale"))
        normalizer.val      = caret::preProcess(cv.val[, -1:-3], method = c("center", "scale"))
        nn.train            = predict(normalizer.train, newdata = cv.train[, -1:-3])
        nn.val              = predict(normalizer.val, newdata = cv.val[, -1:-3])
        # put together normalized data and variables 'return', 'folds' and 'ID'
        nn.train            = cbind(nn.train, cv.train[, 1:3])
        nn.val              = cbind(nn.val, cv.val[, 1:3])
        # call training function for neural network
        do.nn(nn.train, nn.val, i, parameters)
    }
}

# train the neural network model by conducting parameter tuning on decay of the weights and size of the
# hidden layer (number of nodes)
do.nn = function(nn.train, nn.val, i, parameters) {
    # iterate over parameter grid
    for (n in 1:nrow(parameters)) {
        neuralnet = nnet(return ~ . - ID - folds, data = nn.train, trace = FALSE, maxit = 200, size = parameters$decay[n], 
            decay = parameters$decay[n], maxNWts = 200)
        predict_validate_nn(neuralnet, nn.val, i, n)
    }
}
# predict and validate neural network model
predict_validate_nn = function(neuralnet, nn.val, i, n) {
    yhat.val            = predict(neuralnet, newdata = nn.val, type = "raw")
    # compute AUC
    h                   = HMeasure(nn.val$return, yhat.val)
    print(h$metrics["AUC"])
    nn_results[i, n]    <<- h$metrics["AUC"]
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
    # find best size of hidden layer
    best_size       = results_table$size[which(results_table[, "AUC"] == max(results_table[, "AUC"]))]
    print(paste0("best size: ", best_size))
    # find best weight decay
    best_decay      = results_table$decay[which(results_table[, "AUC"] == max(results_table[, "AUC"]))]
    print(paste0("best decay: ", best_decay))
    
    return(results_table)
}
# save results as tex table
save_table = function(table, caption, filename) {
    print.xtable(xtable(table, caption = caption), "latex", filename, size = "\\fontsize{9pt}{10pt}\\selectfont", 
        booktabs = TRUE)
}

# set number of folds
k = max(tr$folds)
# test for size of hidden layer 1 to 15 and for decay 0.001 to 0.1 by using a grid search
nnet.params         = expand.grid(size = seq(1, 15, 1), decay = c(0.1, 0.01, 0.001))
nn_results          = as.data.frame(matrix(0, nrow = k, ncol = nrow(nnet.params)))
cv.generateModel(tr, nnet.params)
nn_results
nn_results_table    = create_results_table(nn_results, nnet.params)
nn_results_table
save_table(nn_results_table, "Neural Network Accuracy Results", "nn_results.tex")

# plot and save neural network
neuralnet = nnet(return ~ . - ID - folds, data = tr, trace = FALSE, size = 4, decay = 0.1)
png("nn_plot_10_01.png", 700, 700)
plotnet(neuralnet, max_sp = TRUE)
dev.off() 
