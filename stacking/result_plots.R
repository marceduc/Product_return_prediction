library(ggplot2)
library(reshape2)
#load AUCs
aucs            = read.csv("Final_aucs_retuned.csv")
row.names(aucs) = aucs$X
#remove rownames
aucs = aucs[2:6]
aucs

line_set = melt(t(aucs))
line_set$Var1 = factor(line_set$Var1)

line_set$Var1 = rep(1:5, 5)
t(aucs)
ggplot(data = line_set, aes(x = Var1, y = value, colour = Var2)) + geom_line() + scale_x_discrete(name = "Model Building Process", 
    limits = c("Naive", "Small\nFeature Set", "Full\nFeature Set", "Parameter\nTuned", "Stacking")) + 
    scale_y_continuous(name = "AUC") + theme(legend.title = element_blank())



