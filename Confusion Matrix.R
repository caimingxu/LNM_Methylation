library(cvms)
library(tibble)
library(export)
library(xlsx) 

rt <- read.table("Training_WithRiskScores.txt",header = T,check.names = F,sep = "\t")
rt <- rt[,-c(1,3)]
basic_table <- table(rt)
basic_table


cfm <- as_tibble(basic_table)
cfm

plot_confusion_matrix(cfm, 
                      target_col = "group", 
                      prediction_col = "prediction",
                      counts_col = "n")
graph2ppt(file="confusion matrix.pptx", width=10, height=8)


## Creating a confusion matrix with evaluate()
eval <- evaluate(rt,
                 target_col = "target",
                 prediction_cols = "prediction",
                 type = "binomial")
eval
conf_mat <- eval$`Confusion Matrix`[[1]]
conf_mat  #Compared to the manually created version, we have two extra columns, Pos_0 and Pos_1. These describe whether the row is the True Positive, True Negative, False Positive, or False Negative, depending on which class (0 or 1) is the positive class.
plot_confusion_matrix(conf_mat)
write.xlsx(conf_mat,file = "result.xlsx")



