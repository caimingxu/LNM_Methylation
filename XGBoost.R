###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######??????ѧ??: http://www.biowolf.cn/
######???????䣺2749657388@qq.com
######????΢??: 18520221056

#install.packages('survival')
#install.packages("survminer")

library(caret)
library(readxl)
library(splitTools)
library(tidyverse)
library(DAAG)
library(tidyverse)
library(beeswarm)
library(caret)
library(Ckmeans.1d.dp)
library(cowplot)
library(caTools) 
library(class) 
library(caTools)
library(cutpointr)
library(DiagrammeR)
library(dplyr)
library(e1071)
library(EnhancedVolcano)
library(export)
library(ggbeeswarm)
library(ggplot2)
library(ggridges)
library(ggpubr)
library(ggeasy)
library(gt)
library(harrypotter)
library(kernelshap)
library(magrittr)
library(pROC)
library(palmerpenguins)
library(pheatmap)
library(plyr)
library(raincloudplots)
library(readxl)
library(RColorBrewer)
library(readr)
library(raincloudplots)
library(shapr)
library(shapviz)
library(SHAPforxgboost)
library(SwimmeR)
library(swimplot)
library(survival)
library(survminer)
library(tidyverse)
library(viridis)
library(wesanderson)
library(xgboost)
library(beeswarm)

#################set.seed(361)LR################
set.seed(489)
rt <- read_excel("overall.xlsx")
#rt <- rt[c(1:13, 15:21)]

train <- subset(rt, cohort == "training")
validation <- subset(rt, cohort == "validation")


##########logloss,aucpr gamma = 1#################logloss,aucpr gamma = 1#################logloss,aucpr gamma = 1################## 
xgb_params <- list(booster = "gbtree", eta = 0.08,
                   max_depth = 6,
                   subsample = 0.04, colsample_bytree = 1,
                   validate_parameters = "TRUE", objective = "binary:logistic", eval_metric = "aucpr",
                   num_class = length(levels(train$group)))

y_train <- as.integer(train$group)
y_valid <- as.integer(validation$group)


train_data <- train[,c(4:12)]
validation_data <- validation[,c(4:12)]




xgb_train<- xgb.DMatrix(data = as.matrix(train_data), label = y_train)
xgb_valid <- xgb.DMatrix(data = as.matrix(validation_data), label = y_valid)



#Models
xgb_model <- xgb.train(params = xgb_params, data = xgb_train, nrounds = 100, verbose = 1)



#XGB Individual, cell-free
train$XGB_pred <- predict(xgb_model, as.matrix(train_data), reshape = TRUE)
validation$XGB_pred <- predict(xgb_model, as.matrix(validation_data), reshape = TRUE)
write.table(cbind(id=rownames(train),group=train$group,riskScore=train$XGB_pred),file="train_riskScore.txt",sep = "\t",row.names = F)
write.table(cbind(id=rownames(validation),group=validation$group,riskScore=validation$XGB_pred),file="val_cf_riskScore.txt",sep = "\t",row.names = F)

##############cf&ex0############
plot.roc(train$group, train$XGB_pred,
         main = "AUROC Training
         [Norm: cf_15b5p Learner; Method: I level -XGBoost Classifier-]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="navy")

plot.roc(validation$group, validation$XGB_pred,
         main = "AUROC Validation
         [Norm: cf_15b5p Learner; Method: I level -XGBoost Classifier-]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="red")

plot.roc(train$group, train$XGB_exo_pred,
         main = "AUROC Training
         [Norm: exo_15b5p Learner; Method: I level -XGBoost Classifier-]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="navy")

plot.roc(validation$group, validation$XGB_exo_pred,
         main = "AUROC Validation
         [Norm: exo_15b5p Learner; Method: I level -XGBoost Classifier-]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="red")
library(ggplot2)
library(ggpubr)
ggplot(train, aes(x=train$XGB_cf_pred, y=train$XGB_exo_pred)) +
  stat_density_2d(aes(fill = ..density..), geom = "raster", contour = F) +
  scale_fill_viridis_c(option= "A", direction=1, guide = "colourbar")+
  facet_wrap(vars(group)) +
  theme_pubclean()+ xlim(0.3,0.9)+ ylim(0.3, 0.8)+
  ggtitle("Title")

graph2ppt(file="Train_cf&exo_panel-density.pptx", width=7, height=6)
#####Plot ROC and get AUC results#######
rocobj <- roc(validation$group, validation$XGB_exo_pred,auc = TRUE,
              ci=TRUE, # compute AUC (of AUC by default)
              print.auc=TRUE) # print the AUC (will contain the CI)

ciobj <- ci.se(rocobj, # CI of sensitivity
               specificities=seq(0, 1, 0.01)) # over a select set of specificities

auc<-auc(rocobj)[1]
auc_low<-ci(rocobj,of="auc")[1]
auc_high<-ci(rocobj,of="auc")[3]
auc_full<-paste("AUC:",round(auc,digits = 3),"(",
                round(auc_low,digits = 3),",",round(auc_high,digits = 3),")",sep = "")

data_ci<-ciobj[1:101,1:3]
data_ci<-as.data.frame(data_ci)
x=as.numeric(rownames(data_ci))
data_ci<-data.frame(x,data_ci)
library(ggplot2)
ggroc(rocobj,color="red",size=1)+theme_bw()+
  geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  geom_ribbon(data = data_ci,aes(x=x,ymin=X2.5.,ymax=X97.5.), fill = 'lightblue',alpha=0.5)+
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        #legend.title=title, 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))+
  labs(x="Specificity",y="Sensitivity")
graph2ppt(file="val_exo_ROC.pptx", width=7, height=6)

roc=roc(validation$group,validation$XGB_exo_pred,plot=FALSE,ci=TRUE)
auc=auc(roc)          #????AUC?? 
CI=as.numeric(roc$ci)
LL <- CI[1]
HH <- CI[3]
v <- var(roc)
b <- roc$auc - .5
se <- sqrt(v)
z <- (b / se)
p <- 2 * pt(-abs(z), df=Inf)  ## two-sided test p-value

roc_result <- coords(roc, "best")
roc_result$auc <- auc
roc_result$'95L'<- LL
roc_result$'95H'<- HH
roc_result$'pValue'<- p

library("writexl")
write_xlsx(roc_result,"val_exo_results.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)



roc=roc(validation$group,validation$XGB_exo_pred,plot=FALSE,ci=TRUE)
auc=auc(roc)          #????AUC?? 
CI=as.numeric(roc$ci)
LL <- CI[1]
HH <- CI[3]
v <- var(roc)
b <- roc$auc - .5
se <- sqrt(v)
z <- (b / se)
p <- 2 * pt(-abs(z), df=Inf)  ## two-sided test p-value

roc_result <- coords(roc, "best")
roc_result$auc <- auc
roc_result$'95L'<- LL
roc_result$'95H'<- HH
roc_result$'pValue'<- p

library("writexl")
write_xlsx(roc_result,"val_exo_results.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)

importance_matrix_cf <- xgb.importance(model = xgb_model_cf)
write.table(importance_matrix_cf, file = "Importance_Matrix_xgb_model_cf.txt", sep="\t")

importance_matrix_exo <- xgb.importance(model = xgb_model_exo)
write.table(importance_matrix_exo, file = "Importance_Matrix_xgb_model_exo.txt", sep="\t")

#Print the decision tree
DecisionTree_cf <- xgb.plot.multi.trees(model = xgb_model_cf)
print(DecisionTree_cf)

#SHAP Values analysis
shap_long_cf <- shap.prep(xgb_model = xgb_model_cf, X_train = as.matrix(train_cf))
write.table(shap_long_cf, file = "SHAP_Values_long_cf.txt", sep="\t")

shap_long_exo <- shap.prep(xgb_model = xgb_model_exo, X_train = as.matrix(train_exo))
write.table(shap_long_exo, file = "SHAP_Values_long_exo.txt", sep="\t")

#Print the decision tree
DecisionTree_exo <- xgb.plot.multi.trees(model = xgb_model_exo)
print(DecisionTree_exo)


##LogReg, Stacked, final
LR_final_learner <- glm(group ~XGB_exo_pred+XGB_cf_pred, data=train, family="binomial")
train$LR_final_pred <- predict(LR_final_learner, train)
validation$LR_final_pred <- predict(LR_final_learner, validation)
write.table(cbind(id=rownames(train),group=train$group,riskScore=train$LR_final_pred),file="train_cf+exo_riskScore.txt",sep = "\t",row.names = F)
write.table(cbind(id=rownames(validation),group=validation$group,riskScore=validation$LR_final_pred),file="val_cf+exo_riskScore.txt",sep = "\t",row.names = F)

#############PCDC FInal#############
plot.roc(train$group, train$LR_final_pred,
         main = "AUROC Training
         [Norm: Final Learner; Meth: III-Level Logistic Regression]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="navy")
plot.roc(validation$group,validation$LR_final_pred,
         main = "AUROC Training
         [Norm: Final Learner; Meth: III-Level Logistic Regression]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="navy")
#############violin plot###############

library(ggstatsplot)
ggbetweenstats(
  data  = validation,
  x     = group,
  y     =  LR_final_pred,
  title = "Distribution of sepal length across Iris species"
)
library(export)
graph2ppt(file="validation_cf&exoviolin.pptx", width=10, height=10)


plot.roc(validation$group, validation$LR_final_pred,
         main = "AUROC validation
         [Norm: Final Learner; Meth: III-Level stacked Logistic Regression]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="red")

#####Plot ROC and get AUC results#######
rocobj <- roc(validation$group, validation$LR_final_pred,auc = TRUE,
              ci=TRUE, # compute AUC (of AUC by default)
              print.auc=TRUE) # print the AUC (will contain the CI)

ciobj <- ci.se(rocobj, # CI of sensitivity
               specificities=seq(0, 1, 0.01)) # over a select set of specificities

auc<-auc(rocobj)[1]
auc_low<-ci(rocobj,of="auc")[1]
auc_high<-ci(rocobj,of="auc")[3]
auc_full<-paste("AUC:",round(auc,digits = 3),"(",
                round(auc_low,digits = 3),",",round(auc_high,digits = 3),")",sep = "")

data_ci<-ciobj[1:101,1:3]
data_ci<-as.data.frame(data_ci)
x=as.numeric(rownames(data_ci))
data_ci<-data.frame(x,data_ci)
library(ggplot2)
ggroc(rocobj,color="red",size=1)+theme_bw()+
  geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  geom_ribbon(data = data_ci,aes(x=x,ymin=X2.5.,ymax=X97.5.), fill = 'lightblue',alpha=0.5)+
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        #legend.title=title, 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))+
  labs(x="Specificity",y="Sensitivity")
graph2ppt(file="val_cf+exo_ROC.pptx", width=7, height=6)

roc=roc(validation$group,validation$LR_final_pred,plot=FALSE,ci=TRUE)
auc=auc(roc)          #????AUC?? 
CI=as.numeric(roc$ci)
LL <- CI[1]
HH <- CI[3]
v <- var(roc)
b <- roc$auc - .5
se <- sqrt(v)
z <- (b / se)
p <- 2 * pt(-abs(z), df=Inf)  ## two-sided test p-value

roc_result <- coords(roc, "best")
roc_result$auc <- auc
roc_result$'95L'<- LL
roc_result$'95H'<- HH
roc_result$'pValue'<- p

library("writexl")
write_xlsx(roc_result,"val_cf+exo_results.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)

#############################@@@@@@5 miRNA ############
xgb_params <- list(booster = "gbtree", eta = 0.01,
                   max_depth = 5,
                   subsample = 0.1, colsample_bytree = 1,
                   validate_parameters = "TRUE", objective = "binary:logistic", eval_metric = "aucpr",
                   num_class = length(levels(train$group)))


set.seed(16)
rt <- read_excel("overall_cf+exo.xlsx")
rt <- rt[c(1:13, 15:21)]


train <- subset(rt, cohort == "training")
validation <- subset(rt, cohort == "validation")



##########logloss,aucpr gamma = 1#################logloss,aucpr gamma = 1#################logloss,aucpr gamma = 1################## 
xgb_params <- list(booster = "gbtree", eta = 0.01,
                   max_depth = 4,
                   subsample = 0.1, colsample_bytree = 1,
                   validate_parameters = "TRUE", objective = "binary:logistic", eval_metric = "aucpr",
                   num_class = length(levels(train$group)))

y_train <- as.integer(train$group)
y_valid <- as.integer(validation$group)


train_exo2 <- train[,c(12,13,18:20)]
train_cf2 <- train[,c(4,5,7,10,11)]
validation_exo2 <- validation[,c(12,13,18:20)]
validation_cf2 <- validation[,c(4,5,7,10,11)]


xgb_train_cf2<- xgb.DMatrix(data = as.matrix(train_cf2), label = y_train)
xgb_train_exo2 <- xgb.DMatrix(data = as.matrix(train_exo2), label = y_train)


xgb_valid_cf2 <- xgb.DMatrix(data = as.matrix(validation_cf2), label = y_valid)
xgb_valid_exo2 <- xgb.DMatrix(data = as.matrix(validation_exo2), label = y_valid)



#Models
xgb_model_cf2 <- xgb.train(params = xgb_params, data = xgb_train_cf2, nrounds = 100, verbose = 1)

xgb_model_exo2 <- xgb.train(params = xgb_params, data = xgb_train_exo2, nrounds = 100, verbose = 1)


#XGB Individual, cell-free
train$XGB_cf_pred2 <- predict(xgb_model_cf2, as.matrix(train_cf2), reshape = TRUE)
validation$XGB_cf_pred2 <- predict(xgb_model_cf2, as.matrix(validation_cf2), reshape = TRUE)
write.table(cbind(id=rownames(train),group=train$group,riskScore=train$XGB_cf_pred2),file="train_cf_riskScore.txt",sep = "\t",row.names = F)
write.table(cbind(id=rownames(validation),group=validation$group,riskScore=validation$XGB_cf_pred2),file="val_cf_riskScore.txt",sep = "\t",row.names = F)

train$XGB_exo_pred2 <- predict(xgb_model_exo2, as.matrix(train_exo2), reshape = TRUE)
validation$XGB_exo_pred2 <- predict(xgb_model_exo2, as.matrix(validation_exo2), reshape = TRUE)
write.table(cbind(id=rownames(train),group=train$group,riskScore=train$XGB_exo_pred2),file="train_exo_riskScore.txt",sep = "\t",row.names = F)
write.table(cbind(id=rownames(validation),group=validation$group,riskScore=validation$XGB_exo_pred2),file="val_exo_riskScore.txt",sep = "\t",row.names = F)


##############cf&ex0############
plot.roc(train$group, train$XGB_cf_pred2,
         main = "AUROC Training
         [Norm: cf_15b5p Learner; Method: I level -XGBoost Classifier-]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="navy")

plot.roc(validation$group, validation$XGB_cf_pred2,
         main = "AUROC Validation
         [Norm: cf_15b5p Learner; Method: I level -XGBoost Classifier-]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="red")

plot.roc(train$group, train$XGB_exo_pred2,
         main = "AUROC Training
         [Norm: exo_15b5p Learner; Method: I level -XGBoost Classifier-]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="navy")

plot.roc(validation$group, validation$XGB_exo_pred2,
         main = "AUROC Validation
         [Norm: exo_15b5p Learner; Method: I level -XGBoost Classifier-]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="red")

#####Plot ROC and get AUC results#######
rocobj <- roc(validation$group, validation$XGB_exo_pred2,auc = TRUE,
              ci=TRUE, # compute AUC (of AUC by default)
              print.auc=TRUE) # print the AUC (will contain the CI)

ciobj <- ci.se(rocobj, # CI of sensitivity
               specificities=seq(0, 1, 0.01)) # over a select set of specificities

auc<-auc(rocobj)[1]
auc_low<-ci(rocobj,of="auc")[1]
auc_high<-ci(rocobj,of="auc")[3]
auc_full<-paste("AUC:",round(auc,digits = 3),"(",
                round(auc_low,digits = 3),",",round(auc_high,digits = 3),")",sep = "")

data_ci<-ciobj[1:101,1:3]
data_ci<-as.data.frame(data_ci)
x=as.numeric(rownames(data_ci))
data_ci<-data.frame(x,data_ci)
library(ggplot2)
ggroc(rocobj,color="red",size=1)+theme_bw()+
  geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  geom_ribbon(data = data_ci,aes(x=x,ymin=X2.5.,ymax=X97.5.), fill = 'lightblue',alpha=0.5)+
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        #legend.title=title, 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))+
  labs(x="Specificity",y="Sensitivity")
graph2ppt(file="val_exo_ROC.pptx", width=7, height=6)

roc=roc(validation$group,validation$XGB_cf_pred2,plot=FALSE,ci=TRUE)
auc=auc(roc)          #????AUC?? 
CI=as.numeric(roc$ci)
LL <- CI[1]
HH <- CI[3]
v <- var(roc)
b <- roc$auc - .5
se <- sqrt(v)
z <- (b / se)
p <- 2 * pt(-abs(z), df=Inf)  ## two-sided test p-value

roc_result <- coords(roc, "best")
roc_result$auc <- auc
roc_result$'95L'<- LL
roc_result$'95H'<- HH
roc_result$'pValue'<- p

library("writexl")
write_xlsx(roc_result,"val_cf_results.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)

importance_matrix_cf2 <- xgb.importance(model = xgb_model_cf2)
write.table(importance_matrix_cf, file = "Importance_Matrix_xgb_model_cf2.txt", sep="\t")

importance_matrix_exo2 <- xgb.importance(model = xgb_model_exo2)
write.table(importance_matrix_exo2, file = "Importance_Matrix_xgb_model_exo2.txt", sep="\t")

#Print the decision tree
DecisionTree_cf2 <- xgb.plot.multi.trees(model = xgb_model_cf2)
print(DecisionTree_cf2)

#SHAP Values analysis
shap_long_cf2 <- shap.prep(xgb_model = xgb_model_cf2, X_train = as.matrix(train_cf2))
write.table(shap_long_cf2, file = "SHAP_Values_long_cf2.txt", sep="\t")

shap_long_exo2 <- shap.prep(xgb_model = xgb_model_exo2, X_train = as.matrix(train_exo2))
write.table(shap_long_exo2, file = "SHAP_Values_long_exo2.txt", sep="\t")

#Print SHAP plot
shp_cf2 <- shapviz(xgb_model_cf2, X_pred = as.matrix(train_cf2))
sv_waterfall(shp_cf2,row_id = 2)
graph2ppt(file="shp_cf.pptx", width=7, height=6)

shp_exo2 <- shapviz(xgb_model_exo2, X_pred = as.matrix(train_exo2))
sv_waterfall(shp_exo2,row_id = 2)
graph2ppt(file="shp_exo.pptx", width=7, height=6)

#Print the decision tree
DecisionTree_exo2 <- xgb.plot.multi.trees(model = xgb_model_exo2)
print(DecisionTree_exo2)


##LogReg, Stacked, final
LR_final_learner2 <- glm(group ~XGB_cf_pred2+XGB_exo_pred2, data=train, family="binomial")
train$LR_final_pred2 <- predict(LR_final_learner2, train)
validation$LR_final_pred2 <- predict(LR_final_learner2, validation)
write.table(cbind(id=rownames(train),group=train$group,riskScore=train$LR_final_pred2),file="train_cf+exo_riskScore.txt",sep = "\t",row.names = F)
write.table(cbind(id=rownames(validation),group=validation$group,riskScore=validation$LR_final_pred2),file="val_cf+exo_riskScore.txt",sep = "\t",row.names = F)

#############PCDC FInal#############
plot.roc(train$group, train$LR_final_pred2,
         main = "AUROC Training
         [Norm: Final Learner; Meth: III-Level Logistic Regression]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="navy")

plot.roc(validation$group, validation$LR_final_pred2,
         main = "AUROC validation
         [Norm: Final Learner; Meth: III-Level stacked Logistic Regression]",cex.main=1,
         legacy.axes=F, percent=T, xlab="False Positive rate (%)", ylab="True Positive rate(%)", print.auc=T, ci=T,print.auc.cex = 1,print.auc.x= 50,print.auc.y= 20,
         print.thres="best",print.thres.best.method="youden", print.thres.adj=c(-0.15, 1.2), print.thres.pattern="Cut-off: %.3f \n\nSp: %.3f \nSe: %.3f",
         print.thres.pattern.cex =1, print.thres.pch=1,print.thres.cex=2.0, col="red")

#####Plot ROC and get AUC results#######
rocobj <- roc(validation$group, validation$LR_final_pred2,auc = TRUE,
              ci=TRUE, # compute AUC (of AUC by default)
              print.auc=TRUE) # print the AUC (will contain the CI)

ciobj <- ci.se(rocobj, # CI of sensitivity
               specificities=seq(0, 1, 0.01)) # over a select set of specificities

auc<-auc(rocobj)[1]
auc_low<-ci(rocobj,of="auc")[1]
auc_high<-ci(rocobj,of="auc")[3]
auc_full<-paste("AUC:",round(auc,digits = 3),"(",
                round(auc_low,digits = 3),",",round(auc_high,digits = 3),")",sep = "")

data_ci<-ciobj[1:101,1:3]
data_ci<-as.data.frame(data_ci)
x=as.numeric(rownames(data_ci))
data_ci<-data.frame(x,data_ci)
library(ggplot2)
ggroc(rocobj,color="red",size=1)+theme_bw()+
  geom_segment(aes(x = 1, y = 0, xend = 0, yend = 1), 
               colour='grey', linetype = 'dotdash') +
  geom_ribbon(data = data_ci,aes(x=x,ymin=X2.5.,ymax=X97.5.), fill = 'lightblue',alpha=0.5)+
  theme(plot.title = element_text(hjust = 0.5), 
        legend.justification=c(1, 0), legend.position=c(.95, .05),
        #legend.title=title, 
        legend.background = element_rect(fill=NULL, size=0.5, 
                                         linetype="solid", colour ="black"))+
  labs(x="Specificity",y="Sensitivity")
graph2ppt(file="Train_cf+exo_ROC.pptx", width=7, height=6)

roc=roc(validation$group,validation$LR_final_pred2,plot=FALSE,ci=TRUE)
auc=auc(roc)          #????AUC?? 
CI=as.numeric(roc$ci)
LL <- CI[1]
HH <- CI[3]
v <- var(roc)
b <- roc$auc - .5
se <- sqrt(v)
z <- (b / se)
p <- 2 * pt(-abs(z), df=Inf)  ## two-sided test p-value

roc_result <- coords(roc, "best")
roc_result$auc <- auc
roc_result$'95L'<- LL
roc_result$'95H'<- HH
roc_result$'pValue'<- p

library("writexl")
write_xlsx(roc_result,"val_cf+exo_results.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)

