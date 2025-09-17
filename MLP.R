library(pROC)
library(ggplot2)
library(keras)
library(dplyr)
library(caret)
library(tensorflow)
library(MASS)
library(class)
library(tree)
library(randomForest)
library(xgboost)
library(glmnet)
library(e1071)
library(ComplexHeatmap)
library(kernlab)
library(gbm)
library(tensorflow)

train<-read.table(file = 'training_Nagoya.txt',header = T,row.names = 1,check.names = F,sep = "\t")
val<-read.table(file = 'val_Kumamoto.txt',header = T,row.names = 1,check.names = F,sep = "\t")

type <- "Disease"
fold <- 10                  # 交叉验证，根据实际样本数量选择
hub_file <- "hup_gene.txt" # 读取核心基因名称
train_exp_names <- "DatasetA.txt"  # 设置训练集
val_exp_names <- "DatasetB.txt"  # 设置训练集

set.seed(123)
allfile <- list.files(paste0(getwd(), "/raw_data"))
labels_file <- allfile[grep(pattern = "sample*", x = allfile)]
exp_file <- allfile[! allfile %in% c(labels_file, hub_file)]
kkkaa <- c(1:length(exp_file))             # 提取数据集数量，方便下一行进行排序
exp_file <- exp_file[c(which(exp_file==train_exp_names), kkkaa[-which(exp_file==train_exp_names)])]
# 读取表达矩阵
exp_list <- lapply(paste0("raw_data/", exp_file), function(x){
  expi <- read.table(x, header = T, sep = "\t", row.names = 1,check.names = F)
  return(expi)
})
com_genes <- intersect(Reduce(intersect, lapply(exp_list, rownames)), read.table(paste0("raw_data/", hub_file), sep = "\t" )[,1]) # 基因集文件
exp_list <- lapply(exp_list, function(x, hubgenes){
  x <- t(x[hubgenes,])
  return(x)
}, hubgenes=com_genes)
# 读取label
labels_list <- lapply(paste0("raw_data/", labels_file), function(x){
  labelsi <- read.table(x, sep = "\t")
  return(labelsi)
})
all_labels <- bind_rows(labels_list)
rownames(all_labels) <- all_labels[,1]
all_labels[,2] <- ifelse(all_labels[,2]==type, 1, 0)
train_exp <- exp_list[[which(exp_file==train_exp_names)]]
val_exp <- exp_list[[which(exp_file==val_exp_names)]]

test_explist <- exp_list[which(!exp_file==train_exp_names)]
com <- intersect(rownames(train_exp), rownames(all_labels))
train_labels <- all_labels[com,]
train_exp <-train_exp[com,]
train_labels <- train_labels[, 2]
source("F1S.R")

### 参数设置
epochselecti <- 50    # 训练的循环数,需要指定
#batch_size <- seq(5, nrow(train_exp), 10)      # 训练的样本批次
batch_size <- c(40,80,120,160)      # 训练的样本批次
learningrate <- c(0.001, 0.005, 0.01, 0.05)  # 学习率
dropoutrate <- c(0.25, 0.5, 0.75)
cutoff <- c(0.25, 0.5, 0.75) # 设置不同的cutoff

###################### 正式执行分析
####################
#################
##############
############
#########
#######
###
##
# 结果列表，包括预测得分and/or预测类别
all_result_summary <- list()
# 精准度得分列表
all_result_acc <- list()
# 召回率得分列表
all_result_recall <- list()
# FS得分列表
all_result_FS <- list()
# 基因重要性列表
all_result_importance <- list()

for (batch_sizei in batch_size) {
  for (learningratei in learningrate) {
    for (dropoutratei in dropoutrate) {
      # 创建一个顺序模型
      mlpmodel <- keras_model_sequential()
      # 添加输入层和隐藏层,神经元数量需要调整
      mlpmodel %>%
        layer_dense(units = 32, activation = "relu", input_shape = ncol(train_exp)) %>%
        layer_dropout(rate = dropoutratei)  # 添加Dropout层以防止过拟合
      mlpmodel %>%
        layer_dense(units = 16, activation = "relu") %>%
        layer_dropout(rate = dropoutratei)  
      mlpmodel %>%
        layer_dense(units = 8, activation = "relu") %>%
        layer_dropout(rate = dropoutratei)  
      # 添加输出层
      mlpmodel %>%
        layer_dense(units = 1, activation = "sigmoid")
      # 编译模型
      mlpmodel %>% compile(
        loss = "binary_crossentropy",
        optimizer = optimizer_adam(learning_rate = learningratei),
        metrics = c("accuracy")
      )
      # 训练模型
      history <- mlpmodel %>% fit(
        x = train_exp,
        y = train_labels,
        epochs = epochselecti,
        batch_size = batch_sizei
      )
      # 打印模型结构
      summary(mlpmodel)
      for (cuti in cutoff) {
        # 评估模型
        train_result <-  as.data.frame(mlpmodel %>% predict(train_exp))
        train_result$type <- factor(ifelse(train_result[,1] > cuti, "positive", "negative") )
        colnames(train_result) <- c("predict_p", "predict_result")
        train_result$real_label <- factor(ifelse(train_labels==1, "positive", "negative"))
        all_result <- lapply(test_explist, function(data, labelsdata, model){
          comd <- intersect(rownames(data), rownames(labelsdata))
          labelsdata <- labelsdata[comd,2]
          expdata <- data[comd,]
          tresult <-  as.data.frame(model %>% predict(expdata))
          tresult$type <- factor(ifelse(tresult[,1] > cuti, "positive", "negative") )
          colnames(tresult) <- c("predict_p", "predict_result")
          tresult$real_label <- factor(ifelse(labelsdata==1, "positive", "negative"))
          return(tresult)
        }, labelsdata=all_labels, model=mlpmodel)
        all_result[[length(all_result)+1]] <- train_result  # 最后一个结果是train
        result_FS <- sapply(all_result, function(x){
          f1S <- f1_score(x$predict_result, x$real_label)
          return(f1S)
        })
        result_acc <- sapply(all_result, function(x){
          accuracy <- sum(as.character(x$predict_result) == as.character(x$real_label)) / length(as.character(x$real_label))
          return(accuracy)
        })
        result_recall <- sapply(all_result, function(x){
          true_positives <- sum(as.character(x$predict_result) == "positive" & as.character(x$real_label) == "positive")
          false_negatives <- sum(as.character(x$predict_result) == "negative" & as.character(x$real_label) == "positive")
          recall <- true_positives / (true_positives + false_negatives)
          return(recall)
        })
        all_result_acc[[paste0("NN-MLP (cutoff:", cuti,", lr:", learningratei, ", bs:", batch_sizei, ", ep:", epochselecti, ", dropout:", dropoutratei, ")")]] <- result_acc
        all_result_recall[[paste0("NN-MLP (cutoff:", cuti,", lr:", learningratei, ", bs:", batch_sizei, ", ep:", epochselecti, ", dropout:", dropoutratei, ")")]] <- result_recall
        all_result_FS[[paste0("NN-MLP (cutoff:", cuti,", lr:", learningratei, ", bs:", batch_sizei, ", ep:", epochselecti, ", dropout:", dropoutratei, ")")]] <- result_FS
        all_result_summary[[paste0("NN-MLP (cutoff:", cuti,", lr:", learningratei, ", bs:", batch_sizei, ", ep:", epochselecti, ", dropout:", dropoutratei, ")")]] <- all_result
        plot(history)+
          theme_bw()+ggtitle("Don't scale")
      }
    }
  }
}

##标准化前数据的训练结果
mod_history <- model%>% fit(train_x,train_y,epochs = 100,batch_size = 8,validation_split = 0.2,verbose = 0)
mod_history <- mlpmodel %>% fit(x = train_exp,y = train_labels,epochs = epochselecti,batch_size = batch_size,verbose = 2)
## 可视化训练过程
plot(history)+
  theme_bw()+ggtitle("Don't scale")

graph2ppt(file="effect plot.pptx", width=8, height=5)
