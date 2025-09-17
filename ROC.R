
# install.packages("plotROC")

library(plotROC)
library(pROC)
library(export)

rt=read.table("cg07167872.txt",header=T,sep="\t",check.names=F,row.names=1)
basicplot=ggplot(rt,aes(d=group,m=cg07167872,col="red"))+geom_roc(n.cuts = 0)
#pdf('riskScore_clinical_plotROC.pdf',width = 10,height = 8)
basicplot+style_roc()
#dev.off()
graph2ppt(file="cg07167872_ROC.pptx", width=7, height=6)

#styleplot          #??ROC
roc=roc(rt$group,rt$cg07167872,plot=FALSE,ci=TRUE)
auc=auc(roc)          #????AUC?? 
CI=as.numeric(roc$ci)
LL <- CI[1]
HH <- CI[3]
v <- var(roc)
b <- roc$auc - .5
se <- sqrt(v)
z <- (b / se)
p <- 2 * pt(-abs(z), df=Inf)  ## two-sided test p-value

#------------------------------------------------------------------
#To calculate the optimal cutoff, 95%CI, auc, sensitivity, specificity, p-value
roc_result <- coords(roc, "best")
roc_result$auc <- auc
roc_result$'95L'<- LL
roc_result$'95H'<- HH
roc_result$'pValue'<- p

library("writexl")
write_xlsx(roc_result,"cg07167872_results.xlsx",col_names = TRUE,format_headers = TRUE,use_zip64 = FALSE)
#write.table(roc_result,"results.txt",sep = "\t",row.names = F)
#To show the optimal cutoff on ROC curve
#pdf('Show Cutoff on ROC.pdf',width = 10,height = 8)
#rocobj <- roc(rt$group, rt$riskScore)
#plot(rocobj,
#     legacy.axes = TRUE,
#     main="Best cutoff",
#     thresholds="best", # the best cutoff based on youden index
#     print.thres="best") # show the best cutoff on ROC curve
#dev.off()







###Video source: http://study.163.com/provider/1026136977/index.htm?share=2&shareId=1026136977
######Video source: http://www.biowolf.cn/shop/
######??????????: http://www.biowolf.cn/
######??????????2749657388@qq.com
######????????: 18520221056
