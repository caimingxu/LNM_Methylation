library(export)
rt <- read.table('Training_WithRiskScores.txt',header = T,sep = "\t",check.names = F)

risk.score <- rt$score
label <- rt$group
xlab = "Risk score"
title = ""
sample = NA
rotate.x = 90

library(ggpubr)
idx = order(risk.score)
risk.score <- risk.score[idx]
label = label[idx]
if(anyNA(sample)){
  tmp.df <- data.frame(Risk=risk.score, Class = label, ID=1:length(risk.score) )
}else{
  sample = sample[idx]
  tmp.df <- data.frame(Risk=risk.score, Class = label, ID=sample )
}
#tiff('riskScore plot.tiff',width = 1500,height = 800,res = 300)
colnames(tmp.df)[1] <- xlab
ggbarplot(tmp.df, x = "ID", y = xlab, xlab = "",
               color = "Class",fill = "Class",palette = c("#1B9E77","#D95F02"),
               legend = "right", title = title)
if(anyNA(sample)){
ggbarplot(tmp.df, x = "ID", y = xlab, xlab = "",
            color = "Class",fill = "Class",palette = c("#1B9E77","#D95F02"),
            legend = "right", title = title) + rremove("x.text") + rremove("x.axis") + rremove("x.ticks")
}else{
ggbarplot(tmp.df, x = "ID", y = xlab, xlab = "",
            color = "Class",fill = "Class",palette = c("#1B9E77","#D95F02"),
            legend = "right", title = title) + rotate_x_text(rotate.x)
}
graph2ppt(file="Training_waterfall plot.pptx", width=10, height=8)
#dev.off()


