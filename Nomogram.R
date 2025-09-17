library(rms)
library(export)
library(survival)
library(survminer)

rt=read.table("validation.txt",sep="\t",header=T,row.name=1,check.names=F)
dd=datadist(rt)
options(datadist="dd") 
pacman::p_load(regplot,rms)

F2<-lrm(group ~ ., data = rt)
summary(F2)

nom3 <- regplot(F2,       
                plots = c("density", "spikes"), ##连续性变量形状，可选"no plot" "density" "boxes" "ecdf" "bars" "boxplot" "violin" "bean" "spikes"；分类变量的形状，可选"no plot" "boxes" "bars" "spikes"
                observation = rt[1,], #红色点，对应行观测，或者T F
                center = T, # 对齐变量
                subticks = T,
                droplines = T,#是否画竖线
                title = "nomogram",
                points = T, # 截距项显示为0-100
                odds = F, # 是否显示OR值
                showP = T, # 是否显示变量的显著性标记
                rank = "sd", # 根据sd给变量排序
                interval="confidence", # 展示可信区间
                clickable = T # 是否可以交互
)

graph2ppt(file="Nomogram.pptx", width=12, height=5)


#install.packages("Hmisc")
#install.packages("lattice")
#install.packages("Formula")
#install.packages("ggplot2")
#install.packages("foreign")
#install.packages("rms")

library(rms)
library(export)
rt=read.table("riskOutput.txt",sep="\t",header=T,row.names=1,check.names=F)   #????????????
#rt=rt[c(1:(ncol(rt)-2))]
#rt$futime=rt$futime/12

dd <- datadist(rt)
options(datadist="dd")

#????????
f <- cph(Surv(futime, fustat) ~ Age + Stage + Position + riskScore, x=T, y=T, surv=T, data=rt, time.inc=1)
surv <- Survival(f)

#????nomogram
nom <- nomogram(f, fun=list(function(x) surv(1, x), function(x) surv(2, x), function(x) surv(5, x)), 
    lp=F, funlabel=c("1-year survival", "3-year survival", "5-year survival"), 
    maxscale=100, 
    fun.at=c(0.99, 0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3,0.2,0.1,0.05))     #????surv????????????????????????1????3????5????????????????surv????????????????"n-year survival"??????????

#nomogram??????
pdf(file="nomogram.pdf",height=6,width=10)
plot(nom)
dev.off()
graph2ppt(file="effect plot.pptx", width=10, height=6)
