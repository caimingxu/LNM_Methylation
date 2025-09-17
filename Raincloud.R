library("plyr")
library("lattice")
library("ggplot2")
library("dplyr")
library("readr")
library("rmarkdown")
library("Rmisc")
library("devtools")
library("gghalves")
library(tidyverse)
library(ggstatsplot)
library(PupillometryR)
library(ggrain)
library(raincloudplots)
library(ggplot2)
library(ggallin)
library(export)

rt=read.table("val_riskScore.txt",header=T,sep="\t",check.names=F)
rt$f1f2 <- interaction(rt$group,rt$subgroup)

ggplot(rt, aes(x = group, y = riskScore.val, fill = group),alpha = 0.5) +
  geom_rain(rain.side = 'l') + coord_equal(1) + coord_flip() 

graph2ppt(file="val_raincloud.pptx", width=6, height=3)


write.table(iris,"cf&exo.txt",sep="\t")

  
