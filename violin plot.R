########
library(ggstatsplot)
rt <- read.table("input.txt",header = T,sep = "\t",row.names = 1,check.names = F)
ggbetweenstats(
  data  = rt,
  x     = group,
  y     =  riskScore,
  title = "Distribution of sepal length across Iris species"
) 
library(export)
graph2ppt(file="violin_overall.pptx", width=10, height=10)
