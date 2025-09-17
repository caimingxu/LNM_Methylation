library(rmda)
library(export)


rt=read.table("val_riskScore.txt",header=T,sep="\t",row.names = 1)
rt<- as.data.frame(rt)
panel<- decision_curve(group~ riskScore.val,rt,
                        family = binomial(link ='logit'),
                        thresholds= seq(0,0.5, by = 0.005),
                        confidence.intervals = 0.95,
                        study.design = 'case-control',
                        population.prevalence = 0.3)


#tiff(file="desicion.tiff",height = 2500,width = 2500,res=300)
#pdf(file ="desicion.pdf" )
plot_decision_curve(list(panel),confidence.intervals=TRUE,
                    curve.names = c("panel"),
                    col = c("red"),
                    cost.benefit.axis=FALSE,
                    lwd = 2:2:2,
                    lty = 2,
                    standardize=FALSE,
                    legend.position = "none")
#dev.off()
graph2ppt(file="decision curve_model.pptx", width=10, height=10)



