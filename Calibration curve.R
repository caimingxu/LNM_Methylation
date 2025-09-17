# Load package
library(CalibrationCurves)
library(export)

rt <- read.table(file = "val_riskScore.txt",header = T,row.names = 1,check.names = F,sep = "\t")
classifier = glm(formula = group ~ riskScore.val,data = rt,family ="binomial" )
y_pred = predict(classifier, newdata = rt,type = "response")


# Default calibration plot
val.prob.ci.2(y_pred, rt$group,m=30,cex=0.6)

graph2ppt(file="calibration curve.pptx", width=10, height=10)
# Adding logistic calibration curves and other additional features
val.prob.ci.2(y_pred, rt$group, CL.smooth=TRUE, logistic.cal=TRUE, lty.log=2,
              col.log="red", lwd.log=1.5)

val.prob.ci.2(y_pred, rt$group, CL.smooth=TRUE, logistic.cal=TRUE, lty.log=9,
              col.log="red", lwd.log=1.5, col.ideal=colors()[10], lwd.ideal=0.5)
