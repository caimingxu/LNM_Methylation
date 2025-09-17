###### Step 1: 加载R包 ######
library(ggplot2)
library(ggrepel)
library(limma)
library(impute)
library(dplyr)
library(scales)

###### Step 2: 差异表达分析（使用limma） ######
# 读取表达矩阵（请根据实际路径修改）
rt <- read.table("sampleDEGs.txt", sep = "\t", header = TRUE, check.names = FALSE, row.names = 1)

# 分组信息（根据样本数调整）
class <- c(rep("con", 15), rep("treat", 6))
design <- model.matrix(~0 + factor(class))
colnames(design) <- c("con", "treat")

# 差异分析主流程
fit <- lmFit(rt, design)
cont.matrix <- makeContrasts(treat - con, levels = design)
fit2 <- contrasts.fit(fit, cont.matrix)
fit2 <- eBayes(fit2)
allDiff <- topTable(fit2, adjust = 'fdr', number = 200000)

# 保存差异分析结果
write.table(allDiff, file = "limmaTab.xls", sep = "\t", quote = FALSE)

# 标记显著性分组
diffsig <- allDiff
diffsig$Significance <- ifelse((diffsig$P.Value < 0.05 & abs(diffsig$logFC) > 0.25), 
                               ifelse(diffsig$logFC > 0.25, "Up", "Down"), "Not")

###### Step 3: 自定义横坐标压缩函数（±0.2范围压缩为1/8） ######
squish_transform <- function(x, compression = 8, threshold = 0.2) {
  sgn <- sign(x)
  abs_x <- abs(x)
  out <- ifelse(abs_x < threshold, x / compression, 
                sgn * (abs_x - threshold + threshold / compression))
  return(out)
}

squish_inverse <- function(y, compression = 8, threshold = 0.2) {
  sgn <- sign(y)
  abs_y <- abs(y)
  out <- ifelse(abs_y < threshold / compression, y * compression, 
                sgn * (abs_y - threshold / compression + threshold))
  return(out)
}

squish_trans <- trans_new(
  name = "squished",
  transform = squish_transform,
  inverse = squish_inverse
)

###### Step 4: 火山图绘制 ######
pdf(file = "volcano.pdf",width = 6,height = 5)
ggplot(diffsig, aes(logFC, -log10(P.Value))) +
  geom_point(aes(color = Significance), alpha = 1, size = 2) +
  scale_color_manual(
    values = c("Up" = "#ff6b6b", "Down" = "#70a1ff", "Not" = "#d3d3d3"),
    labels = c("Up" = "Up", "Down" = "Down", "Not" = "No change")
  ) +
  scale_x_continuous(
    trans = squish_trans,
    limits = c(-0.5, 0.5),
    breaks = c(-0.5, -0.3, 0, 0.3, 0.5)  # ✅ 去掉 ±0.2 的刻度
  ) +
  geom_vline(xintercept = c(-0.25, 0.25), color = "black", linetype = "dashed") +  # ✅ 仅保留 ±0.25 虚线
  geom_hline(yintercept = -log10(0.05), color = "black", linetype = "dashed") +
  labs(
    x = expression(log[2]*"(FoldChange)"),
    y = expression(-log[10]*"(P.Value)"),
    title = ""
  ) +
  theme_bw() +  # ✅ 白色背景
  theme(
    axis.text = element_text(size = 13),
    axis.title = element_text(size = 13),
    plot.title = element_text(size = 16, hjust = 0.5, face = "bold"),
    legend.title = element_blank(),
    legend.text = element_text(size = 11),
    panel.grid = element_blank(),      # ✅ 去除灰色网格线
    axis.line = element_blank(),       # ✅ 去除坐标轴线
    panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8)  # ✅ 黑色边框保留
  )
dev.off()
