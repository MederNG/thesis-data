library(MAGeCKFlute)
library(ggplot2)
library(dplyr)
library(ggrepel)
getwd()
setwd("C:/Users/meder/OneDrive/Desktop/Thesis docs")
data<-read.csv("mle_post_processing.csv")
selected_columns1 <- c("X", "Gene", "X2D_4w_DMSO", "X2D_4w_MRTX")
selected_columns2 <- c("Gene", "X2D_4w_MRTX","X2D_4w_MRTX.wald.fdr")
dd.rra <- data[, selected_columns2]
dd.rra$Rank <- rank(-dd.rra$X2D_4w_MRTX, ties.method = "max")
head(dd.rra)
dd.rra$LogFDR = -log10(dd.rra$X2D_4w_MRTX.wald.fdr)
geneList= dd.rra$X2D_4w_MRTX
names(geneList) = dd.rra$Gene
snake<-RankView(geneList, top = 5, bottom = 5, genelist = c("MAP4K2", "CDK19", "FYN")) + xlab("Rank")
print(snake)

ggsave("snakeplot.png", snake, width = 10, height = 8, dpi = 1000)

volcano = VolcanoView(dd.rra, x = "X2D_4w_MRTX", y = "X2D_4w_MRTX.wald.fdr", Label = "Gene")
genes_to_mark <- c("MAP4K2", "CDK19", "FYN")
highlighted_genes_data <- dd.rra[dd.rra$Gene %in% genes_to_mark, ]

volcano_plot <- volcano +
  geom_text_repel(data = highlighted_genes_data, aes(label = Gene),
                box.padding = 0.5, point.padding = 0.2, max.overlaps = Inf,
                segment.size = 0.2)
print(volcano_plot)
ggsave("volcano_plot.png", volcano_plot, width = 10, height = 8, dpi = 1000)

ctrlname = "X2D_4w_DMSO"
treatname = "X2D_4w_MRTX"
dd=ReadBeta(data)
dd_essential = NormalizeBeta(dd, samples=c(ctrlname, treatname), method="cell_cycle")
DensityView(dd_essential, samples=c(ctrlname, treatname))

dd_essential$Control = rowMeans(dd_essential[,ctrlname, drop = FALSE])
dd_essential$Treatment = rowMeans(dd_essential[,treatname, drop = FALSE])

p1 = ScatterView(dd_essential, "Control", "Treatment", groups = c("top", "bottom"), auto_cut_diag = TRUE, display_cut = TRUE)
print(p1)

ggsave("selection.png", p1, width = 10, height = 8, dpi = 300)




 
