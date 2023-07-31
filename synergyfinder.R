library("tidyverse")
library("synergyfinder")
library(readxl)


getwd()
setwd("C:/Users/meder/OneDrive/Desktop/Thesis docs")
read_excel("synergy for R.xlsx")
data_synergy<-read_excel("synergy for R.xlsx")
head(data_synergy)

res <- ReshapeData(
  data = data_synergy,
  data_type = "viability",
  impute = TRUE,
  impute_method = NULL,
  noise = TRUE,
  seed = 1)
str(res)

res <- CalculateSynergy(
  data = res,
  method = c("ZIP", "HSA", "Bliss", "Loewe"),
  Emin = NA,
  Emax = NA,
  correct_baseline = "non")
res$drug_pairs
str(res$synergy_scores)

res <- CalculateSensitivity(
  data = res,
  correct_baseline = "non"
)
sensitive_columns <- c(
  "block_id", "drug1", "drug2",
  "ic50_1", "ic50_2",
  "ri_1", "ri_2",
  "css1_ic502", "css2_ic501", "css")
res$drug_pairs[, sensitive_columns]

data_2d<-Plot2DrugContour(
  data = res,
  plot_block = 1,
  drugs = c(1, 2),
  plot_value = "Bliss_synergy",
  dynamic = FALSE,
  summary_statistic = c("quantile_25", "quantile_75")
)

data_3d <-Plot2DrugSurface(
  data = res,
  plot_block = 1,
  drugs = c(1, 2),
  plot_value = "Bliss_synergy",
  dynamic = FALSE,
  summary_statistic = c("mean", "quantile_25", "median", "quantile_75")
)
data_3d 
