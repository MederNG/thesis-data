#run all the necessary libraries
library(readxl)
library(data.table)
library(tidyverse)
library(openxlsx)
library(xlsx)
library(rvest)


#read the excel files
setwd("C:/Users/meder/OneDrive/Desktop/Thesis docs/inputs/")
genes_info <- read_excel("genes info.xlsx")
p53 <- read_excel("p53.xlsx")
CELL_CYCLE <- read_excel("CELL_CYCLE.xlsx")
CELL_CYCLE_CHECKPOINT_SIGNALING <- read_excel("CELL_CYCLE_CHECKPOINT_SIGNALING.xlsx")
CELL_CYCLE_G1_S_PHASE_TRANSITION <- read_excel("CELL_CYCLE_G1_S_PHASE_TRANSITION.xlsx")
CELL_CYCLE_G2_M_PHASE_TRANSITION <- read_excel("CELL_CYCLE_G2_M_PHASE_TRANSITION.xlsx")
CYCLIN_DEPENDENT_PROTEIN_KINASE_ACTIVITY <- read_excel("CYCLIN_DEPENDENT_PROTEIN_KINASE_ACTIVITY.xlsx")
REGULATION_OF_CELL_CYCLE <- read_excel("REGULATION_OF_CELL_CYCLE.xlsx")
REGULATION_OF_MAP_KINASE_ACTIVITY <- read_excel("REGULATION_OF_MAP_KINASE_ACTIVITY.xlsx")
REGULATION_OF_RAS_PROTEIN_SIGNAL_TRANSDUCTION <- read_excel("REGULATION_OF_RAS_PROTEIN_SIGNAL_TRANSDUCTION.xlsx")
common_genes <-read_excel("common_genes.xlsx")
common_genes <- common_genes[, -1]

#merge scraped data table with genes info and the list of common genes with b-scores
final_table <- merge(common_genes, genes_info)

# Create a new column in final_table called "p53_patway"
final_table$p53_patway <- ""

# Loop through each gene in the "Gene" column of final_table
for (gene in final_table$Gene) {
  # Check if the gene is present in any column of p53
  if (gene %in% colnames(p53)) {
    # If it is, add a "+" to the "p53_patway" column of final_table
    final_table$p53_patway[final_table$Gene == gene] <- "yes"
  } else {
    # If it isn't, add a "-" to the "p53_patway" column of final_table
    final_table$p53_patway[final_table$Gene == gene] <- "no"
  }
}

# Create a new column in final_table called "CELL_CYCLE"
final_table$CELL_CYCLE <- ""

# Loop through each gene in the "Gene" column of final_table
for (gene in final_table$Gene) {
  # Check if the gene is present in any column of CELL_CYCLE
  if (gene %in% colnames(CELL_CYCLE)) {
    # If it is, add a "+" to the "CELL_CYCLE" column of final_table
    final_table$CELL_CYCLE[final_table$Gene == gene] <- "yes"
  } else {
    # If it isn't, add a "-" to the "CELL_CYCLE" column of final_table
    final_table$CELL_CYCLE[final_table$Gene == gene] <- "no"
  }
}

# Create a new column in final_table called "CELL_CYCLE_CHECKPOINT_SIGNALING"
final_table$CELL_CYCLE_CHECKPOINT_SIGNALING <- ""

# Loop through each gene in the "Gene" column of final_table
for (gene in final_table$Gene) {
  # Check if the gene is present in any column of ÑELL_CYCLE_CHECKPOINT_SIGNALING
  if (gene %in% colnames(CELL_CYCLE_CHECKPOINT_SIGNALING)) {
    # If it is, add a "+" to the "ÑELL_CYCLE_CHECKPOINT_SIGNALING" column of final_table
    final_table$CELL_CYCLE_CHECKPOINT_SIGNALING[final_table$Gene == gene] <- "yes"
  } else {
    # If it isn't, add a "-" to the "ÑELL_CYCLE_CHECKPOINT_SIGNALING" column of final_table
    final_table$CELL_CYCLE_CHECKPOINT_SIGNALING[final_table$Gene == gene] <- "no"
  }
}

# Create a new column in final_table called "CELL_CYCLE_G1_S_PHASE_TRANSITION"
final_table$CELL_CYCLE_G1_S_PHASE_TRANSITION <- ""

# Loop through each gene in the "Gene" column of final_table
for (gene in final_table$Gene) {
  # Check if the gene is present in any column of CELL_CYCLE_G1_S_PHASE_TRANSITION
  if (gene %in% colnames(CELL_CYCLE_G1_S_PHASE_TRANSITION)) {
    # If it is, add a "+" to the "CELL_CYCLE_G1_S_PHASE_TRANSITION" column of final_table
    final_table$CELL_CYCLE_G1_S_PHASE_TRANSITION[final_table$Gene == gene] <- "yes"
  } else {
    # If it isn't, add a "-" to the "CELL_CYCLE_G1_S_PHASE_TRANSITION" column of final_table
    final_table$CELL_CYCLE_G1_S_PHASE_TRANSITION[final_table$Gene == gene] <- "no"
  }
}

# Create a new column in final_table called "CELL_CYCLE_G2_M_PHASE_TRANSITION"
final_table$CELL_CYCLE_G2_M_PHASE_TRANSITION <- ""

# Loop through each gene in the "Gene" column of final_table
for (gene in final_table$Gene) {
  # Check if the gene is present in any column of CELL_CYCLE_G2_M_PHASE_TRANSITION
  if (gene %in% colnames(CELL_CYCLE_G2_M_PHASE_TRANSITION)) {
    # If it is, add a "+" to the "CELL_CYCLE_G2_M_PHASE_TRANSITION" column of final_table
    final_table$CELL_CYCLE_G2_M_PHASE_TRANSITION[final_table$Gene == gene] <- "yes"
  } else {
    # If it isn't, add a "-" to the "CELL_CYCLE_G2_M_PHASE_TRANSITION" column of final_table
    final_table$CELL_CYCLE_G2_M_PHASE_TRANSITION[final_table$Gene == gene] <- "no"
  }
}

# Create a new column in final_table called "CYCLIN_DEPENDENT_PROTEIN_KINASE_ACTIVITY"
final_table$CYCLIN_DEPENDENT_PROTEIN_KINASE_ACTIVITY <- ""

# Loop through each gene in the "Gene" column of final_table
for (gene in final_table$Gene) {
  # Check if the gene is present in any column of CYCLIN_DEPENDENT_PROTEIN_KINASE_ACTIVITY
  if (gene %in% colnames(CYCLIN_DEPENDENT_PROTEIN_KINASE_ACTIVITY)) {
    # If it is, add a "+" to the "CYCLIN_DEPENDENT_PROTEIN_KINASE_ACTIVITY" column of final_table
    final_table$CYCLIN_DEPENDENT_PROTEIN_KINASE_ACTIVITY[final_table$Gene == gene] <- "yes"
  } else {
    # If it isn't, add a "-" to the "CYCLIN_DEPENDENT_PROTEIN_KINASE_ACTIVITY" column of final_table
    final_table$CYCLIN_DEPENDENT_PROTEIN_KINASE_ACTIVITY[final_table$Gene == gene] <- "no"
  }
}

# Create a new column in final_table called "REGULATION_OF_CELL_CYCLE"
final_table$REGULATION_OF_CELL_CYCLE <- ""

# Loop through each gene in the "Gene" column of final_table
for (gene in final_table$Gene) {
  # Check if the gene is present in any column of REGULATION_OF_CELL_CYCLE
  if (gene %in% colnames(REGULATION_OF_CELL_CYCLE)) {
    # If it is, add a "+" to the "REGULATION_OF_CELL_CYCLE" column of final_table
    final_table$REGULATION_OF_CELL_CYCLE[final_table$Gene == gene] <- "yes"
  } else {
    # If it isn't, add a "-" to the "REGULATION_OF_CELL_CYCLE" column of final_table
    final_table$REGULATION_OF_CELL_CYCLE[final_table$Gene == gene] <- "no"
  }
}

# Create a new column in final_table called "REGULATION_OF_MAP_KINASE_ACTIVITY"
final_table$REGULATION_OF_MAP_KINASE_ACTIVITY <- ""

# Loop through each gene in the "Gene" column of final_table
for (gene in final_table$Gene) {
  # Check if the gene is present in any column of REGULATION_OF_MAP_KINASE_ACTIVITY
  if (gene %in% colnames(REGULATION_OF_MAP_KINASE_ACTIVITY)) {
    # If it is, add a "+" to the "REGULATION_OF_MAP_KINASE_ACTIVITY" column of final_table
    final_table$REGULATION_OF_MAP_KINASE_ACTIVITY[final_table$Gene == gene] <- "yes"
  } else {
    # If it isn't, add a "-" to the "REGULATION_OF_MAP_KINASE_ACTIVITY" column of final_table
    final_table$REGULATION_OF_MAP_KINASE_ACTIVITY[final_table$Gene == gene] <- "no"
  }
}

# Create a new column in final_table called "REGULATION_OF_RAS_PROTEIN_SIGNAL_TRANSDUCTION"
final_table$REGULATION_OF_RAS_PROTEIN_SIGNAL_TRANSDUCTION <- ""

# Loop through each gene in the "Gene" column of final_table
for (gene in final_table$Gene) {
  # Check if the gene is present in any column of REGULATION_OF_RAS_PROTEIN_SIGNAL_TRANSDUCTION
  if (gene %in% colnames(REGULATION_OF_RAS_PROTEIN_SIGNAL_TRANSDUCTION)) {
    # If it is, add a "+" to the "REGULATION_OF_RAS_PROTEIN_SIGNAL_TRANSDUCTION" column of final_table
    final_table$REGULATION_OF_RAS_PROTEIN_SIGNAL_TRANSDUCTION[final_table$Gene == gene] <- "yes"
  } else {
    # If it isn't, add a "-" to the "REGULATION_OF_RAS_PROTEIN_SIGNAL_TRANSDUCTION" column of final_table
    final_table$REGULATION_OF_RAS_PROTEIN_SIGNAL_TRANSDUCTION[final_table$Gene == gene] <- "no"
  }
}
#kinase database from the website
url <- "http://www.kinhub.org/kinases.html"

page <- read_html(url)

table <- html_table(html_nodes(page, "table")[[1]])

genes <- table
kinases <- data.frame(genes)

# Create a new column in final_table called "Kinase"
final_table$Kinase <- ""

# Extract all unique kinase values
kinase_values <- unique(unlist(genes))

# Loop through each gene in the "Gene" column of final_table
for (gene in final_table$Gene) {
  # Check if the gene is present in any of the kinase values
  if (gene %in% kinase_values) {
    # If it is, add a "+" to the "Kinase" column of final_table
    final_table$Kinase[final_table$Gene == gene] <- "yes"
  } else {
    # If it isn't, add a "-" to the "Kinase" column of final_table
    final_table$Kinase[final_table$Gene == gene] <- "no"
  }
}
final_table <- final_table %>% 
  mutate(drugs = NA_character_)

final_table[1, "drugs"] <- '=HYPERLINK("https://www.genecards.org/cgi-bin/carddisp.pl?gene="&B2&"#drugs_compounds";"drugs list")'

#set a new directory and write the updated final_table to a new Excel file
setwd("C:/Users/meder/OneDrive/Desktop/Thesis docs/outputs/")
write.xlsx(final_table, "final_table_2D_bottom50_DMSO.xlsx")
