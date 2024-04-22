#Gene Table
#Packages
library(tidyverse)
library(readxl)
library(dplyr)
gene_table <- read_excel("/cloud/project/data/mp_gene_table.xlsx")


#creating summary table of sequences

gene_table

# Assign protein_family based on substrings
gene_table <- gene_table %>%
  mutate("Protein Family" = case_when(
    grepl("CML", `Gene Name`) ~ "CML",
    grepl("CaM", `Gene Name`) ~ "CaM",
    grepl("CBL", `Gene Name`) ~ "CBL",
    TRUE ~ "Other"
  ))

gene_table_counts <- gene_table %>%
  count(`Protein Family`)

