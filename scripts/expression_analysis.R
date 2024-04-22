#Analysing expression data
#Packages ----
library(tidyverse)
library(dplyr)
library(readxl)
library(patchwork)

#Reading in data ----
# Specify the path to your Excel file
excel_file_path <- '/cloud/project/data/expression_data/1000_expression_data.xlsx'

# Get the sheet names
sheet_names <- excel_sheets(excel_file_path)

# Loop through each sheet, read it into a data frame, and assign it dynamically
for (sheet_name in sheet_names) {
  # Read the sheet into a data frame
  df <- read_excel(excel_file_path, sheet = sheet_name)
  
  # Remove any spaces in the sheet name and convert to a valid variable name
  df_name <- make.names(sheet_name)
  
  # Assign the data frame to the variable with the sheet name
  assign(df_name, df)
}

#Created data frames for all sheets

#Renaming dataframes to gene names ----

# Get the sheet names
sheet_names <- excel_sheets(excel_file_path)

# Print the actual sheet names to check
print(sheet_names)

# Specify the range of sheet numbers that you have (e.g., 1 through 63)
sheet_numbers <- 1:61

new_names <- c("Mp1g00710.1", "Mp1g00920.1", "Mp1g03300.1", "Mp1g04860.1", "Mp1g13580.1", "Mp1g16520.1", "Mp1g18460.1", "Mp1g22210.1", "Mp1g23280.1", "Mp1g27470.1", "Mp1g27830.1", "Mp1g28200.1", "Mp2g03100.1", "Mp2g07750.1", "Mp2g14920.1", "Mp2g14930.1", "Mp2g14950.1", "Mp2g14960.1", "Mp3g02440.1", "Mp3g03110.1", "Mp3g03120.1", "Mp3g09930.1", "Mp3g14560.1", "Mp3g16360.1", "Mp3g19130.1", "Mp3g20340.1", "Mp3g22080.1", "Mp4g00900.1", "Mp4g01220.1", "Mp4g01600.1", "Mp4g13490.1", "Mp4g19490.1", "Mp4g19500.1", "Mp4g22970.1", "Mp5g13250.1", "Mp5g13430.1", "Mp5g15300.1", "Mp5g15310.1", "Mp5g15630.1", "Mp5g15820.1", "Mp5g19810.2", "Mp5g19860.1", "Mp5g22000.1", "Mp5g24400.1", "Mp6g03680.1", "Mp6g06060.1", "Mp6g09720.1", "Mp6g10070.1", "Mp6g11320.1", "Mp6g16010.1", "Mp6g18000.1", "Mp6g20440.1", "Mp6g20450.1", "Mp6g20470.1", "Mp8g01550.1", "Mp8g02300.1", "Mp8g02310.1", "Mp8g04190.1", "Mp8g07910.1", "Mp8g12640.1", "MpVg01160.1")

original_names <- paste0("Sheet", 1:61)

# Create a list to store the renamed data frames
renamed_dfs <- lapply(seq_along(original_names), function(i) {
  # Extract the original data frame
  original_df <- get(original_names[i])
  
  # Assign the data frame to the new name
  assign(new_names[i], original_df, envir = .GlobalEnv)
  
  # Return the renamed data frame
  return(original_df)
})


#Creating longformatdataframes for all dataframes ----

# List of dataframes (assuming you have them loaded)
list_of_dataframes <- list(Mp1g00710.1, Mp1g00920.1, Mp1g03300.1, Mp1g04860.1, Mp1g13580.1, Mp1g16520.1, Mp1g18460.1, Mp1g22210.1, Mp1g23280.1, Mp1g27470.1, Mp1g27830.1, Mp1g28200.1, Mp2g03100.1, Mp2g07750.1, Mp2g14920.1, Mp2g14930.1, Mp2g14930.1, Mp2g14960.1, Mp3g02440.1, Mp3g03110.1, Mp3g03120.1, Mp3g09930.1, Mp3g14560.1, Mp3g16360.1, Mp3g19130.1, Mp3g20340.1, Mp3g22080.1, Mp4g00900.1, Mp4g01220.1, Mp4g01600.1, Mp4g13490.1, Mp4g19490.1, Mp4g19500.1, Mp4g22970.1, Mp5g13250.1, Mp5g13430.1, Mp5g15300.1, Mp5g15310.1, Mp5g15630.1, Mp5g15820.1, Mp5g19810.2, Mp5g19860.1, Mp5g22000.1, Mp5g24400.1, Mp6g03680.1, Mp6g06060.1, Mp6g09720.1, Mp6g10070.1, Mp6g11320.1, Mp6g16010.1, Mp6g18000.1, Mp6g20440.1, Mp6g20450.1, Mp6g20470.1, Mp8g01550.1, Mp8g02300.1, Mp8g02310.1, Mp8g04190.1, Mp8g07910.1, Mp8g12640.1, MpVg01160.1)

# Clean column names for all dataframes in the list
# Loop through the list of dataframes
for (i in seq_along(list_of_dataframes)) {
  df <- list_of_dataframes[[i]]
  df <- df %>%
    janitor::clean_names() %>%
    select(tissue, expression_level) %>%
    pivot_wider(names_from = "tissue", values_from = "expression_level")%>%
    janitor::clean_names()
  
  # Update the dataframe in the list
  list_of_dataframes[[i]] <- df
}

#Compiling gene entries ----

# Combine dataframes into one and add "gene" column
combined_dataframe <- bind_rows(list_of_dataframes)

# Create a vector for gene names
combined_dataframe$gene <- c(
  "Mp1g00710.1", "Mp1g00920.1", "Mp1g03300.1", "Mp1g04860.1", "Mp1g13580.1", 
  "Mp1g16520.1", "Mp1g18460.1", "Mp1g22210.1", "Mp1g23280.1", "Mp1g27470.1", 
  "Mp1g27830.1", "Mp1g28200.1", "Mp2g03100.1", "Mp2g07750.1", "Mp2g14920.1", 
  "Mp2g14930.1", "Mp2g14950.1", "Mp2g14960.1", "Mp3g02440.1", "Mp3g03110.1", 
  "Mp3g03120.1", "Mp3g09930.1", "Mp3g14560.1", "Mp3g16360.1", "Mp3g19130.1", 
  "Mp3g20340.1", "Mp3g22080.1", "Mp4g00900.1", "Mp4g01220.1", "Mp4g01600.1", 
  "Mp4g13490.1", "Mp4g19490.1", "Mp4g19500.1", "Mp4g22970.1", "Mp5g13250.1", 
  "Mp5g13430.1", "Mp5g15300.1", "Mp5g15310.1", "Mp5g15630.1", "Mp5g15820.1", 
  "Mp5g19810.2", "Mp5g19860.1", "Mp5g22000.1", "Mp5g24400.1", "Mp6g03680.1", 
  "Mp6g06060.1", "Mp6g09720.1", "Mp6g10070.1", "Mp6g11320.1", "Mp6g16010.1", 
  "Mp6g18000.1", "Mp6g20440.1", "Mp6g20450.1", "Mp6g20470.1", "Mp8g01550.1", 
  "Mp8g02300.1", "Mp8g02310.1", "Mp8g04190.1", "Mp8g07910.1", "Mp8g12640.1", 
  "MpVg01160.1")

combined_dataframe <- mutate(combined_dataframe, 
                             gene = c(
                               "Mp1g00710.1", "Mp1g00920.1", "Mp1g03300.1", "Mp1g04860.1", "Mp1g13580.1", 
                               "Mp1g16520.1", "Mp1g18460.1", "Mp1g22210.1", "Mp1g23280.1", "Mp1g27470.1", 
                               "Mp1g27830.1", "Mp1g28200.1", "Mp2g03100.1", "Mp2g07750.1", "Mp2g14920.1", 
                               "Mp2g14930.1", "Mp2g14950.1", "Mp2g14960.1", "Mp3g02440.1", "Mp3g03110.1", 
                               "Mp3g03120.1", "Mp3g09930.1", "Mp3g14560.1", "Mp3g16360.1", "Mp3g19130.1", 
                               "Mp3g20340.1", "Mp3g22080.1", "Mp4g00900.1", "Mp4g01220.1", "Mp4g01600.1", 
                               "Mp4g13490.1", "Mp4g19490.1", "Mp4g19500.1", "Mp4g22970.1", "Mp5g13250.1", 
                               "Mp5g13430.1", "Mp5g15300.1", "Mp5g15310.1", "Mp5g15630.1", "Mp5g15820.1", 
                               "Mp5g19810.2", "Mp5g19860.1", "Mp5g22000.1", "Mp5g24400.1", "Mp6g03680.1", 
                               "Mp6g06060.1", "Mp6g09720.1", "Mp6g10070.1", "Mp6g11320.1", "Mp6g16010.1", 
                               "Mp6g18000.1", "Mp6g20440.1", "Mp6g20450.1", "Mp6g20470.1", "Mp8g01550.1", 
                               "Mp8g02300.1", "Mp8g02310.1", "Mp8g04190.1", "Mp8g07910.1", "Mp8g12640.1", 
                               "MpVg01160.1"))

#Moving gene coloumn to the front
combined_dataframe <- combined_dataframe %>%
  select(gene, everything())

# Print the resulting combined dataframe
print(combined_dataframe)

#Adding gene names to combined dataframe ----
# Define the variables
gene_name_column <- c(
  "N/A", "MpCaM", "MpCML1", "MpCML2", "MpCML3",
  "MpCML4", "MpCML5", "MpCML6", "N/A", "N/A",
  "MpCML7", "MpCML8", "MpCML9", "MpCBL-A", "MpCML10",
  "MpCML11", "MpCML12", "MpCML13", "N/A", "MpCML14",
  "MpCML15", "MpCML16", "N/A", "MpCML17", "MpCML18",
  "N/A", "N/A", "MpCBL-B", "MpCML19", "N/A",
  "N/A", "MpCML20", "MpCML21", "MpCML22", "MpCML23",
  "N/A", "MpCML24", "MpCML25", "MpCML26", "MpCML27",
  "MpCBL-C", "MpCBL-D", "MpCML28", "MpCML29", "MpCML30",
  "MpCML31", "N/A", "MpCML32", "MpCML33", "MpCML34",
  "MpCML35", "MpCML36", "MpCML37", "MpCML38", "N/A",
  "MpCML39", "MpCML40", "MpCML41", "MpCML42", "MpCML43", "N/A"
)

# Add the new column to the combined dataframe
combined_dataframe <- cbind(combined_dataframe, gene_name = gene_name_column)

# Print the new data frame
print(combined_dataframe)

#moving gene name after gene id
combined_dataframe <- combined_dataframe %>%
  select(gene, gene_name, everything())

# Assign protein_family based on substrings
combined_dataframe <- combined_dataframe %>%
  mutate(protein_family = case_when(
    grepl("CML", gene_name) ~ "CML",
    grepl("CaM", gene_name) ~ "CaM",
    grepl("CBL", gene_name) ~ "CBL",
    TRUE ~ "Other"
  ))


#Manually check that order has remianed consistent
#Order is consistent

#Creating more specific dataframes ----
#Creating dataframe with only tissue expression data single evironmental stress or diurnal data
tissue_expression_data <- combined_dataframe %>%
  select(gene, gene_name, protein_family, antheridium_male, thallus, gemmaling, gemma_cup_unspecified, sporeling, sperm, antheridiophore_male, gemma_cup_male, mid_rib_male, archegoniophore_female)

stress_expression_data <- combined_dataframe %>%
  select(gene, gene_name, protein_family, abiotic_stress_salt_single_stress, abiotic_stress_mannitol_single_stress, abiotic_stress_kno3_deficiency_single_stress, abiotic_stress_cold_single_stress, abiotic_stress_light_single_stress, abiotic_stress_dark_single_stress, abiotic_stress_heat_single_stress)

diurnal_expression_data <- combined_dataframe %>%
  select(gene, gene_name, protein_family, diurnal_zt2, diurnal_zt6, diurnal_zt10, diurnal_zt14, diurnal_zt18, diurnal_zt22)

#Changing Coloumn names
tissue_expression_data <- tissue_expression_data %>%
  rename("Antheridium"="antheridium_male",
         "Thallus"="thallus",
         "Gemmaling"="gemmaling",
         "Gemma Cup"="gemma_cup_unspecified",
         "Sporeling"="sporeling",
         "Sperm"="sperm",
         "Antheridiophore"="antheridiophore_male",
         "Gemma Cup Male"="gemma_cup_male",
         "Mid Rib"="mid_rib_male",
         "Archegoniophore"="archegoniophore_female")

stress_expression_data <- stress_expression_data %>%
  rename("Salt Stress"="abiotic_stress_salt_single_stress",
         "Mannitol Stress"="abiotic_stress_mannitol_single_stress",
         "Kno3 Stress"="abiotic_stress_kno3_deficiency_single_stress",
         "Cold Stress"="abiotic_stress_cold_single_stress",
         "Light Stress"="abiotic_stress_light_single_stress",
         "Dark Stress"="abiotic_stress_dark_single_stress",
         "Heat Stress"="abiotic_stress_heat_single_stress")

names(diurnal_expression_data) <- sub("^diurnal_", "", names(diurnal_expression_data))

#making diurnal names uppercase
# Store column names that should not be converted to uppercase
excluded_columns <- c("gene", "gene_name", "protein_family")

# Convert column names to uppercase except for excluded columns
colnames(diurnal_expression_data)[!colnames(diurnal_expression_data) %in% excluded_columns] <- toupper(colnames(diurnal_expression_data)[!colnames(diurnal_expression_data) %in% excluded_columns])


#Reordering variables
stress_expression_data <- stress_expression_data %>%
  select("gene",
         "gene_name",
         "protein_family",
         "Kno3 Stress",
         "Salt Stress",
         "Mannitol Stress",
         "Dark Stress",
         "Light Stress",
         "Cold Stress",
         "Heat Stress")

tissue_expression_data <- tissue_expression_data %>%
  select("gene",
         "gene_name",
         "protein_family",
         "Antheridium",
         "Antheridiophore",
         "Sperm",
         "Archegoniophore",
         "Gemma Cup Male",
         "Gemma Cup",
         "Gemmaling",
         "Sporeling",
         "Mid Rib",
         "Thallus")

#Creating visualisation for expression data.
#Create seperate heatmaps for tissue and enviromental stress data

#Arranging dataframes based on protein family ----
tissue_expression_data <- tissue_expression_data %>%
  arrange(protein_family)


stress_expression_data <- stress_expression_data %>%
  arrange(protein_family)


diurnal_expression_data <- diurnal_expression_data %>%
  arrange(protein_family)

#excluding erroneous proteins containing non-ef-hand domains from heatmap
tissue_expression_data <- tissue_expression_data %>%
  filter(`gene_name` != "N/A")

stress_expression_data <- stress_expression_data %>%
  filter(`gene_name` != "N/A")


diurnal_expression_data <- diurnal_expression_data %>%
  filter(`gene_name` != "N/A")

#Creating heatmaps----
#Tissue expression heatmap
# Extract the numeric data from combined_dataframe
tissue_heatmap_data <- as.matrix(tissue_expression_data[, -(1:3)])  # Exclude the gene column

# Get the gene names
tissue_gene_names <- tissue_expression_data$gene_name

# Set gene names as row names
rownames(tissue_heatmap_data) <- tissue_gene_names

# Create heatmap
heatmap(tissue_heatmap_data,
        scale = "row",
        Rowv = NA, 
        Colv = NA,
        cexRow-0.8,
        cexCol=0.6)

#Stress expression heatmap
# Extract the numeric data from combined_dataframe
stress_heatmap_data <- as.matrix(stress_expression_data[, -(1:3)])  # Exclude the gene column

# Get the gene names
stress_gene_names <- stress_expression_data$gene_name

# Set gene names as row names
rownames(stress_heatmap_data) <- stress_gene_names

# Create the heatmap
heatmap(stress_heatmap_data,
        scale = "row",
        Rowv = NA, 
        Colv = NA,
        cexRow=0.8,
        cexCol=0.8)

#Diurnal expression heatmap
# Extract the numeric data from combined_dataframe
diurnal_heatmap_data <- as.matrix(diurnal_expression_data[, -(1:3)])  # Exclude the gene column

# Get the gene names
diurnal_gene_names <- diurnal_expression_data$gene_name

# Set gene names as row names
rownames(diurnal_heatmap_data) <- diurnal_gene_names

# Create the heatmap
heatmap(diurnal_heatmap_data,
        scale = "row",
        Rowv = NA, 
        Colv = NA,
        cexRow-0.8,
        cexCol=0.6)


#Creatring diurnal timeline











#NOW NAME PROTEINS AND CREATE TABLE ON FRIDAY 
#ALSO CLUSTER APPRORPTIATE GENES STRESS TREATMENTS AND TISSUES TOGETHER
#ALSO CREATE HEATMAP FOR CIRCADIAN EXPRESSION
#ALSO CREATE PERCENT IDENTITY TABLE

