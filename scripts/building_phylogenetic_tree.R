#Building phlogenetic tree
#Packages ----
library(ape)
library(ggtree)
library(dplyr)
library(phytools)
library(DECIPHER)



#Making the lower threshold tree ----
all_lower_newick_tree<- readLines("/cloud/project/data/clustalo/clustalo_all_lower_x_at_tree.txt")

# Join the lines into a single string
all_lower_newick_tree_string <- paste(all_lower_newick_tree, collapse = "")

#Read the Newick-formatted tree and plot
all_lower_tree <- read.tree(text = all_lower_newick_tree_string, format = "newick")
all_lower_tree_plot <- plot(all_lower_tree, cex = 0.7, type="p")  # Adjust cex for label size

#view plot
all_lower_tree_plot

#Making tree for only Mp sequences ----
mp_newick_tree <- readLines("/cloud/project/data/clustalo/clustalo_mp_tree.txt")

#join the lines into a single string
mp_newick_tree_string <- paste(mp_newick_tree, collapse = "")

#Read the newick-formatted tree and plot
mp_tree <- read.tree(text = mp_newick_tree_string, format = "newick")
mp_tree_plot <- plot(mp_tree, cex = 0.7, type="p") #Adjust cex for label size

#view plot
mp_tree_plot



