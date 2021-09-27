library(deSolve) # for integrating ODEs
library(tidyverse) # data wrangling and plotting
THRESH <- 10^(-8) # consider species extinct below this threshold

three_colors <- c("black", "#077B8A", "#7B70AF")
five_colors <- c("black", "#077B8A", "#177BA1", "#4A78AE", "#7B70AF") # , "#A366A0" #BF5F85
four_colors <- c("black", "#077B8A", "#177BA1", "#7B70AF") # , "#A366A0" #BF5F85
p2p2_colors <- c("#077B8A","#7B70AF", "black", "darkgrey") 