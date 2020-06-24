library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(extrafont)

font_import() # takes a few minutes
loadfonts(device="postscript")
loadfonts(device = "pdf", quiet = FALSE)


police <- "Arial"

line_scenario <- c("longdash", "twodash", "dotted", "solid")


formmat_img <- c( "png", "pdf")
# pour trouver son user_path
getwd()

user_path <- "/Users/paul/Documents/Professionnel/"
path_plot <- str_c("ThreeME_V3/plots/")
path_res.plot <- str_c("ThreeME_V3/results/plots/")


plot_list <- c("plot_res.1", "plot_res.2", "plot_res.3", "plot_res.4")


for (plot in plot_list)
{
source(str_c(user_path,path_plot,plot,".R"))
}