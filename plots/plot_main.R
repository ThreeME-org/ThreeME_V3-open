library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(extrafont)
library(scales)
library(readxl)

# takes a few minutes
loadfonts(device="postscript")
loadfonts(device = "pdf", quiet = FALSE)


police <- "Calibri"

line_scenario <- c("solid", "twodash", "dotted", "longdash")
scenario <- c("ClimatePol", "Covid", "Covid_LowOil", "Covid_lowOil_ClimatePol")

scenario_label <- c("Climate Pol", "Covid", "Covid-LowOil", "Covid-LowOil-Climate Pol.")
formmat_img <- c( "png", "pdf")

# pour trouver son user_path getwd()
user_path <- str_c("/Users/paul/Documents/Professionnel/")


path_plot <- str_c("ThreeME_V3/plots/")

path_res.plot <- str_c("ThreeME_V3/results/plots/")


plot_list <- c("plot_res.1",
               "plot_res.2",
               "plot_res.3",
               "plot_res.4",
               "plot_res.all",
               "plot_calib",
               "plot_sector.1",
               "plot_sector.2",
               "plot_sector.3",
               "plot_sector.4"
               
               )


source("sector.desc.R")

for (plot in plot_list)
{
source(str_c(user_path,path_plot,plot,".R"))
}