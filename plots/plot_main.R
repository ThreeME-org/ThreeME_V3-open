library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(extrafont)
library(scales)
library(readxl)

font_import()
fonts()
  #takes a few minutes
  loadfonts(device="postscript")
  loadfonts(device = "pdf", quiet = FALSE)
  
  # Palette of colors 
  pal <- sequential_hcl(n = 4, h = 0, c = c(0, NA, NA), l = c(20, 70), power = 1.2, register = )
  
  # Police used in the plots
  police <- "Calibri"
  
  line_scenario <- c("solid", "dotted", "twodash", "longdash")
  scenario <- c("ClimatePol", "Covid", "Covid_LowOil", "Covid_lowOil_ClimatePol")
  scenario_label <- c("Pol Clim.", "COVID", "COVID & PétroleBas", "COVID & PétroleBas & Pol Clim.")
  #scenario_label <- c("Climate Pol", "Covid", "Covid-LowOil", "Covid-LowOil-Climate Pol.")
  
  df_SC <- cbind("scenario"= scenario,"label" = scenario_label) %>% data.frame(., stringsAsFactors = FALSE)
  
  
  format_img <- c( "png", "pdf")
  
  # pour trouver son user_path getwd()
  user_path <- str_c("/Users/paul/Documents/Professionnel/")
  path_plot <- str_c("ThreeME_V3/plots/")
  path_res.plot <- str_c("ThreeME_V3/results/plots/")
  
  source(str_c(user_path,path_plot, "sector.desc.R"))
  
  
  
  # function 
  grid_arrange_shared_legend <- function(...) {
    plots <- list(...)
    g <- ggplotGrob(plots[[1]] + theme(legend.key.width = unit(2.5, "line"),
                                       legend.position="bottom"))$grobs
    legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
    lheight <- sum(legend$height)
    grid.arrange(
      do.call(arrangeGrob, lapply(plots, function(x)
        x + theme(legend.position="none"))),
      legend,
      ncol = 1,
      heights = unit.c(unit(1, "npc") - lheight, lheight))
  }
  
  sectors.desc$label_sectors_FR
  
  plot_list <- c(
    "plot_res.1",
                 "plot_res.2",
               "plot_res.3",
                 "plot_res.4",
                 "plot_res.all"
       #          "plot_calib",
        #         "plot_sector.1",
         #        "plot_sector.2",
          #       "plot_sector.3",
           #      "plot_sector.4"
                 )
  
  
  
  for (plot in plot_list)
  {
  source(str_c(user_path,path_plot,plot,".R"))
  }