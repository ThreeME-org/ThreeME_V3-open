library(scales)
library(readxl)

plot.dir <- "plot_calib.1"

frmt <- "png" 
for (frmt in formmat_img){
  
  dir.create(str_c(user_path,path_res.plot,frmt,"/", plot.dir,"/"), recursive = TRUE)

  
  label_sectors <- c("Agriculture, forestry and fishing",
                     "Mining and quarrying, energy, water,\n waste management and remediation",
                     "Manufacture of food, beverages \n and tobacco products",
                     "Coking and refining",
                     "Manufacture of electrical, electronic and \n computer equipment; machinery manufacturing",
                     "Transport equipment manufacturing",
                     "Manufacture of other industrial products",
                     "Construction",
                     "Trade ; repair of motor vehicles and motorcycles",
                     "Transport and storage",
                     "Accommodation and catering",
                     "Information and communication",
                     "Financial and insurance activities",
                     "Real estate activities",
                     "Specialized, scientific and technical activities \n and administrative and support service activities",
                     "Public administration, education,\n human health and social work",
                     "Other service activities")
  label_sectors <- factor(label_sectors, levels = label_sectors)
  
  
  data_plot <- read_xls(str_c(user_path,"ThreeME_V3/data/France/scenarii.xls"), "calibration",range = "B2:D19", col_names = TRUE) %>% 
    cbind(label_sectors,.) %>% 
    `colnames<-`(c("Products", "Households Consumption", "Investment", "Exportations")) %>% gather( key = shock, value = value, - Products)

  pal <- c("#d3d3d3", "#808080", "#2B2B2B")
  
  plot_g <- ggplot() +
    geom_bar(data =  data_plot  ,
             aes(x = factor(Products, levels= rev(label_sectors)), 
                 y = value,
                 fill = shock),
             stat="identity", width = 0.95, position = position_dodge()) + 
    scale_color_manual(values =  pal, aesthetics = "fill") +
    scale_y_continuous(labels = label_percent(accuracy = 1L)) + 
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, family = police),
      plot.subtitle = element_text(size = 8, family = police),
      legend.text= element_text(size = 14, family = police),
      legend.position = "bottom",
      axis.title.x = element_text(size = 16 ,family = police),
      axis.title.y = element_text(size = 12,family = police), 
      axis.text.x =  element_text( size = 12,family = police),
      axis.text.y = element_text( size = 12, family = police, hjust = 0),
      panel.grid  = element_blank(), 
      legend.title = element_blank()
    ) +
    labs(
      title= str_c(""), #Goods and services demand variation between the Covid scenarios and the baseline scenario, in 2020
      subtitle =  "" ,
      caption="",
      x= "",
      y= ""
    )  + 
    coord_flip()
  
  
  
  plot        <- grid_arrange_shared_legend( 
    plot_g,
    ncol=1, widths=c(1),
    nrow=1)
  
  
  ggsave(str_c(plot.dir,".",frmt), plot, device = frmt, path = str_c(user_path, path_res.plot,frmt,"/", plot.dir), width = 200 , height = 300 , units = "mm", dpi = 600)
  write.csv(data_plot,str_c(user_path,path_res.plot,frmt,"/", plot.dir,"/",plot.dir,".data.csv"))
}