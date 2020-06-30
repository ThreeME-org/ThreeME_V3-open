library(scales)
library(readxl)

plot.dir <- "plot_sector"

frmt <- "png" 
for (frmt in formmat_img){
  
  dir.create(str_c(user_path,path_res.plot,frmt,"/", plot.dir,"/"), recursive = TRUE)
  
  label_sectors <- factor(label_sectors, levels = label_sectors)
  
  rep.plot <- list()
  for (sc in scenario){
    rep.plot[[str_c("df_",sc)]] <- read.csv(str_c(user_path,"ThreeME_V3/results/",sc,".csv")) %>% rename(year = "X") %>%
      filter(year >= 2015) 
  }
  
  data_year <- rep.plot[[str_c("df_",sc)]] %>% select(year)
  data_plot <- data.frame()
  for (sc in scenario){
    df.temp <- rep.plot[[str_c("df_",sc)]] %>%
      select(contains("X100..F_L_S")) %>%
      `colnames<-`(str_c(sectors.desc$label_sectors)) %>%  cbind( data_year, "Scenario" = str_c(sc), .) %>% 
      gather(key = sectors, value = value, - year, - Scenario)
    
    data_plot <- rbind(data_plot,df.temp)
  }
  data_plot_1 <- data_plot %>% filter(year == 2020)
  data_plot_2 <- data_plot %>% filter(year == 2040)
  
  pal <- c("#a6a6a6", "#d3d3d3", "#808080", "#2B2B2B")
  
  
  plot_g1 <- ggplot() +
    geom_bar(data =  data_plot_1  ,
             aes(x = factor(sectors, levels= rev(label_sectors)), 
                 y = value/100,
                 fill = Scenario),
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
  
  plot_g2 <- ggplot() +
    geom_bar(data =  data_plot_2  ,
             aes(x = factor(sectors, levels= rev(label_sectors)), 
                 y = value/100,
                 fill = Scenario),
             stat="identity", width = 0.95, position = position_dodge()) + 
    scale_color_manual(values =  pal, aesthetics = "fill") +
    scale_y_continuous(labels = label_percent(accuracy = 1L)) + 
    theme_minimal() +
    theme(
      plot.title = element_text(size = 12, family = police),
      plot.subtitle = element_text(size = 8, family = police),
      legend.text= element_text(size = 14, family = police),
      legend.position = "bottom",
      axis.line = element_line(colour = "gray", 
                               size = 0.3, linetype = "solid"),
      axis.title.x = element_text(size = 16 ,family = police),
      axis.title.y = element_text(size = 12,family = police), 
      axis.text.x =  element_text(size = 12,family = police), 
      axis.text.y = element_blank(),
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
  
  

  plot        <- grid.arrange( 
    plot_g1,plot_g2,
    ncol=2, widths=c(7/10,3/10),
    nrow=1)
  
  
  ggsave(str_c(plot.dir,".",frmt), plot, device = frmt, path = str_c(user_path, path_res.plot,frmt,"/", plot.dir), width = 200 , height = 300 , units = "mm", dpi = 600)
  write.csv(data_plot,str_c(user_path,path_res.plot,frmt,"/", plot.dir,"/",plot.dir,".data.csv"))
}