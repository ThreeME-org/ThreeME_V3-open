plot.dir <- "plot_fig.2.Inv"


for (frmt in format_img){
  
  dir.create(str_c(user_path,path_res.plot,frmt,"/", plot.dir,"/"), recursive = TRUE)
  
  
  for (sc in scenario){
    rep.plot[[str_c("df_",sc)]] <- read.csv(str_c(user_path,"ThreeME_V3/results/",sc,".csv")) %>% rename(year = "X") %>%
      filter(year >= 2015) 
  }
  
  data_plot <- rep.plot[[str_c("df_",sc)]] %>% select(year)
  for (sc in scenario){
    df.temp <- rep.plot[[str_c("df_",sc)]] %>%
      select("X100..I_2.I_0.1.") %>%
      `colnames<-`(str_c( "",sc)) 
    data_plot <- cbind(data_plot, df.temp)
  }
  
  data_plot <- data_plot %>% `colnames<-`(c("year", scenario_label)) %>%  gather(key = scenario, value = value, - year)
  
  plot.4 <- ggplot(data =  data_plot, aes(x = year, y = value/100)) +
    geom_line(aes(linetype = scenario)) +
    scale_linetype_manual(values = line_scenario) +
    scale_y_continuous(labels = label_percent(accuracy = 1L)) + 
    theme_classic() + 
   # theme_minimal() +
    theme(
      plot.title = element_text(size = 12, family = police, hjust = 0.5),
      plot.subtitle = element_text(size = 8, family = police),
      legend.text= element_text(size = 10, family = police),
        #axis.line = element_line(colour = "gray", 
                                 #size = 0.3, linetype = "solid"),
      axis.title.x = element_text(size = 16 ,family = police),
      axis.title.y = element_text(size = 12,family = police), 
      axis.text.x =  element_text( size = 10,family = police),
      axis.text.y = element_text( size = 10, family = police, hjust = 0.5),
      panel.grid  = element_blank(), 
      legend.title = element_blank()
    ) +
    labs(
      title= str_c("Investment"), 
      subtitle =  "" ,
      caption="",
      x= "",
      y= "In %"
    )  
  plot.4
  
  
  ggsave(str_c(plot.dir,".",frmt), plot.4, device = frmt, path = str_c(user_path, path_res.plot,frmt,"/", plot.dir), width = 300 , height = 300 , units = "mm", dpi = 600)
  write.csv(data_plot,str_c(user_path,path_res.plot,frmt,"/", plot.dir,"/",plot.dir,".data.csv"))
}