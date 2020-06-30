library(scales)
library(readxl)
library(colorspace)

plot.dir <- "plot_sector.2"


pal <- sequential_hcl(n = 4, h = 0, c = c(0, NA, NA), l = c(30, 90), power = 1.5, register = )

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
      select(contains("..I_S")) %>%
      `colnames<-`(str_c(sectors.desc$label_sectors)) %>%  cbind( data_year, "Scenario" = str_c(sc), .) %>% 
      gather(key = sectors, value = value, - year, - Scenario)
    
    data_plot <- rbind(data_plot,df.temp)
  }
  data_plot <- data_plot %>% filter(year == 2040)
  
  
  
  plot_g <- ggplot() +
    geom_bar(data =  data_plot  ,
             aes(x = factor(sectors, levels= rev(label_sectors)), 
                 y = value/100,
                 fill = Scenario),
             stat="identity", width = 0.95, position = position_dodge()) + 
    geom_hline(aes(yintercept = 0),color = "black", linetype= "solid", size = 0.1) +
    scale_color_manual(values =  rev(pal), aesthetics = "fill") +
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
      title= str_c(""), #Employment variation in % deviation to the baseline across sectors in 2040 
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