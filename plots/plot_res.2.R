library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(extrafont)


font_import() # takes a few minutes
loadfonts(device="postscript")
loadfonts(device = "pdf", quiet = FALSE)
police <- "Arial"

getwd()
user_path <- "/Users/paul/Documents/Professionnel/"
path_res.plot <- str_c("ThreeME_V3/results/plots/")
plot.dir <- "plot_res.2"


scenario <- c("TC", "COVID", "COVID_LOWOIL",  "COVID_LOWOIL_TC" )



for (frmt in c( "png", "pdf")){
  
  dir.create(str_c(user_path,path_res.plot,frmt,"/", plot.dir,"/"), recursive = TRUE)
  
  date <-  "2020-06-24_08-29-13"

  rep.plot <- list()
  for (sc in scenario){
    rep.plot[[str_c("df_",sc)]] <- read.csv(str_c(user_path,"ThreeME_V3/results/allresults_",sc,".csv")) %>% rename(year = "X") %>%
      filter(year >= 2015) 
  }
  
  data_plot <- rep.plot[[str_c("df_",sc)]] %>% select(year)
  for (sc in scenario){
    df.temp <- rep.plot[[str_c("df_",sc)]] %>%
      select("X100..EMS_CO2_2.EMS_CO2_0.1.") %>%
      `colnames<-`(str_c( "",sc)) 
    data_plot <- cbind(data_plot, df.temp)
  }
  data_plot <- data_plot %>% gather(key = scenario, value = value, - year)
  
  plot <- ggplot(data =  data_plot, aes(x = year, y = value)) +
    geom_line(aes(linetype = scenario)) +
    scale_linetype_manual(values=c("solid", "twodash", "dotted", "longdash" )) +
    ylim(-40, 20) + 
    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, family = police),
      plot.subtitle = element_text(size = 8, family = police),
      legend.text= element_text(size = 10, family = police),
      #legend.direction = "horizontal",
      #legend.position=c(0.5, 1.05),
      axis.title.x = element_text(size = 16 ,family = police),
      axis.title.y = element_text(size = 12,family = police), 
      axis.text.x =  element_text( size = 10,family = police),
      axis.text.y = element_text( size = 10, family = police, hjust = 0.5),
      panel.grid  = element_blank(), 
      legend.title = element_blank()
    ) +
    labs(
      title= str_c(""), 
      subtitle =  "" ,
      caption="",
      x= "",
      y=" In %"
    )  
  
  
  
  ggsave(str_c(plot.dir,".",frmt), plot, device = frmt, path = str_c(user_path, path_res.plot,frmt,"/", plot.dir), width = 300 , height = 150 , units = "mm", dpi = 600)
  write.csv(data_plot,str_c(user_path,path_res.plot,frmt,"/", plot.dir,"/",plot.dir,".data.csv"))
}