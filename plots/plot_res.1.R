library(dplyr)
library(ggplot2)
library(stringr)
library(extrafont)


font_import() # takes a few minutes
loadfonts(device="postscript")
loadfonts(device = "pdf", quiet = FALSE)
police <- "Calibri"

user_path <- "C:/Users/PMalliet/Documents/GitHub/"
path_res.plot <- str_c("ThreeME_V3/results/plots/")
plot.dir <- "plot_res.1"


for (frmt in c( "png", "pdf")){
  
  dir.create(str_c(user_path,path_res.plot,frmt,"/", plot.dir,"/"), recursive = TRUE)
  
 date <-  "2020-06-24_08-29-13"

  rep.plot <- list()
  
  rep.plot$df <- read.csv(str_c(user_path,"ThreeME_V3/results/",date,"_allresults.csv")) %>% rename(year = "X")
  
data_plot <- rep.plot$df %>% select(year, contains("GDP_2")) %>% `colnames<-`(c("year", "GDP")) %>% filter(year >= 2015)
  

  
  
  plot <- ggplot() +
    geom_line(data =  data_plot,
             aes(x = year, 
                 y = GDP) ) +

    theme_minimal() +
    theme(
      plot.title = element_text(size = 16, family = "Calibri"),
      plot.subtitle = element_text(size = 8, family = "Calibri"),
      legend.text= element_text(size = 10, family = "Calibri"),
      axis.title.x = element_text(size = 16 ,family = "Calibri"),
      axis.title.y = element_text(size = 12,family = "Calibri"), 
      axis.text.x =  element_text( size = 10,family = "Calibri"),
      axis.text.y = element_text( size = 10, family = "Calibri", hjust = 0.5),
      panel.grid  = element_blank(), 
      legend.title = element_blank()
    ) +
    labs(
      title= str_c(""), 
      subtitle =  "" ,
      caption="ThreeME simulations",
      x= "",
      y=" In % deviation to the baseline"
    )  

  
  
  ggsave(str_c(plot.dir,".",frmt), plot, device = frmt, path = str_c(user_path, path_res.plot,frmt,"/", plot.dir), width = 300 , height = 150 , units = "mm", dpi = 300)
    write.csv(data_plot,str_c(path_res.plot,frmt,"/", plot.dir,"/",plot.dir,".data.csv"))
}