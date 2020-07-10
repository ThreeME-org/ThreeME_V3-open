  library(ggplot2)
  library(gridExtra)
  library(grid)
  
  plot.dir <- "plot_res.all"
  
  
  
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
      heights =  unit.c(unit(1, "npc") - lheight, lheight))
  }
  
  
  for (frmt in formmat_img){
    
    dir.create(str_c(user_path,path_res.plot,frmt,"/", plot.dir,"/"), recursive = TRUE)
    
    
    plot        <- grid_arrange_shared_legend( 
      plot.1,plot.2, plot.3, plot.4,
      ncol=2, widths=c(5/10,5/10),
      nrow=3)
    
    
    
    ggsave(str_c(plot.dir,".",frmt), plot, device = frmt, path = str_c(user_path, path_res.plot,frmt,"/", plot.dir), width = 200 , height = 200 , units = "mm", dpi = 600)
   ggsave(str_c(plot.dir,".pdf"), plot, device = cairo_pdf, path = str_c(user_path, path_res.plot,frmt,"/", plot.dir), width = 200 , height = 200 , units = "mm", dpi = 600)
  
     write.csv(data_plot,str_c(user_path,path_res.plot,frmt,"/", plot.dir,"/",plot.dir,".data.csv"))
  }
