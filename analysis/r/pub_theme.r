# pub theme
col_param <- c("1" = "black","2"="black")
fill_param <- c("1" = "#808080","2" = "white")

pub_theme <- function() {
  theme_grey()+
    theme(
      axis.line = element_line(colour = "black",size = 0.3),
      axis.ticks = element_line(colour = 'black',size = 0.2),
      legend.key = element_blank(),
      legend.text = element_text(size = 10),
      legend.title = element_text(size = 10),
      legend.key.size = unit(0.5, "cm"),
      legend.background = element_blank(),
      panel.spacing.x = unit(1.5, "lines"),
      panel.spacing.y = unit(0.6,"lines"),
      strip.background = element_rect(fill = "white"),
      axis.title.x = element_text(size = 12),
      axis.text.x = element_text(size = 10, margin = unit(c(t = 2.5, r = 0, b = 0, l = 0), "mm")),
      axis.title.y = element_text(size = 12),
      axis.text.y = element_text(size = 10, margin = unit(c(t = 0.1, r = 2.5, b = 0, l = 0), "mm")),
      axis.ticks.length = unit(-1, "mm"),
      plot.margin=margin(0.2,.5, 0, 0.1, "cm"),
      panel.background = element_blank(),
      axis.text = element_text(colour = "black"),
      legend.position = 'none'
    ) 
}

theme_ms <- function(base_size=12, base_family="Helvetica") {
  library(grid)
  (theme_bw(base_size = base_size, base_family = base_family)+
      theme(# text=element_text(color="black"),
            axis.title=element_text(#face="bold",
                                    size = rel(1.3)),
            axis.text=element_text(size = rel(1), color = "black"),
            legend.title=element_text(face="bold"),
            legend.text=element_text(face="bold"),
            legend.background=element_rect(fill="transparent"),
            legend.key.size = unit(0.8, 'lines'),
            panel.border=element_rect(color="black",size=0.5),
            panel.grid=element_blank()
      ))
}
