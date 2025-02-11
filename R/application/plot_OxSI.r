plot_OxSI <- function(OxSI_dat,
                      legend = TRUE,
                      start_plot_date = NULL, 
                      end_plot_date = NULL) {
  
  start_plot_date = if(is.null(start_plot_date)) min(OxSI_dat$date) else start_plot_date
  end_plot_date = if(is.null(end_plot_date)) max(OxSI_dat$date) else end_plot_date
  
  
  # plot SI with dates on x-axis and legend (as standalone figure)
  
  p <- OxSI_dat |> 
    filter(date >= start_plot_date,
           date <= end_plot_date) |> 
    group_by(period) |> 
    reframe(date_min = min(date),
            date_max = max(date),
            OxSI = mean(OxSI)) |> 
    ggplot(aes(xmin = date_min, xmax = date_max, ymin = 0, ymax = 1, fill = OxSI, col = OxSI))+
    geom_rect() +
    scale_x_date(limits = c(start_plot_date, end_plot_date),
                 expand = expansion(c(0, 0)),
                 date_labels = "%b %Y") +
    scale_y_continuous(expand = c(0, 0))+
    scale_fill_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                         na.value = "white")+
    scale_color_gradientn(colors = rev(brewer.pal(11, "RdBu")),
                          na.value = "white")+
    #guides(fill = "none")+
    labs(x = NULL,
         fill = "Stringency Index",
         color = "Stringency Index") +
    theme_minimal() +
    theme(legend.position = "bottom",
          axis.text.y = element_blank(),
          axis.line.y = element_blank(),
          axis.title = element_blank(),
          panel.grid.major = element_blank(),
          #legend.title = element_blank(),
          panel.grid.minor = element_blank())
  
  # plot SI without dates on x-axis and legend (as bar on top of time series of effectiveness)
  
  if(!legend) {
    p <- p +  
      guides(fill = "none",
             color = "none") +
      labs(title = "Stringency Index") +
      theme_minimal() +
      theme(axis.text = element_blank(),
            axis.title = element_blank(),
            plot.title = element_text(hjust = 0.99, vjust=0.5, color = "white", size = 10, margin=margin(t = 0, b = -20)))
   }

  return(p)
  
}
