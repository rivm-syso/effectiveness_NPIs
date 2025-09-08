plot_results <- function(data, 
                         what, # eff, S, Rc, R 
                         waning = NULL,
                         color_scheme, 
                         start_plot_date = NULL, 
                         end_plot_date = NULL) {
  
  start_plot_date = if(is.null(start_plot_date)) min(data$date) else start_plot_date
  end_plot_date = if(is.null(end_plot_date)) max(data$date) else end_plot_date
  
  y_labels <- c("eff" = "Effectiveness",
                "S" = "Effective susceptibility",
                "Rc" = "Counterfactual reproduction number", 
                "R" = "Effective reproduction number") 
  
  p <- data |> 
    filter(waning == waning) |> 
    filter(date >= start_plot_date,
           date <= end_plot_date) |> 
    select(date, waning, starts_with(paste0(what, "_"))) |> 
    pivot_longer(cols = contains(what), names_prefix = paste0(what, "_")) |> 
    pivot_wider(names_from = name, values_from = value) |> 
    ggplot(aes(x = date, y = mean, col = waning, fill = waning)) + 
    geom_ribbon(aes(ymin = lower, ymax = upper),
                alpha = 0.3, col = NA) +
    geom_line() +
    coord_cartesian(ylim = c(if(what == "eff") NA else 0, if(what == "eff") 1 else NA)) +
    scale_x_date(limits = c(start_plot_date, end_plot_date),
                 expand = expansion(c(0, 0)),
                 date_labels = "%b %Y") +
    scale_y_continuous(expand = expansion(c(if(what == "eff") 0.01 else 0, if(what == "eff") 0 else 0.01))) +
    scale_color_manual(values = color_scheme) +
    scale_fill_manual(values = color_scheme) +
    labs(x = NULL,
         y = y_labels[what],
         col = "Waning",
         fill = "Waning") +
    theme_light() + 
    theme(legend.position = "inside",
          legend.position.inside = c(0.95, 0.95),
          legend.justification = c(1, 1))
  
  if(what %in% c("R", "Rc")) p <- p + geom_hline(yintercept = 1, lty = 2, col = "darkgrey")
  
  return(p)
  
}



