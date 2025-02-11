plot_effective_R <- function(R_dat, 
                             start_plot_date = NULL, 
                             end_plot_date = NULL) {
  
  start_plot_date = if(is.null(start_plot_date)) min(R_dat$date) else start_plot_date
  end_plot_date = if(is.null(end_plot_date)) max(R_dat$date) else end_plot_date
  
  R_dat |> 
    ggplot(aes(x = date, y = R_mean, col = Rtype, fill = Rtype)) + 
    geom_hline(yintercept = 1, lty = 2, col = "darkgrey") +
    geom_ribbon(aes(ymin = R_lower, ymax = R_upper),
                alpha = 0.3, col = NA) +
    geom_line() +
    scale_x_date(limits = c(start_plot_date, end_plot_date),
                 expand = expansion(c(0, 0)),
                 date_labels = "%b %Y") +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(c(0, 0.05))) +
    scale_color_manual(values = c( "#9ccb86", "#e88471", "#009392")) +
    scale_fill_manual(values = c( "#9ccb86","#e88471", "#009392")) +
    labs(x = NULL,
         y = "Effective reproduction number",
         col = "based on",
         fill = "based on") +
    theme_light() + 
    theme(legend.position = "inside",
          legend.position.inside = c(0.95, 0.95),
          legend.justification = c(1, 1))
  
}
