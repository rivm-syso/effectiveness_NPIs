plot_fraction_of_infections <- function(data, 
                                        frac, 
                                        color_scheme,
                                        start_plot_date = NULL, 
                                        end_plot_date = NULL) {
  
  start_plot_date = if(is.null(start_plot_date)) min(data$date) else start_plot_date
  end_plot_date = if(is.null(end_plot_date)) max(data$date) else end_plot_date
  
  # add dummy variable type (will be used for faceting outside this function)
  if(!exists("type", where = data)) data <- data |> mutate(type = "")
  
  data |> 
    select(age_group, date, estimate, p, waning, type) |> 
    pivot_wider(names_from = estimate, values_from = p, names_prefix = "p_") |> 
    ggplot(aes(x = date, y = p_mean, fill = waning, col = waning)) +
    geom_ribbon(aes(ymin = p_lower, ymax = p_upper),
                alpha = 0.4,
                col = NA) +
    geom_line() +
    scale_x_date(limits = c(start_plot_date, end_plot_date),
                 expand = expansion(c(0, 0)),
                 date_breaks = "6 months",
                 date_labels = "%b %Y",
                 date_minor_breaks = "3 months") +
    scale_y_continuous(expand = expansion(c(0,0))) +
    scale_color_manual(values = color_scheme) +
    scale_fill_manual(values = color_scheme) +
    labs(x = NULL,
         y = paste("Fraction of", frac),
         color = "Waning",
         fill = "Waning") +
    theme_light() +
    facet_wrap(facets = vars(age_group)) +
    theme(strip.background = element_blank(),
          strip.text = element_text(colour = 1),
          legend.position = "bottom")
  
}
