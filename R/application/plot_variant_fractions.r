plot_variant_fractions <- function(variant_frac, 
                                   variant_dat, 
                                   start_plot_date = NULL, 
                                   end_plot_date = NULL) {
  
  start_plot_date = if(is.null(start_plot_date)) min(variant_frac$date) else start_plot_date
  end_plot_date = if(is.null(end_plot_date)) max(variant_frac$date) else end_plot_date
  
  ggplot(data = variant_frac,
         aes(x = date, y = fraction, col = variant)) +
    geom_line() +
    geom_point(data = variant_dat |> 
                 group_by(date) |> 
                 mutate(fraction = count/sum(count))) +
    scale_x_date(limits = c(start_plot_date, end_plot_date),
                 expand = expansion(c(0, 0)),
                 date_labels = "%b %Y") +
    scale_y_continuous(limits = c(0, 1),
                       expand = expansion(c(0, 0))) +
    labs(x = NULL,
         y = "Fraction",
         col = "Variant") +
    theme_light() 
}
