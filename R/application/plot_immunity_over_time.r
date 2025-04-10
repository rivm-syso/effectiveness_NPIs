

plot_immunity_over_time <- function(data,
                                    waning_rate,
                                    color_scheme,
                                    start_plot_date = NULL, 
                                    end_plot_date = NULL) {
  
  start_plot_date = if(is.null(start_plot_date)) min(data$date) else start_plot_date
  end_plot_date = if(is.null(end_plot_date)) max(data$date) else end_plot_date
  
  data |> 
    filter(estimate == "mean", waning == waning_rate) |> 
    mutate(frac_vac_immune_not_infected = frac_vac_immune*(1 - frac_inf_immune),
           frac_naive = (1 - frac_vaccinated)*(1 - frac_cumulative_infected),
           frac_susceptible_nonnaive = (frac_cumulative_infected - frac_inf_immune)*(1 - frac_vac_immune) + (1 - frac_cumulative_infected)*(frac_vaccinated - frac_vac_immune),
           limit_1 = frac_inf_immune,
           limit_2 = frac_inf_immune + frac_vac_immune_not_infected ,
           limit_3 = frac_inf_immune + frac_vac_immune_not_infected + frac_susceptible_nonnaive,
           limit_4 = 1) |> 
    select(date, age_group, starts_with("limit")) |> 
    pivot_longer(cols = starts_with("limit"), names_to = "limit", names_prefix = "limit_", values_to = "frac_max") |> 
    mutate(status = factor(names(color_scheme)[5-as.integer(limit)], levels = names(color_scheme))) |> 
    group_by(date, age_group) |> 
    arrange(limit) |> 
    mutate(frac_min = c(0, frac_max[1:3])) |> 
    ungroup() |> 
    ggplot(aes(x = date, ymin = frac_min, ymax = frac_max, fill = status)) +
    geom_ribbon(alpha = 0.8,
                col = NA) +
    scale_x_date(limits = c(start_plot_date, end_plot_date),
                 expand = expansion(c(0, 0)),
                 date_labels = "%b %Y") +
    scale_y_continuous(expand = expansion(c(0, 0)),
                       minor_breaks = 0.25) +
    scale_fill_manual(values = color_scheme) +
    labs(x = NULL,
         y = "Fraction",
         fill = NULL,
         subtitle = paste(waning_rate, "waning")) +
    theme_minimal() +
    theme(legend.position = "bottom") +
    facet_wrap(facets = vars(age_group)) 
}
