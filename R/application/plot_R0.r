plot_R0 <- function(data,
                    start_plot_date = NULL, 
                    end_plot_date = NULL) {
  
  start_plot_date = if(is.null(start_plot_date)) min(data$date) else start_plot_date
  end_plot_date = if(is.null(end_plot_date)) max(data$date) else end_plot_date

  data |> 
    rename(R0_full = R0) |> 
    mutate(R0_variant = startR0*weighted_transmissibility,
           R0_start = startR0,
           R0_season = startR0*(1+season)/(1+season_atrefdate)) |> 
    select(date, starts_with("R0")) |> 
    pivot_longer(cols = starts_with("R0"), names_to = "type", values_to = "R0", names_prefix = "R0_") |> 
    mutate(component = if_else(type == "full", FALSE, TRUE),
           type = factor(type, levels = c("full", "start", "season", "variant"))) |> 
    ggplot(aes(x = date, y = R0, lty = interaction(type, component), col = interaction(type, component))) +
    geom_line(lwd = 0.8) +
    geom_vline(xintercept = as.Date("2020-03-01"),
               col = rgb(0.5, 0.5, 0.5)) +
    scale_x_date(limits = c(start_plot_date, end_plot_date),
                 expand = expansion(c(0, 0)),
                 date_labels = "%b %Y") +
    scale_y_continuous(limits = c(0, NA),
                       expand = expansion(c(0, 0.05))) +
    scale_linetype_manual(values = 1:4,
                          labels = expression(full: ~ R[0]^initial ~ theta(t) ~ sigma(t)/sigma(t[ref]), 
                                              initial: ~ R[0]^initial, 
                                              season: ~ R[0]^initial ~ sigma(t)/sigma(t[ref]), 
                                              variant: ~ R[0]^initial ~ theta(t))) +
    scale_color_manual(values = c(1, "royalblue", "royalblue", "royalblue"),
                       labels = expression(full: ~ R[0]^initial ~ theta(t) ~ sigma(t)/sigma(t[ref]), 
                                           initial: ~ R[0]^initial, 
                                           season: ~ R[0]^initial ~ sigma(t)/sigma(t[ref]), 
                                           variant: ~ R[0]^initial ~ theta(t))) +
    annotate("text", x = as.Date("2020-03-01"), y = 0.5, hjust = -0.5, label = expression(t[ref]), size = 5, color = rgb(0.5, 0.5, 0.5)) +
    labs(x = NULL,
         y = "Basic reproduction number",
         linetype = NULL,
         color = NULL) +
    theme_light() +
    theme(legend.position = "inside",
          legend.position.inside = c(0.05, 0.95),
          legend.justification = c(0, 1))
  
}



