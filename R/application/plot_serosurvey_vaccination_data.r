plot_serosurvey_vaccination_data <- function(immunity_dat, 
                                             serosurvey_dat,
                                             inc_period,
                                             sero_delay,
                                             start_plot_date = NULL, 
                                             end_plot_date = NULL) {
  
  start_plot_date = if(is.null(start_plot_date)) min(immunity_dat$date) else start_plot_date
  end_plot_date = if(is.null(end_plot_date)) max(immunity_dat$date) else end_plot_date
  
  included_rounds <- immunity_dat$round |> unique()
  
  immunity_dat |> 
    filter(waning == "medium") |> 
    select(age_group, date, estimate, frac_vaccinated, frac_cumulative_infected) |> 
    pivot_longer(cols = starts_with("frac"), names_to = "type", values_to = "frac", names_prefix = "frac_") |> 
    pivot_wider(names_from = estimate, values_from = frac, names_prefix = "frac_") |> 
    # shift time axis to represent moment of seropositive response (11 days after infection)
    # to compare with data in serosurvey
    ggplot(aes(x = date + inc_period + sero_delay, y = frac_mean, fill = type, col = type)) +
    geom_ribbon(aes(ymin = frac_lower, ymax = frac_upper), 
                alpha = 0.4, 
                col = NA) +
    geom_line() +
    scale_color_manual(values = c("#01689b", "#ca005d"), labels = c("Cumulative infected", "Vaccinated"), name = NULL) +
    scale_fill_manual(values = c("#01689b", "#ca005d"), labels = c("Cumulative infected", "Vaccinated"), name = NULL) +
    new_scale_color() +
    geom_pointrange(data = serosurvey_dat |>
                      rename(age_group = part_age_group) |>
                      filter(round < max(round),
                             round %in% included_rounds) |> 
                      mutate(used_for_fitting = (round %in% included_rounds)),
                    aes(x = date, y = cuminf_mean, ymin = cuminf_lower, ymax = cuminf_upper, col = used_for_fitting),
                    inherit.aes = FALSE,
                    size = 0.3,
                    pch = 21,
                    fill = "white") +
    scale_color_manual(values = c(1), name = NULL, labels = c("Serosurvey data")) +
    scale_x_date(limits = c(start_plot_date, end_plot_date),
                 expand = expansion(c(0, 0)),
                 date_labels = "%b %Y") +
    theme_light() + 
    labs(x = NULL,
         y = "Fraction") +
    facet_wrap(facets = vars(age_group)) +
    theme(legend.direction = "horizontal",
          legend.box = "horizontal",
          legend.position = "bottom",
          strip.background = element_blank(),
          strip.text = element_text(colour = 1))
  
}
