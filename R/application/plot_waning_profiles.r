plot_waning_profiles <- function(prof_infection_protected,
                                 prof_vaccine_protected,
                                 color_scheme) {
  
  
  bind_rows(as_tibble(prof_infection_protected) |> 
              mutate(type = "infection",
                     time = 1:n()),
            as_tibble(prof_vaccine_protected) |> 
              mutate(type = "vaccination",
                     time = 1:n())) |> 
    pivot_longer(cols = names(prof_infection_protected), names_to = "waning", values_to = "frac_protected") |> 
    mutate(waning = factor(waning, levels = rev(c("slow", "medium", "fast")))) |> 
    ggplot(aes(x = time, y = frac_protected, lty = type, col = waning)) +
    geom_line() +
    scale_x_continuous(limits = c(0, 182),
                       expand = expansion(c(0, 0))) +
    scale_color_manual(values = color_scheme) +
    labs(x = "Time since infection / vaccination (days)", 
         y = "Immune fraction",
         color = "Waning",
         linetype = "Immune after") +
    theme_light() +
    theme(legend.box = "horizontal",
          legend.position = "inside",
          legend.position.inside = c(0.95, 0.05),
          legend.justification = c(1, 0))
  
}

