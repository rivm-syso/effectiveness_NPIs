################################################################################
#
# Make plots of:
# - data
# - results
# 
################################################################################

output_format <- "png"

end_date <- as.Date("2021-10-31")

waning_colors <- c("slow" = "#9ccb86",
                   "medium" = "#009392",
                   "fast" = "#e88471")


immunity_colors <- c("Naive" = "#FDDBC7",
                     "Non-naive susceptible" = "#F4A582",
                     "Immune after vaccination" = "#4393C3",
                     "Immune after infection" = "#2166AC")




plot_effective_R(R_dat = R_data)

# Fig 1
plot_R0(data = R0_over_time,
        end_plot_date = end_date) |> 
  ggsave(filename = paste0("./figures/application/R0.", output_format), height = 3.5, width = 5, dpi = 300, bg = "white")

# Fig 2
plot_serosurvey_vaccination_data(immunity_dat = immunity_over_time,
                                 serosurvey_dat = serosurvey_data,
                                 end_plot_date = end_date) |> 
  ggsave(filename = paste0("./figures/application/Infected_alo_vaccinated_data.", output_format), height = 5, width = 7, dpi = 300, bg = "white")



# Fig 3
(plot_immunity_over_time(data = immunity_over_time,
                        waning_rate = "medium",
                        color_scheme = immunity_colors,
                        end_plot_date = end_date) +
  labs(subtitle = NULL)) |> 
  ggsave(filename = paste0("./figures/application/Immunity_medium_waning.", output_format), height = 5, width = 7, dpi = 300, bg = "white")


# prepare observed data to plot with Fig. 4

observed_data <- 
  bind_rows("Fraction of reinfections" = if(exists("reinfections_data")) {
    reinfections_data |> 
      filter(reinfected_NA < 0.5) |> 
      mutate(frac = reinfected_1/(reinfected_1  + reinfected_0),
             frac_min = reinfected_1/(reinfected_1  + reinfected_0),
             frac_max = reinfected_1/(reinfected_1  + reinfected_0))
  },
  "Fraction of breakthrough infections" = if(exists("breakthrough_infections_data")) {
    breakthrough_infections_data |> 
      filter(vaccinated_NA < 0.5) |>
      # define range for infections in partly vaccinated persons (who received 1 of 2 vaccine doses)
      mutate(frac = (vaccinated_1 + 0.5*vaccinated_0.5)/(vaccinated_1  + vaccinated_0.5 + vaccinated_0),
             frac_min = (vaccinated_1)/(vaccinated_1  + vaccinated_0.5 + vaccinated_0),
             frac_max = (vaccinated_1 + vaccinated_0.5)/(vaccinated_1  + vaccinated_0.5 + vaccinated_0))
  },
  .id = "type") |> 
  mutate(trick = "to plot legend for variable with single level")

# Fig 4
(plot_fraction_of_infections(data = bind_rows("Fraction of reinfections" = immunity_over_time |> mutate(p = 1-p_newinf) |> filter(age_group == "all"),
                                              "Fraction of breakthrough infections" = immunity_over_time |> mutate(p = p_breakthrough) |> filter(age_group == "all"),
                                              .id = "type"),
                             frac = "",
                             color_scheme = waning_colors,
                             end_plot_date = end_date) + 
    {if(nrow(observed_data) != 0) 
      list(new_scale_color(),
        new_scale_fill(),
        geom_line(data = observed_data,
                  aes(x = date, y = frac, col = trick),
                  inherit.aes = FALSE),
        
        geom_ribbon(data = observed_data,
                    aes(x = date, ymin = frac_min, ymax = frac_max, fill = trick),
                    alpha = 0.2,
                    col = NA,
                    inherit.aes = FALSE),
        
        scale_color_manual(values = c(1), name = NULL, labels = c("Test-positive case data")),
        scale_fill_manual(values = c(1), name = NULL, labels = c("Test-positive case data")))
    } +

    coord_cartesian(ylim = c(0, 0.6)) +
    labs(y = NULL) +
    facet_wrap(facets = vars(type |> fct_rev()),
               strip.position = "left") + 
    theme(strip.placement = "outside",
          strip.text = element_text(size = 8)) + 
    guides(colour = guide_legend(order = 2),
           fill = guide_legend(order = 2))) |> 
  ggsave(filename = paste0("./figures/application/Fraction_infections_all.", output_format), height = 3, width = 6, dpi = 300, bg = "white")


rm(observed_data)

# Fig 5

pA <- plot_results(data = effectiveness_over_time |> mutate(trick = "to have a legend with one key"),
             what = "Rc",
             color_scheme = waning_colors,
             end_plot_date = end_date) +
  labs(y = "Reproduction number",
       fill = "Counterfactual R\nwith waning",
       col = "Counterfactual R\nwith waning") +
  new_scale_color() +
  new_scale_fill() +
  geom_ribbon(data = effectiveness_over_time |> filter(waning == "medium") |> mutate(trick = "to have a legend with one key"),
              mapping = aes(x = date, y = R_mean, ymin = R_lower, ymax = R_upper, fill = trick)) +
  geom_line(data = effectiveness_over_time |> mutate(trick = "to have a legend with one key"),
            mapping = aes(x = date, y = R_mean, col = trick)) +
  scale_fill_manual(
    values = adjustcolor(c("darkgrey"), alpha = 0.5),
    labels = c("observed")) +
  scale_color_manual(
    values = c("darkgrey"),
    labels = c("observed")) +
  labs(fill = "Effective R",
       col = "Effective R") +
  theme(legend.box = "horizontal",
        legend.position.inside = c(0.92, 0.97),
        legend.title = element_text(size = 10))


pB <- plot_results(data = effectiveness_over_time |> 
                   inner_join(OxSI_data) |> 
                   group_by(period, waning) |> 
                   mutate(eff_mean = mean(eff_mean),
                          eff_lower = mean(eff_lower),
                          eff_upper = mean(eff_upper)),
                 what = "eff",
                 color_scheme = waning_colors,
                 end_plot_date = end_date) +
  theme(legend.title = element_text(size = 10))


pC <- plot_OxSI(OxSI_dat = OxSI_data, 
                legend = FALSE,
                start_plot_date = min(effectiveness_over_time$date),
                end_plot_date = end_date) +
  theme(plot.title = element_text(color = 1))


(pA + pB + pC +  
  plot_layout(ncol = 1, heights = c(0.85, 0.85, 0.05)) +
    plot_annotation(tag_levels = list(c("A", "B", "")))) |> 
  ggsave(filename = paste0("./figures/application/Effectiveness.", output_format), height = 9, width = 7, dpi = 300, bg = "white")


rm(pA, pB, pC)

# Fig 7
(plot_waning_profiles(prof_infection_protected = profile_infection_protected,
                      prof_vaccine_protected = profile_vaccine_protected,
                      color_scheme = waning_colors) +
    theme(axis.title = element_text(size = 10),
          legend.title = element_text(size = 10))) |> 
  ggsave(filename = paste0("./figures/application/Waning_profiles.", output_format), height = 3.5, width = 5, dpi = 300, bg = "white")





# Fig S1
plot_variant_fractions(variant_frac = variant_fractions,
                       variant_dat = variant_data,
                       end_plot_date = end_date) |> 
  ggsave(filename = paste0("./figures/application/Variant_fractions.", output_format), height = 3, width = 7, dpi = 300, bg = "white")


# Fig S2
plot_structure(color_scheme = immunity_colors) |> 
  ggsave(filename = paste0("./figures/application/Structure_population_immunity.", output_format), height = 4, width = 6, dpi = 300, bg = "white")



# Fig S3
plot_immunity_over_time(data = immunity_over_time,
                        waning_rate = "slow",
                        color_scheme = immunity_colors,
                        end_plot_date = end_date) |> 
  ggsave(filename = paste0("./figures/application/Immunity_slow_waning.", output_format), height = 5, width = 7, dpi = 300, bg = "white")

# Fig S4
plot_immunity_over_time(data = immunity_over_time,
                        waning_rate = "fast",
                        color_scheme = immunity_colors,
                        end_plot_date = end_date) |> 
  ggsave(filename = paste0("./figures/application/Immunity_fast_waning.", output_format), height = 5, width = 7, dpi = 300, bg = "white")


# Fig S5
plot_fraction_of_infections(data = immunity_over_time |> mutate(p = 1- p_newinf) |> filter(date <= end_date),
                            frac = "reinfections",
                            color_scheme = waning_colors,
                            end_plot_date = end_date) |> 
  ggsave(filename = paste0("./figures/application/Fraction_reinfections.", output_format), height = 5, width = 7, dpi = 300, bg = "white")


# Fig S6
plot_fraction_of_infections(data = immunity_over_time |> mutate(p = p_breakthrough),
                            frac = "breakthrough infections",
                            color_scheme = waning_colors,
                            end_plot_date = end_date) |> 
  ggsave(filename = paste0("./figures/application/Fraction_breakthrough_infections.", output_format), height = 5, width = 7, dpi = 300, bg = "white")



