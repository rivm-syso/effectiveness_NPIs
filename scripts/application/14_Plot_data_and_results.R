################################################################################
#
# Make plots of:
# - data
# - results
# 
################################################################################

# show effective R (not used in manuscript)
plot_effective_R(R_dat = R_data)

# Fig 1
plot_R0(data = R0_over_time,
        end_plot_date = parameters$end_date) |> 
  ggsave(filename = paste0("./figures/application/R0.", parameters$output_format), height = 3.5, width = 5.2, dpi = 450, bg = "white")

# Fig 2
plot_serosurvey_vaccination_data(immunity_dat = immunity_over_time,
                                 serosurvey_dat = serosurvey_data,
                                 inc_period = parameters$incubation_period,
                                 sero_delay = parameters$seropositive_delay,
                                 end_plot_date = parameters$end_date) |> 
  ggsave(filename = paste0("./figures/application/Infected_vaccinated_data.", parameters$output_format), height = 5, width = 7, dpi = 450, bg = "white")



# Fig 3, S3 and S4

for(w in parameters$waning_scenarios) {
  
  (plot_immunity_over_time(data = immunity_over_time,
                           waning_rate = w,
                           color_scheme = parameters$immunity_colors,
                           end_plot_date = parameters$end_date) +
     labs(subtitle = NULL)) |> 
    ggsave(filename = paste0("./figures/application/Immunity_", w, "_waning.", parameters$output_format), height = 5, width = 7, dpi = 450, bg = "white")
}


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
(plot_fraction_of_infections(data = bind_rows("Fraction of reinfections" = immunity_over_time |> mutate(p = p_reinf) |> filter(age_group == "all"),
                                              "Fraction of breakthrough infections" = immunity_over_time |> mutate(p = p_breakthrough) |> filter(age_group == "all"),
                                              .id = "type"),
                             frac = "",
                             color_scheme = parameters$waning_colors,
                             end_plot_date = parameters$end_date) + 
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
          strip.text = element_text(size = 8),
          legend.title = element_text(size = 10),
          legend.text = element_text(size = 8)) + 
    guides(colour = guide_legend(order = 2),
           fill = guide_legend(order = 2))) |> 
  ggsave(filename = paste0("./figures/application/Fraction_infections_all.", parameters$output_format), height = 3, width = 5.2, dpi = 450, bg = "white")


# Fig 5

pA <- plot_results(data = effectiveness_over_time |> mutate(trick = "to have a legend with one key"),
             what = "Rc",
             color_scheme = parameters$waning_colors,
             end_plot_date = parameters$end_date) +
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
        legend.position.inside = c(0.99, 0.99),
        legend.title = element_text(size = 9))


pB <- plot_results(data = effectiveness_over_time |> 
                   inner_join(OxSI_data) |> 
                   group_by(period, waning) |> 
                   mutate(eff_mean = mean(eff_mean),
                          eff_lower = mean(eff_lower),
                          eff_upper = mean(eff_upper)),
                 what = "eff",
                 color_scheme = parameters$waning_colors,
                 end_plot_date = parameters$end_date) +
  theme(legend.position.inside = c(0.99, 0.99),
        legend.title = element_text(size = 9))


pC <- plot_OxSI(OxSI_dat = OxSI_data, 
                legend = FALSE,
                start_plot_date = min(effectiveness_over_time$date),
                end_plot_date = parameters$end_date) +
  theme(plot.title = element_text(color = 1))


(pA + pB + pC +  
  plot_layout(ncol = 1, heights = c(0.85, 0.85, 0.05)) +
    plot_annotation(tag_levels = list(c("A", "B", "")))) |> 
  ggsave(filename = paste0("./figures/application/Effectiveness.", parameters$output_format), height = 8.75, width = 7, dpi = 450, bg = "white")


# Fig 7
(plot_waning_profiles(prof_infection_immunity = profile_infection_immunity,
                      prof_vaccine_immunity = profile_vaccine_immunity,
                      color_scheme = parameters$waning_colors) +
    theme(axis.title = element_text(size = 10),
          legend.title = element_text(size = 10))) |> 
  ggsave(filename = paste0("./figures/application/Waning_profiles.", parameters$output_format), height = 3.5, width = 5.2, dpi = 450, bg = "white")





# Fig S1
plot_variant_fractions(variant_frac = variant_fractions,
                       variant_dat = variant_data,
                       end_plot_date = parameters$end_date) |> 
  ggsave(filename = paste0("./figures/application/Variant_fractions.", parameters$output_format), height = 3, width = 7, dpi = 300, bg = "white")


# Fig S2
plot_structure(color_scheme = parameters$immunity_colors) |> 
  ggsave(filename = paste0("./figures/application/Structure_population_immunity.", parameters$output_format), height = 4, width = 6, dpi = 300, bg = "white")



# Fig S5
plot_fraction_of_infections(data = immunity_over_time |> mutate(p = p_reinf) |> filter(date <= parameters$end_date),
                            frac = "reinfections",
                            color_scheme = parameters$waning_colors,
                            end_plot_date = parameters$end_date) |> 
  ggsave(filename = paste0("./figures/application/Fraction_reinfections.", parameters$output_format), height = 5, width = 7, dpi = 300, bg = "white")


# Fig S6
plot_fraction_of_infections(data = immunity_over_time |> mutate(p = p_breakthrough),
                            frac = "breakthrough infections",
                            color_scheme = parameters$waning_colors,
                            end_plot_date = parameters$end_date) |> 
  ggsave(filename = paste0("./figures/application/Fraction_breakthrough_infections.", parameters$output_format), height = 5, width = 7, dpi = 300, bg = "white")


# Striking image

pB <- plot_results(data = effectiveness_over_time |> 
                     filter(waning == "medium") |> 
                     inner_join(OxSI_data) |> 
                     group_by(period, waning) |> 
                     mutate(eff_mean = mean(eff_mean),
                            eff_lower = mean(eff_lower),
                            eff_upper = mean(eff_upper)),
                   what = "eff",
                   color_scheme = parameters$waning_colors,
                   end_plot_date = parameters$end_date) +
  geom_hline(yintercept = 0, col = "grey") +
  scale_y_continuous(expand = expansion(c(0, 0)),
                     breaks = seq(-0.5, 1, 0.5),
                     labels = paste0(100*seq(-0.5, 1, 0.5), "%")) +
  theme(plot.margin = margin(0, 0, 0, 0, "pt"),
        panel.grid.minor = element_blank()) +
  guides(col = "none",
         fill = "none") +
  labs(y = "Effectiveness of NPIs")


pC <- plot_OxSI(OxSI_dat = OxSI_data, 
                legend = FALSE,
                start_plot_date = min(effectiveness_over_time$date),
                end_plot_date = parameters$end_date) +
  theme(#plot.title = element_text(color = 1),
    plot.title = element_text(hjust = 0.99, vjust=0.8, color = 1, size = 9, margin=margin(t = 0, b = -20)),
    plot.margin = margin(0, 0, 0, 0, "pt"),
    axis.ticks.length = unit(0, "pt"),
    legend.position = "top")


(pC + pB +  
    plot_layout(ncol = 1, heights = c(0.05, 0.95))
    ) |> 
  ggsave(filename = "./figures/application/EffectivenessNPIs_strikingimage.pdf", height = 4, width = 6, dpi = 450, bg = "white")

rm(pA, pB, pC)
rm(w)
rm(observed_data)
