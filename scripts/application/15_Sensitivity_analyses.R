################################################################################
#
# Sensitivity analyses
# - seasonality
# 
################################################################################


season_parameters <- tibble(reduction = c(0, 0.2, 1/3, 0.42)) |> 
  mutate(ratio = 1/(1-reduction),
         amplitude = (ratio - 1)/(ratio + 1),
         labels = paste0(round(100*reduction), "%"))
  

season_sensitivity <- lapply(season_parameters$ratio, function(s)
  
  estimate_effectiveness(immunity_time = immunity_over_time,
                         R0_time = determine_R0_over_time(ref_date = startR0_date,
                                                          variant_frac = variant_fractions,
                                                          startR0 = start_R0,
                                                          season_ratio = s),
                         R_dat = R_data_movingaverage) |> 
    mutate(season_ratio = s)) |> bind_rows()


pA <- season_sensitivity |> 
  filter(waning == "medium") |> 
  select(-waning) |> 
  mutate(waning = factor(season_ratio, levels = season_parameters$ratio, labels = season_parameters$labels)) |> 
  inner_join(OxSI_data) |> 
  group_by(period, waning) |> 
  mutate(eff_mean = mean(eff_mean),
         eff_lower = mean(eff_lower),
         eff_upper = mean(eff_upper)) |> 
  plot_results(what = "eff",
               color_scheme = set_names(c("darkgrey",waning_colors), season_parameters$labels),
               end_plot_date = as.Date("2021-10-31")) + 
  labs(color = "Seasonal reduction\n(summer compared to winter)",
       fill = "Seasonal reduction\n(summer compared to winter)") +
  guides(color = guide_legend(ncol = 4),
         fill = guide_legend(ncol = 4))


pB <- plot_OxSI(OxSI_dat = OxSI_data, 
          legend = FALSE,
          start_plot_date = min(effectiveness_over_time$date),
          end_plot_date = end_date) +
  theme(plot.title = element_text(color = 1))

(pA + pB +  
    plot_layout(ncol = 1, heights = c(0.85, 0.05)))  |> 
  ggsave(filename = paste0("./figures/application/Effectiveness_seasonality.", output_format), height = 5, width = 7, dpi = 300, bg = "white")

rm(season_parameters)
rm(season_sensitivity)
rm(pA, pB)
