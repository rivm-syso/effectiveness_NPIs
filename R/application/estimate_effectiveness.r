################################################################################
#
# Determine effectiveness of NPIs over time 
# - calculate effective susceptibility in population either by:
#   - hom: taking weighted average of age groups (disregarding heterogeneity between age groups) or
#   - het: weighting age group susceptibility by incidence^2 (accounting for heterogeneity between age groups)
# - calculate counterfactual R by multiplying R0 with effective susceptibility
# - calculate effectiveness as 1 - observed R / counterfactual R
# 
################################################################################

estimate_effectiveness <- function(immunity_time,
                                   R0_time,
                                   R_dat) {
  
  effective_susceptibility <- immunity_time |> 
    filter(age_group != "all") |> 
    group_by(age_group, estimate, waning) |> 
    arrange(date) |> 
    mutate(S = frac_pop*(1-frac_protected),
           w = frac_infected/S) |>
    group_by(date, estimate, waning) %>%
    reframe(S = sum(S)/sum(frac_pop)) |> 
    # reframe(S_hom = sum(S)/sum(frac_pop),
    #         S_het = sum(S*w^2)/(sum(frac_pop*w^2))) |> 
    # pivot_longer(cols = starts_with("S_"), names_to = "method", values_to = "S", names_prefix = "S_") |> 
    # note that upper and lower limits are reversed (this will be corrected in effectiveness calculation)
    pivot_wider(names_from = estimate, values_from = "S", names_prefix = "S_")
  

  effectiveness <- effective_susceptibility |> 
    inner_join(R0_time) |>
    inner_join(R_dat) |> 
    # calculate Rc with uncertainty (recall S_upper is in fact lower bound S)
    # calculate effectiveness with largest possible uncertainty 
    mutate(Rc_mean = R0*S_mean,
           Rc_lower = R0*S_upper,
           Rc_upper = R0*S_lower,
           # calculate variance, assuming normally distributed R and Rc, with 95% CI covering 4 sd
           R_var = ((R_upper - R_lower)/4)^2,
           Rc_var = ((Rc_upper - Rc_lower)/4)^2,
           # calculate ratio R/Rc with FiellerRatio, assuming independent R and Rc distributions (covar = 0)
           FiellerRatioCI(a = R_mean, b = Rc_mean, varA = R_var, varB = Rc_var, covar = 0, alpha = 0.05) |> 
             rename_with( ~ paste0("ratio_", .x)),
           # effectiveness = 1-ratio (and reverse limits)
           eff_mean = 1-ratio_mean,
           eff_lower = 1-ratio_upper,
           eff_upper = 1-ratio_lower
    ) |> 
    select(-starts_with("ratio"))
  
  return(effectiveness)
    
}
