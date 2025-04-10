################################################################################
#
# Construct immunity in population over time with
# - cumulative infected data (from serosurvey data)
# - vaccination data
# - waning immunity profiles after infection and vaccination
# 
################################################################################

construct_immunity <- function(cumulative_infected_dat, vaccination_dat, infection_immunity, vaccine_immunity) {
  
  # Note: fractions are always expressed as fraction of the population of that age group at that time
  data <- cumulative_infected_dat |> 
    left_join(vaccination_dat) |> 
    group_by(age_group) |> 
    arrange(date) |> 
    fill(coverage, frac_vaccinated, .direction = "updown") |> 
    mutate(# fraction immune after vaccination determined by vaccination incidence and waning
      frac_vac_immune = calculate_vac_immunity(f_vac = diff(c(0, frac_vaccinated)), profile = vaccine_immunity),
      # first infections: difference of cumulative infected
      frac_first_infected = c(diff(frac_cumulative_infected), 0),
      # infection pressure: fraction of first infections in never infected
      infection_pressure = frac_first_infected/(1-frac_cumulative_infected),
      # fraction immune after infection determined by reconstructing (re)infection incidence and waning
      calculate_inf_immunity(inf_pres = infection_pressure,
                             f_firstinf = frac_first_infected,
                             profile = infection_immunity) |> 
        rename(frac_inf_immune = f_inf_imm,
               p_reinf_check = p_reinf),
      # fraction immune after infection and/or vaccination
      frac_immune = frac_inf_immune + (1-frac_inf_immune)*frac_vac_immune,
      # fraction breakthrough infections in new infections
      p_breakthrough = (frac_vaccinated - frac_vac_immune)/(1 - frac_vac_immune),
      # fraction reinfections in new infections
      p_reinf = (frac_cumulative_infected - frac_inf_immune)/(1 - frac_inf_immune),
      # the overall fraction reinfections should be identical to the fraction calculated in calculate_inf_immunity
      check = zapsmall(p_reinf/p_reinf_check) == 1
    )
  
  if((data |> filter(!check) |> nrow()) != 0) {
    fails <- data |> filter(!check)
    stop(paste("Fraction reinfections does not match calculations in age group", fails$age_group, "on", fails$dates, "\n"))
  }
  
  
  return(data)
}
