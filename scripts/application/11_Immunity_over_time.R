################################################################################
#
# Determine immunity over time per age group
# - adjust case data by age group:
#   - use 7-day rolling average of symptom onset of cases to flatten out weekday effect 
#   - shift number of symptom onsets per day to number of infections per day
# - construct fraction infected at least once 
#   - per age group over time to match serosurvey data
#   - omit round 1 because test was still in development
#   - omit round 3 because fraction infected at least once decreases in some age groups 
# - construct immunity per age group over time
#   - calculate fraction immune by infection and vaccination (taking waning into account)
#   - adjust fraction of infections per age group to account for reinfections
#   - fraction of reinfections in infections = 1 -  fraction naive/fraction susceptible
# 
################################################################################


immunity_over_time <- tibble()

for(w in parameters$waning_scenarios) {
  for(l in c("mean", "lower", "upper")) {
    
    print(paste("waning scenario", w, "limit", l))
    
    immunity_over_time <- bind_rows(immunity_over_time,
                                    construct_cumulative_infected(case_dat = case_data, 
                                                                  serosurvey_dat = serosurvey_data,
                                                                  population_dat = population_data,
                                                                  limit = l, 
                                                                  inc_period = parameters$incubation_period, 
                                                                  sero_delay = parameters$seropositive_delay, 
                                                                  skip_rounds = c(1, 3)) |> 
                                      construct_immunity(vaccination_dat = vaccination_data,
                                                         infection_immunity = profile_infection_immunity[[w]],
                                                         vaccine_immunity = profile_vaccine_immunity[[w]]) |> 
                                      mutate(estimate = l,
                                             waning = w))
    
  }
}


immunity_over_time <- bind_rows(immunity_over_time,
                           immunity_over_time |> 
                             group_by(date, estimate, waning) |> 
                             reframe(frac_cumulative_infected = sum(frac_pop*frac_cumulative_infected),
                                     frac_vaccinated = sum(frac_pop*frac_vaccinated),
                                     frac_inf_immune = sum(frac_pop*frac_inf_immune),
                                     frac_vac_immune = sum(frac_pop*frac_vac_immune),
                                     frac_immune = sum(frac_pop*frac_immune),
                                     p_reinf = sum(frac_pop*p_reinf),
                                     p_breakthrough = sum(frac_pop*p_breakthrough),
                                     frac_pop = sum(frac_pop)) |>
                             # filter out last 2 days when not all age groups are present:
                             filter(zapsmall(frac_pop/1) == 1) |> 
                             mutate(age_group = "all")
)



# check if fractions add up to 1
if(immunity_over_time |> 
  mutate(frac_vac_immune_not_infected = frac_vac_immune*(1 - frac_inf_immune),
         frac_naive = (1 - frac_vaccinated)*(1 - frac_cumulative_infected),
         frac_susceptible_nonnaive = (frac_cumulative_infected - frac_inf_immune)*(1 - frac_vac_immune) + (1 - frac_cumulative_infected)*(frac_vaccinated - frac_vac_immune)) |> 
  select(date, age_group, frac_inf_immune, frac_vac_immune_not_infected, frac_naive, frac_susceptible_nonnaive) |> 
  mutate(check = rowSums(across(starts_with("frac_")))) |> 
  filter(zapsmall(check) != 1) |> 
  nrow() != 0) warning("Fractions in population do not sum to 1")
  
rm(l, w)
