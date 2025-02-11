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
#   - calculate fraction protected by infection and vaccination (taking waning into account)
#   - adjust fraction of infections per age group to account for reinfections
#   - fraction of reinfections in infections = 1 -  fraction naive/fraction susceptible
# 
################################################################################

case_data_to_fit <- case_data |>
  group_by(age_group) |>
  arrange(date) |>
  mutate(n = rollmean(n, k = 7, fill = 0),
         n_original = n,
         # set cases back from symptom onset to time of infection
         n = lead(x = n, n = incubation_period)) |> 
  left_join(population_data)


immunity_over_time <- tibble()

for(w in c("medium", "fast", "slow")) {
  for(l in c("mean", "lower", "upper")) {
    
    print(paste("waning", w, "limit", l))
    
    immunity_over_time <- bind_rows(immunity_over_time,
                               construct_infected_at_least_once(case_dat = case_data_to_fit, 
                                                                sero_dat = serosurvey_data, 
                                                                limit = l, 
                                                                inc_period = incubation_period, 
                                                                sero_delay = seropositive_delay, 
                                                                skip_rounds = c(1, 3)) |> 
                                 construct_immunity(vaccination_dat = vaccination_data,
                                                    infection_protected = profile_infection_protected[[w]],
                                                    vaccine_protected = profile_vaccine_protected[[w]]) |> 
                                 mutate(estimate = l,
                                        waning = w))
    
  }
}


immunity_over_time <- bind_rows(immunity_over_time,
                           immunity_over_time |> 
                             group_by(date, estimate, waning) |> 
                             reframe(frac_infected = sum(frac_pop*frac_infected),
                                     frac_infected_atleastonce = sum(frac_pop*frac_infected_atleastonce),
                                     frac_vaccinated = sum(frac_pop*frac_vaccinated),
                                     frac_inf_protected = sum(frac_pop*frac_inf_protected),
                                     frac_vac_protected = sum(frac_pop*frac_vac_protected),
                                     frac_protected = sum(frac_pop*frac_protected),
                                     p_newinf = sum(frac_pop*p_newinf),
                                     p_breakthrough = sum(frac_pop*p_breakthrough),
                                     frac_pop = sum(frac_pop)) |>
                             # filter out last 2 days when not all age groups are present:
                             filter(zapsmall(frac_pop/1) == 1) |> 
                             mutate(age_group = "all")
)



# check if fractions add up to 1
if(immunity_over_time |> 
  mutate(frac_vac_protected_not_infected = frac_vac_protected*(1 - frac_inf_protected),
         frac_never_vaccinated_infected = (1 - frac_vaccinated)*(1 - frac_infected_atleastonce),
         frac_susceptible_nonnaive = (frac_infected_atleastonce - frac_inf_protected)*(1 - frac_vac_protected) + (1 - frac_infected_atleastonce)*(frac_vaccinated - frac_vac_protected)) |> 
  select(date, age_group, frac_inf_protected, frac_vac_protected_not_infected, frac_never_vaccinated_infected, frac_susceptible_nonnaive) |> 
  mutate(check = rowSums(across(starts_with("frac_")))) |> 
  filter(zapsmall(check) != 1) |> 
  nrow() != 0) warning("Fractions in population do not sum to 1")
  
rm(case_data_to_fit)
rm(l, w)
