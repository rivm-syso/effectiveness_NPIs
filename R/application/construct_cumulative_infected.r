################################################################################
#
# Construct cumulative infected fraction in each age group over time from
# - serosurvey data (containing cumulative infected fraction of participants per round)
# - case data (to fill in dates between survey rounds)
# - population data (to express case numbers as fraction of population)
# - incubation period and seropositive delay to scale time axes to date of infection
# 
################################################################################

construct_cumulative_infected <- function(case_dat, serosurvey_dat, population_dat, limit, inc_period, sero_delay, skip_rounds) {
  
  # adjust case data by age group
  case_dat <- case_dat |>
    group_by(age_group) |>
    arrange(date) |>
    mutate(#use 7-day rolling average of symptom onset of cases to flatten out weekday effect 
           n = rollmean(n, k = 7, fill = 0),
           # set cases back from symptom onset to time of infection
           n = lead(x = n, n = inc_period)) 

  # adjust serosurvey data (shift time axis to date of infection and determine
  # incidence of first infections)
  serosurvey_dat <- serosurvey_dat |> 
    # select which limit to use
    rename(cuminf = paste0("cuminf_", limit)) |> 
    # omit rounds
    filter(!(round %in% skip_rounds)) |> 
    rename(age_group = part_age_group) |> 
    group_by(age_group) |> 
    arrange(date) |> 
    mutate(age_group = factor(age_group),
           # shift time axis from date of (first) seropositive signal to date of infection
           date = date - inc_period - sero_delay,
           # determine first infection incidence in each round
           difinf = c(diff(c(0, cuminf)))) |> 
    select(age_group, round, cuminf, date, difinf)
  
  # combine all data sets
  cumulative_infected <- case_dat |>
    full_join(serosurvey_dat) |> 
    left_join(population_dat)|> 
    fill(round, .direction = "up") |> 
    group_by(age_group, round) |> 
    arrange(date) |> 
    # NB: inc and inf are both infection incidence: inc from case numbers, inf in full population
    # i.e. inc = reporting_rate * inf
    mutate(cuminf_observed = cuminf, # store observations to use in final check 
           inc = n/population, # observed infection incidence (taking changing population size of age group into account)
           cuminc = cumsum(inc), # cumulative incidence ...
           # ... scaled to the observed infected fraction during that round
           inf = last(difinf)*inc/last(cuminc)) |> 
    group_by(age_group) |> 
    arrange(date) |> 
    # cumulative sum of scaled incidence should match observations
    mutate(cuminf = cumsum(inf),
           check = zapsmall(cuminf/cuminf_observed) == 1) |> 
    # filters out round 10 because case data is not available up to round 10 
    filter(!is.na(inf))
  
  # give warning when check fails
  if((cumulative_infected |> filter(!check) |> nrow()) != 0) {
    fails <- cumulative_infected |> filter(!check)
    warning(paste("Inferred cumulative incidence does not match observations in age group", fails$age_group, "in round", fails$round, "\n"))
  }
  
  return(cumulative_infected |> 
           select(date, age_group, round, cuminf, frac_pop) |> 
           rename(frac_cumulative_infected = cuminf))
}
