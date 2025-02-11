construct_infected_at_least_once <- function(case_dat, sero_dat, limit, inc_period, sero_delay, skip_rounds) {
  
  sero_dat <- sero_dat |> 
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
           # determine at least once infection incidence in each round
           difinf = c(diff(c(0, cuminf)))) |> 
    select(age_group, round, cuminf, date, difinf)
  
  infected_at_least_once <- case_dat |>
    full_join(sero_dat) |> 
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
  if((infected_at_least_once |> filter(!check) |> nrow()) != 0) {
    fails <- infected_at_least_once |> filter(!check)
    warning(paste("Inferred cumulative incidence does not match observations in age group", fails$age_group, "in round", fails$round, "\n"))
  }
  
  return(infected_at_least_once |> 
           select(date, age_group, round, cuminf, inf, frac_pop, population) |> 
           rename(frac_infected = inf,
                  frac_infected_atleastonce = cuminf))
}
