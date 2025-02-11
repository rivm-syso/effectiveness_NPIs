
construct_immunity <- function(adjusted_case_dat, vaccination_dat, infection_protected, vaccine_protected) {
  
  # Note: fractions are always expressed as fraction of the population of that age group at that time
  data <- adjusted_case_dat |> 
    left_join(vaccination_dat) |> 
    fill(coverage, frac_vaccinated, .direction = "updown") |> 
    group_by(age_group) |> 
    arrange(date) |> 
    mutate(p_newinf = rep(1, n()),
           frac_inf_protected = calculate_inf_protection(f_inf = frac_infected, pop = population, p_ninf = p_newinf, prot = infection_protected),
           # vaccine protection not altered in iterative loop below
           frac_vac_protected = calculate_vac_protection(f_vac = diff(c(0, frac_vaccinated)), prot = vaccine_protected),
           # to check: should be the same as frac_infected_atleastonce
           frac_infected_alo = cumsum(p_newinf*frac_infected),
           frac_protected = frac_inf_protected + (1-frac_inf_protected)*frac_vac_protected,
           p_breakthrough = 1 - (1 - frac_vaccinated)*(1 - frac_inf_protected)/(1 - frac_protected),
           p_adjust = p_newinf * (1 - frac_protected)/((1 - frac_infected_alo)*(1 - frac_vac_protected)),
           p_newinf = (1 - frac_infected_alo)*(1 - frac_vac_protected) /(1 - frac_protected),
           check = zapsmall(frac_infected_alo/frac_infected_atleastonce) == 1
    )

  if((data |> filter(!check) |> nrow()) != 0) {
    fails <- data |> filter(!check)
    stop(paste("Inferred cumulative incidence does not match observations in age group", fails$age_group, "on", fails$dates, "\n"))
  }
  

  # Iterative loop to adjust infections to match probability of reinfection (= 1-p_newinf)
  
  while(!(all(data$p_adjust < 1.001) & all(data$p_adjust > 0.999))) {
    
    data <- data |>
      # data is already grouped by age group, but just in case
      group_by(age_group) |> 
      arrange(date) |> 
      mutate(frac_infected = frac_infected*p_adjust,
             frac_inf_protected = calculate_inf_protection(f_inf = frac_infected, pop = population, p_ninf = p_newinf, prot = infection_protected),
             # to check: should be the same as frac_infected_atleastonce
             frac_infected_alo = cumsum(p_newinf*frac_infected),
             frac_protected = frac_inf_protected + (1-frac_inf_protected)*frac_vac_protected,
             p_breakthrough = 1 - (1 - frac_vaccinated)*(1 - frac_inf_protected)/(1 - frac_protected),
             p_adjust = p_newinf * (1 - frac_protected)/((1 - frac_infected_alo)*(1 - frac_vac_protected)),
             p_newinf = (1 - frac_infected_alo)*(1 - frac_vac_protected)/(1 - frac_protected),
             check = zapsmall(frac_infected_alo/frac_infected_atleastonce) == 1
      )
    
    if((data |> filter(!check) |> nrow()) != 0) {
      fails <- data |> filter(!check)
      stop(paste("Inferred cumulative incidence does not match observations in age group", fails$age_group, "on", fails$dates, "\n"))
    }
    
  }

  return(data |> select(-check, -frac_infected_alo))
}


