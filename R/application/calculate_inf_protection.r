
calculate_inf_protection <- function(f_inf, pop, p_ninf, prot) {
  
  f_inf_protected <- rep(0, length(f_inf))
  # number of infections
  n_inf <- f_inf*pop
  
  for(t in 2:length(n_inf)) {
    # total number of reinfections at time t
    tot_reinf <- (1-p_ninf[t])*n_inf[t]
    
    # to avoid numerical issues (at least a tenth of a person reinfected)
    if(tot_reinf > 1e-01) {
      # distribution of number of reinfections at time t over period before t
      n_reinf <- (1-rev(prot[1:t]))*n_inf[1:t]
      # normalize to total number of reinfections
      n_reinf <- tot_reinf*n_reinf/max(cumsum(n_reinf))
      # subtract number of reinfections at time of infection from total number of infections
      # so they won't be reinfected at any next t in this loop
      # so f_inf stores infections with 'active' protection at time t
      n_inf <- n_inf - c(n_reinf, rep(0, length(n_inf) - length(n_reinf)))
    }
    # calculate protection at time t
    f_inf_protected[t] <- sum(rev(prot[1:t])*n_inf[1:t])/pop[t]
  }
  
  return(f_inf_protected)
  
}
