################################################################################
#
# Calculate immunity after infection
# - using
#   - inf_pres: infection pressure over time in never infected susceptibles
#   - f_firstinf: fraction first infected over time
#   - profile: waning immunity profile after infection
# - assuming that the inf_pres in previously infected susceptibles is equal to the
#   inf_pres in never infected susceptibles
# - calculation consists of loop to construct fraction immune after infection
#   at time t based on previous time steps
# - note vaccination status do not need to be taken into account because we
#   assume this is independent of infection status
# - note population changes do not need to be taken into account because we
#   assume this is independent of infection status
# 
################################################################################

calculate_inf_immunity <- function(inf_pres, f_firstinf, profile) {
  
  # dynamic vector at time t containing times tau (from 1 to t) with fraction last infected at time tau 
  f_inf <- rep(0, length(inf_pres))
  # fraction immune after infection
  f_inf_imm <- rep(0, length(inf_pres))
  # fraction reinfections
  p_reinf <- rep(0, length(inf_pres))

  for(t in 2:(length(inf_pres))) {
    
    # fraction immune after infection
    f_inf_imm[t] <- sum(f_inf[1:(t-1)]*rev(profile[1:(t-1)]))

    # last infected before t and susceptible on t
    f_inf_sus <- f_inf[1:(t-1)]*(1-rev(profile[1:(t-1)]))
    
    # last infected before t and reinfected on t using assumption that infection pressure
    # in never infected susceptibles is equal to previously infected who lost immunity
    f_reinf <- inf_pres[t]*f_inf_sus

    # infected at t are the fraction first infected and the sum of reinfections
    f_inf[t] <- f_firstinf[t] + sum(f_reinf)
    
    # subtract reinfections from the vector with last infection times
    f_inf[1:(t-1)] <- f_inf[1:(t-1)] - f_reinf
    
    # fraction reinfections in all infections
    p_reinf[t] <- sum(f_reinf)/f_inf[t]

  }
  
  return(tibble(f_inf_imm, p_reinf))
  
}

