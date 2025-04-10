################################################################################
#
# Calculate immunity after vaccination
# by convolution of vaccination incidence and immunity waning profile
# 
################################################################################

calculate_vac_immunity <- function(f_vac, profile) {

  f_vac_imm <- rep(0, length(f_vac))
  
  for(t in 2:length(f_vac)) {
    # calculate protection at time t
    f_vac_imm[t] <- sum(f_vac[1:(t-1)]*rev(profile[1:(t-1)]))
  }
  
  return(f_vac_imm)
  
}

