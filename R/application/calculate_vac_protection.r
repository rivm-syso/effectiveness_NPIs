calculate_vac_protection <- function(f_vac, prot) {
  # this function should yield same result as
  # zapsmall(convolve(prot, rev(f_vac), type = "open"))[1:n()]
  f_vac_protected <- rep(0, length(f_vac))
  
  for(t in 2:length(f_vac)) {
    # calculate protection at time t
    f_vac_protected[t] <- sum(rev(prot[1:t])*f_vac[1:t])
  }
  
  return(f_vac_protected)
  
}

