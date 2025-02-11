################################################################################
#
# Decide profiles since infection for:
# - infection_protected: immediately protected after infection, after plateau start of waning
# - vaccine_protected: max at two weeks after end primary series, after plateau start of waning
# - waning for both starts 42 days after infection or vaccination
#
# Waning after infection:
# https://www.thelancet.com/journals/lancet/article/PIIS0140-6736(22)02465-5/fulltext
# Protection from re-infection from ancestral, alpha, and delta variants declined over time but remained at 78·6% (49·8–93·6) at 40 weeks
#
# Waning after vaccination:
# https://www.thelancet.com/journals/lanres/article/PIIS2213-2600(23)00015-2/fulltext
# For infections caused by any SARS-CoV-2 strain, vaccine effectiveness for the primary series reduced from 83% (95% CI 80–86) at baseline (14–42 days) to 62% (53–69) by 112–139 days.
#
################################################################################

max_profile <- 3650 # to ensure convolution is applied to full time series

incubation_period <- 5 # time between infection and symptom onset
seropositive_delay <- 6 # time between symptom onset and seropositive

full_protection_duration <- 42 # time between infection/vaccination and start waning

# Protected after infection
# Lancet: protection 78·6% (49·8–93·6) at 40 weeks
remaining_infection_protection = setNames(c(0.786, 0.498, 0.936), nm = c("medium", "fast", "slow"))
infection_waning_exponent <- log(1/remaining_infection_protection)/(40*7-full_protection_duration)
profile_infection_protected <- as.list(as_tibble(
  sapply(infection_waning_exponent, 
         function(w) c(rep(1, full_protection_duration), exp(-w*(1:(max_profile - full_protection_duration)))))))


# Protected after vaccination (completed primary series)
# Lancet: from 83% (95% CI 80–86) at baseline (14–42 days) ...
max_protection <- setNames(c(0.83, 0.80, 0.86), nm = c("medium", "fast", "slow"))
frac_protection_at_start <- 0.5
days_until_max_protection <- 14
max_protection_duration <- full_protection_duration - days_until_max_protection
# Lancet: ... to 62% (53–69) by 112–139 days
remaining_vaccine_protection <- c(0.62, 0.53, 0.69)/max_protection
vaccine_waning_exponent <- log(1/remaining_vaccine_protection)/(126-(days_until_max_protection + max_protection_duration))
profile_vaccine_protected <- as.list(as_tibble(
  mapply(function(m, w) m*c(seq(frac_protection_at_start, 1, length.out = days_until_max_protection),
                            rep(1, max_protection_duration),
                            exp(-w*(1:(max_profile - (days_until_max_protection + max_protection_duration))))), 
         max_protection, 
         vaccine_waning_exponent)))


rm(days_until_max_protection, frac_protection_at_start, full_protection_duration)
rm(max_profile, max_protection, max_protection_duration)
rm(remaining_infection_protection, remaining_vaccine_protection, vaccine_waning_exponent, infection_waning_exponent)


