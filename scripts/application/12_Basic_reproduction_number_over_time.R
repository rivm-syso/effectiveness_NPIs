################################################################################
#
# Construct basic reproduction number over time, which consist of 3 parts:
# - start R_0 as average observed R before 2020-03-01
# - modification due to emergence of variants
# - modification due to seasonal effect
# 
################################################################################

# reference date
startR0_date <- as.Date("2020-03-01")

# average of R values before 1 March 2020 used as R0 at 1 March 2020
start_R0 = R_data |> 
  filter(date < startR0_date) |> 
  summarise(R0 = mean(R_mean)) |> 
  pull(R0)

# determine transmissibility and fractions of variants assuming
# gamma distributed generation interval with mean of 4 days and shape 4
variant_fractions <- determine_variant_transmissibility(variant_dat = variant_data, mean_gen_int = 4, shape_gen_int = 4)

R0_over_time <- determine_R0_over_time(ref_date = startR0_date,
                                       variant_frac = variant_fractions,
                                       startR0 = start_R0,
                                       season_ratio = 1.5)

