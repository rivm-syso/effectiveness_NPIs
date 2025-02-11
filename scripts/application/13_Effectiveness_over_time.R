################################################################################
#
# Determine effectiveness of NPIs over time from
# - immunity data over time by age group
# - basic reproduction number over time
# - observed reproduction number over time
#   use 7-day rolling average of observed R to flatten out weekday effect 
# 
################################################################################


R_data_movingaverage <- R_data |> 
  mutate(R_mean = rollmean(R_mean, k = 7, fill = 0),
         R_lower = rollmean(R_lower, k = 7, fill = 0),
         R_upper = rollmean(R_upper, k = 7, fill = 0)) |> 
  # discard first and last 3 days (because of centered moving average)
  filter(R_upper != 0)

effectiveness_over_time <- estimate_effectiveness(immunity_time = immunity_over_time,
                                                  R0_time = R0_over_time,
                                                  R_dat = R_data_movingaverage)


# check some results

tmp <- effectiveness_over_time |> 
  filter(waning == "medium",
         date < as.Date("2021-10-31")) |> 
  inner_join(OxSI_data) |> 
  group_by(period, waning) |> 
  mutate(eff_mean = mean(eff_mean),
         eff_lower = mean(eff_lower),
         eff_upper = mean(eff_upper)) |> 
  ungroup() |> 
  filter(!duplicated(period))

# negative effectiveness in period around July 2021
tmp |> filter(eff_mean == min(eff_mean)) |> 
  select(date, starts_with("eff_"))

# changes in effectiveness and stringency mostly congruent
tmp |> 
  mutate(pos_diff_eff = c(NA, diff(eff_mean)) > 0,
         pos_diff_SI = c(NA, diff(OxSI)) > 0) |> 
  select(pos_diff_eff, pos_diff_SI) |> 
  table()

rm(tmp)
