################################################################################
#
# Get serosurvey data
# - from data folder
# - including code from raw data to data in data folder
#   (raw data is not publicly available)
#   - determine cumulative infection incidence (= infected at least once)
#   - calculate fraction infected at least once by age group and round, 
#     using pico sample weights and survey package
#   - mean and 95% confidence interval
# 
################################################################################


file_cuminf <- "./data/data_cumulative_infection_from_serosurvey.csv"

if(file.exists(file_cuminf)) {
  
  serosurvey_data <- read_csv(file = file_cuminf)

} else {
  
  library(haven)
  library(survey)
  
  pico_data <- read_sas("undisclosed_path/pico_varselection_20240326.sas7bdat") 
  
  pico_data <- pico_data |> 
    # omit variables testpos_r[0-9]_cat (denoting symptomatic or asymptomatic test)
    select(!ends_with("_cat")) |> 
    # rename cases seropositive for infection
    rename(seropos_r1 = pico1_s1_pos_combi,
           seropos_r2 = pico2_S1_pos,
           seropos_r3 = pico3_s1_pos,
           seropos_r4 = pico4_pos_inf,
           seropos_r5 = pico5_pos_inf,
           seropos_r6 = pico6_pos_inf,
           seropos_r7 = pico7_pos_inf,
           seropos_r8 = pico8_pos_inf,
           seropos_r9 = pico9_pos_inf,
           seropos_r10 = pico10_pos_inf,
           weight_r1 = pico1_gewicht,
           weight_r2 = pico2_gewicht,
           weight_r3 = pico3_gewicht,
           weight_r4 = pico4_gewicht,
           weight_r5 = pico5_gewicht,
           weight_r6 = pico6_gewicht_inf,
           weight_r7 = pico7_gewicht_inf,
           weight_r8 = pico8_gewicht_inf,
           weight_r9 = pico9_gewicht_inf,
           weight_r10 = pico10_gewicht_inf) |> 
    pivot_longer(cols = matches("_r([0-9])"), 
                 names_to = c( ".value", "pico"),
                 names_sep = "_r") |> 
    # leave out LVC participants (not part or random sample)
    filter(region != 6) |> 
    filter(crf_label != "") |> 
    mutate(round = as.integer(pico)) |>  
    rename(part_id = dn_randomisatienr,
           part_gender = Geslacht,
           part_age = lftyear,
           fillindate = algdatuminvul) |> 
    filter(!is.na(part_age)) |> 
    mutate(part_age_group = cut(part_age, breaks = c(seq(0, 70, 10), Inf), include.lowest = TRUE, right = FALSE,
                                labels = c("0-9", "10-19", "20-29", "30-39", "40-49", "50-59", "60-69", "70+"))) |> 
    select(part_id, round, part_age_group, fillindate, seropos, weight) 

    
  
  # Impute cumulative infection incidence: once infected, always seropos
  
  first_infected <- pico_data |> 
    filter(seropos == 1) |> 
    group_by(part_id) |> 
    summarise(round_infected = min(round))
  
  # 90 unique participants seroreverted: in 198 rounds they were seronegative
  # after the round they were first found infected
  pico_data |> 
    left_join(first_infected) |> 
    filter(seropos == 0 & round > round_infected) |> 
    summarise(n = n(),
              n_part = length(unique(part_id)))

  # Set seropos of seroreverted participants to 1 (and drop NA values)
  pico_data <- pico_data |> 
    left_join(first_infected) |> 
    mutate(seropos = if_else(round > round_infected, 1, seropos, seropos)) |> 
    filter(!is.na(seropos), !is.na(weight))
  
  # Median fill in date by round and age group
  data_rounds <- pico_data |> 
    group_by(round, part_age_group) |> 
    reframe(date = median(fillindate, na.rm = TRUE))
  
  # Determine cumulative infection incidence by round and age group
  # with confidence intervals using survey package
  
  des <- svydesign(
    ids = ~ part_id,
    weights = ~ weight,
    data = pico_data)
  
  serosurvey_data <- svyby(~seropos, ~part_age_group + round, des, svyciprop, vartype ="ci", method = "beta") |> 
    full_join(data_rounds) |> 
    rename(cuminf_mean = seropos,
           cuminf_upper = ci_u,
           cuminf_lower = ci_l)
    
  
  write_csv(serosurvey_data, file = file_cuminf)
  
  rm(data_rounds)
  rm(des)
  rm(first_infected)
  rm(pico_data)

}

rm(file_cuminf)

