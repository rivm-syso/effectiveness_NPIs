################################################################################
#
# Get reproduction number data
# - from open data at data.rivm.nl/covid-19
# - case reproduction number, based on
#   - hospitalisations in Osiris data from start to 2020-06-12
#   - test positive cases in Osiris data from 2020-06-13 to 2023-03-14
#   - hospitalisations in NICE data from 2023-03-15 to 2023-07-09
# 
################################################################################


R_data <- bind_rows(read_json("https://data.rivm.nl/covid-19/COVID-19_reproductiegetal_tm_03102021.json", simplifyVector = TRUE),
                    read_json("https://data.rivm.nl/covid-19/COVID-19_reproductiegetal.json", simplifyVector = TRUE))


R_data <- R_data |> 
  as_tibble() |> 
  transmute(date = as.Date(Date),
            R_mean = as.numeric(Rt_avg),
            R_lower = as.numeric(Rt_low),
            R_upper = as.numeric(Rt_up),
            Rtype = population) |> 
  mutate(Rtype = case_when(Rtype == "hosp" & date < as.Date("2021-01-01") ~ "Hospitalisations in Osiris",
                           Rtype == "hosp" ~ "Hospitalisations in NICE",
                           TRUE ~ "Test-positive cases"))


