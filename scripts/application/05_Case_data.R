################################################################################
#
# Load open data of COVID-19 cases
# - consisting of two files (with cases reported up until and after 3 October 2021)
# - see: https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.html
# - Date for statistics is 
#   - first day of illness (type DOO), if not known, 
#   - date of positive lab result (DPL), if not known, 
#   - reporting date to Public Health Service (DON)
# 
################################################################################

case_data <- bind_rows(
  read_csv2("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk_tm_03102021.csv"),
  read_csv2("https://data.rivm.nl/covid-19/COVID-19_casus_landelijk.csv"))

case_data <- case_data |> 
  # filter out cases with age group "<50" (n = 226) or "Unknown" (464)
  filter(Agegroup != "<50", Agegroup != "Unknown") |> 
  # assume symptom onset (DOO) is two days before positive test (DPL)
  transmute(date = if_else(Date_statistics_type == "DPL", Date_statistics - 2, Date_statistics),
            age_group = if_else(Agegroup == "70-79" | Agegroup == "80-89" | Agegroup == "90+", "70+", Agegroup) |> factor()) |> 
  group_by(date, age_group, .drop = FALSE) |> 
  count() |> 
  ungroup() |> 
  # from 10 Feb 2020 onwards at least one case per day, choose as start analysis
  filter(date >= as.Date("2020-02-10"))

case_data <- case_data |> 
  # add 10 days before needed to shift time series by incubation period and calculate moving average
  full_join(expand_grid(date = min(case_data$date) - rev(1:10), 
                        age_group = levels(case_data$age_group),
                        n = 0)) |>
  arrange(date, age_group) |> 
  mutate(age_group = factor(age_group))

