################################################################################
#
# Load validation data for reinfections and breakthrough infections
# - from data folder OR
# - from Osiris data
#   - calculate fraction of infections with known infection type and vaccination status 
#   - calculate fraction of reinfections and breakthrough infections per week
# 
################################################################################

file_reinf <- "./data/data_reinfections.csv"
file_breakthrough <- "./data/data_breakthrough_infections.csv"

if(file.exists(file_reinf) & file.exists(file_breakthrough)) {
  
  reinfections_data <- read_csv(file = file_reinf)
  breakthrough_infections_data <- read_csv(file = file_breakthrough)
  
} else {

  options(fst_threads = 4)
  library(fst)
  
  report_date <- as.Date("2022-03-01")
  
  dataO <- tibble(filename = list.files("undisclosed_path", full.names = TRUE)) |> 
    filter(grepl(filename, pattern = gsub(as.character(report_date), pattern = "-", replacement = "")))  |> 
    pull(filename) |> 
    read_fst(columns = c("OSIRISNR", "ZIE1eZiekteDt", "NCOVdat1eposncov", "MELGGDOntvDt", "MELRIVMOntvDt", "EIGENAARDesc", "Herinfectie", "Vaccinatie_status_ezd_Desc")) |> 
    as_tibble() |> 
    # only cases in European Netherlands
    filter(!(EIGENAARDesc %in% c("Sint Eustatius", "Bonaire", "Saba", "Sint Maarten", "Aruba", "Curaçao", "Curacao"))) 
  
  # median of symptom onset to positive lab result is 2 days (mean 2.78 days)
  # use this to impute date of infection
  dataO |> 
    filter(NCOVdat1eposncov < as.Date("2021-12-01")) |> 
    mutate(SOtolab = as.integer(NCOVdat1eposncov - ZIE1eZiekteDt)) |> 
    filter(SOtolab >= -10, SOtolab < 20) |> 
    summarise(median_SOtolab = median(SOtolab))
  
  tmp <- dataO |> 
    mutate(#impute date of infection
      date_inf = if_else(!is.na(ZIE1eZiekteDt),
                         ZIE1eZiekteDt - incubation_period,
                         NCOVdat1eposncov - incubation_period - 2),
      week = ISOweek(date_inf),
      # put date (for plotting) at middle of the week
      date = ISOweek2date(paste0(week, "-4")),
      reinfected = case_when(grepl(Herinfectie, pattern = "Ja") ~ TRUE,
                             Herinfectie == "Nee" ~ FALSE,
                             TRUE ~ NA),
      vaccinated = case_when(grepl(Vaccinatie_status_ezd_Desc, pattern = "Geen") ~ FALSE,
                                  Vaccinatie_status_ezd_Desc == "Deels" ~ TRUE,
                                  Vaccinatie_status_ezd_Desc == "Volledig" ~ TRUE,
                                  Vaccinatie_status_ezd_Desc == "Booster" ~ TRUE,
                                  TRUE ~ NA))

  tmp <- dataO |> 
    mutate(#impute date of infection
      date_inf = if_else(!is.na(ZIE1eZiekteDt),
                         ZIE1eZiekteDt - incubation_period,
                         NCOVdat1eposncov - incubation_period - 2),
      week = ISOweek(date_inf),
      # put date (for plotting) at middle of the week
      date = ISOweek2date(paste0(week, "-4")),
      reinfected = case_when(grepl(Herinfectie, pattern = "Ja") ~ 1,
                             Herinfectie == "Nee" ~ 0,
                             TRUE ~ NA_real_),
      vaccinated = case_when(grepl(Vaccinatie_status_ezd_Desc, pattern = "Geen") ~ 0,
                             Vaccinatie_status_ezd_Desc == "Deels" ~ 0.5,
                             Vaccinatie_status_ezd_Desc == "Volledig" ~ 1,
                             Vaccinatie_status_ezd_Desc == "Booster" ~ 1,
                             TRUE ~ NA_real_))
  
    
  tmp |> 
    filter(date > as.Date("2020-03-01")) |> 
    group_by(date, reinfected) |> 
    count() |> 
    group_by(date) |> 
    mutate(frac = n/sum(n)) |> 
    ungroup() |> 
    select(-n) |> 
    pivot_wider(names_from = reinfected, values_from = frac, names_prefix = "reinfected_") |> 
    write_csv(file = file_reinf)

  tmp |> 
    filter(date > as.Date("2020-03-01")) |> 
    group_by(date, vaccinated) |> 
    count() |>
    group_by(date) |> 
    mutate(frac = n/sum(n)) |> 
    ungroup() |> 
    select(-n) |> 
    pivot_wider(names_from = vaccinated, values_from = frac, names_prefix = "vaccinated_") |> 
    write_csv(file = file_breakthrough)
  
  rm(report_date)
  rm(dataO)
  rm(tmp)

}

rm(file_reinf)
rm(file_breakthrough)
