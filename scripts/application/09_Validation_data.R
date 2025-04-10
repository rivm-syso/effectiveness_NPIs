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
  require(fst)
  
  report_date <- as.Date("2022-03-01")
  
  dataO <- tibble(filename = list.files("undisclosed_path/", full.names = TRUE)) |> 
    filter(grepl(filename, pattern = gsub(as.character(report_date), pattern = "-", replacement = "")))  |> 
    pull(filename) |> 
    read_fst(columns = c("OSIRISNR", "ZIE1eZiekteDt", "NCOVdat1eposncov", "MELGGDOntvDt", "MELRIVMOntvDt", "EIGENAARDesc", "Herinfectie", "Vaccinatie_status_ezd_Desc")) |> 
    as_tibble() |> 
    # only cases in European Netherlands
    filter(!(EIGENAARDesc %in% c("Sint Eustatius", "Bonaire", "Saba", "Sint Maarten", "Aruba", "Curaçao", "Curacao"))) 
  
  # median of symptom onset to positive lab result is 2 days (mean 2.78 days)
  # use this to impute date of infection
  median_SOtolab <- dataO |> 
    filter(NCOVdat1eposncov < as.Date("2021-12-01")) |> 
    mutate(SOtolab = as.integer(NCOVdat1eposncov - ZIE1eZiekteDt)) |> 
    filter(SOtolab >= -10, SOtolab < 20) |> 
    pull(SOtolab) |> 
    median()

  dataO <- dataO |> 
    mutate(#impute date of infection
      date_inf = if_else(!is.na(ZIE1eZiekteDt),
                         ZIE1eZiekteDt - parameters$incubation_period,
                         NCOVdat1eposncov - parameters$incubation_period - median_SOtolab),
      week = ISOweek(date_inf),
      # put date (for plotting) at middle of the week
      date = ISOweek2date(paste0(week, "-4")),
      # define infection type (reinfection or first infection)
      reinfected = case_when(grepl(Herinfectie, pattern = "Ja") ~ 1,
                             Herinfectie == "Nee" ~ 0,
                             TRUE ~ NA_real_),
      # weigh vaccination status none/partly/vaccinated as 0/0.5/1
      vaccinated = case_when(grepl(Vaccinatie_status_ezd_Desc, pattern = "Geen") ~ 0,
                             Vaccinatie_status_ezd_Desc == "Deels" ~ 0.5,
                             Vaccinatie_status_ezd_Desc == "Volledig" ~ 1,
                             Vaccinatie_status_ezd_Desc == "Booster" ~ 1,
                             TRUE ~ NA_real_))
  
  # local helper function to aggregate data by infection type or vaccination status
  # and write to file

  determine_validation_data <- function(dat, type, file_name) {

    dat |> 
      filter(date > as.Date("2020-03-01")) |> 
      group_by(date, !!sym(type)) |> 
      count() |> 
      group_by(date) |> 
      mutate(frac = n/sum(n)) |> 
      ungroup() |> 
      select(-n) |> 
      pivot_wider(names_from = !!sym(type), values_from = frac, names_prefix = paste0(type, "_")) |> 
      write_csv(file = file_name)
  }
   
  determine_validation_data(dat = dataO,
                            type = "reinfected",
                            file_name = file_reinf)
   
  
  determine_validation_data(dat = dataO,
                            type = "vaccinated",
                            file_name = file_breakthrough)
  

  rm(report_date)
  rm(dataO)
  rm(determine_validation_data)

}

rm(file_reinf)
rm(file_breakthrough)
