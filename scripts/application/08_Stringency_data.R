################################################################################
#
# Load Oxford stringency index 
# - select average index in the Netherlands
# - see https://github.com/OxCGRT
# - define periods with constant integer stringency index
# 
################################################################################

#https://github.com/OxCGRT
OxfordSI <- read_csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-dataset/main/data/OxCGRT_compact_national_v1.csv",
                     col_select = c(CountryName, Date, StringencyIndex_Average))


# determine periods of constant integer OxSI by period
OxSI_data <- OxfordSI |> 
  filter(CountryName == "Netherlands") |> 
  transmute(date = as.Date(as.character(Date), format = "%Y%m%d"),
            OxSI = round(StringencyIndex_Average)) |> 
  mutate(lagSI = lag(OxSI, default = 0), # add column with OxSI of day before
         changeOxSI = (lagSI != OxSI), # change when OxSI differs from day before
         period = cumsum(changeOxSI)) |> # number periods by summing the changes
  select(-lagSI)


rm(OxfordSI)
