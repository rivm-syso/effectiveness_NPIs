################################################################################
#
# Load open data of variants:
# - weekly number of samples by variant
# - see https://data.rivm.nl/covid-19/COVID-19_varianten.html
# 
################################################################################

# read in open data
variant_data <- read_csv2("https://data.rivm.nl/covid-19/COVID-19_varianten.csv",
                          col_select = c(Date_of_statistics_week_start, Variant_name, Variant_cases, Sample_size))

variant_data <- variant_data |> 
  # samples on average taken in the middle of the week (3.5 days later than week start)
  # samples represent infections of 7 days ago (5 days incubation + 2 days to sample)
  # -> shift week_start by +3.5-5-2 = -3.5 days to obtain infection date
  mutate(date = round(Date_of_statistics_week_start - 3.5)) |> 
  # remove records of variants with less than 50 samples (i.e. Lambda and Mu)
  group_by(Variant_name) |> 
  filter(sum(Variant_cases) > 50) |> 
  # combine all Omicron and Recombinants samples into 1 Omicron variant (will not be used in final analysis)
  mutate(Variant_name = if_else(Variant_name == "Recombinants", "Omicron", Variant_name)) |> 
  group_by(date, Variant_name, Sample_size) |> 
  summarise(Variant_cases = sum(Variant_cases)) |> 
  # count number of samples without variant name (most are wild type)
  group_by(date) |> 
  mutate(WildType = Sample_size - sum(Variant_cases)) |> 
  ungroup() |> 
  # remove spurious wild type samples after dominance of Alpha
  mutate(WildType = if_else(date > ymd("2021-05-01"), 0, WildType)) |> 
  distinct() |> 
  # rearrange data
  pivot_wider(names_from = Variant_name, values_from = Variant_cases) |> 
  pivot_longer(cols = !contains(c("date", "Sample")), names_to = "variant", values_to = "count") |> 
  mutate(variant = factor(variant, levels = c("WildType", "Alpha", "Beta", "Gamma", "Delta", "Omicron"))) 

