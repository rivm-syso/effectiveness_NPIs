################################################################################
#
# Get population data from CBS
# - table 83482NED contains the population numbers from January 2016 until November
#   2023 by age and month
# - download all or specific months
# - download males and females, no distinction by migration background or generation
# 
################################################################################

get_cbs_population <- function(periods = NULL) {
  
  metadata <- cbs_get_meta("83482NED")
  
  periods = if(length(periods) == 0) metadata$Perioden$Key else periods
  
  cbs_get_data(
    "83482NED",
    Perioden = periods,
    Migratieachtergrond = "T001040", # Total, no distinction in migration background
    Generatie = "T001040", # Total, no distinction in migration generation
    Geslacht = metadata$Geslacht |> 
      filter(Title %in% c("Mannen", "Vrouwen")) |> # males and females
      pull(Key),
    Leeftijd = metadata$Leeftijd |> 
      filter(CategoryGroupID == 2) |> # age by year (i.e. not aggregated in age group)
      pull(Key)
  ) |>
    cbs_add_label_columns() |>
    # recode age to integers
    transmute(
      age = as.integer(gsub(Leeftijd_label, pattern = " [^0-9]+", replacement = "")),
      sex = if_else(Geslacht_label == "Mannen", "M", "F") |> 
        as.factor(),
      population = BevolkingOpDeEersteVanDeMaand_1,
      period = Perioden)
  
}
