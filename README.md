# Effectiveness of NPIs

This repository contains the code developed and used for assessing the effectiveness of non-pharmaceutical interventions (NPIs) during the COVID-19 pandemic in the Netherlands. The application of this project is described in ["Estimating the effectiveness of non-pharmaceutical interventions against COVID-19 transmission in the Netherlands"](https://www.medrxiv.org/content/10.1101/2025.04.14.25325804v1), by [Jantien A Backer](mailto:jantien.backer@rivm.nl), Don Klinkenberg, Fuminari Miura and Jacco Wallinga. 

The repository consists of:

* `articles`: markdown files of the manuscript and supplementary material

* `data`: additional data sets for the application script

* `figures`: figures produced by the application script

* `R`: functions used in the application script

* `scripts`: code for the application script

All code has been written in the programming language [R](https://www.r-project.org/about.html); see [Requirements](#requirements) for detailed specification.

## Data

All data used for the application is publicly available for reproducibility. Data is either available as an online data set and downloaded when running the script, or provided as csv files in the `data` folder of this repository.

Online data sets:

* Population size distribution from [CBS](https://opendata.cbs.nl/#/CBS/nl/dataset/83482NED/table?dl=98643) (Statistics Netherlands): the number of persons by age and sex on the first day of every month, from 1 January 2016 until 1 November 2023. The data is aggregated in eight age groups (0-9, 10-19, 20-29, 30-39, 40-49, 50-59, 60-69, 70+) and linearly interpolated between the first day of each month, yielding the population size for each age group at each date.

* Effective reproduction number from [RIVM](https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/ed0699d1-c9d5-4436-8517-27eb993eab6e): case reproduction number based on COVID-19 hospitalisations (before 13 June 2020 and after 14 March 2023) or test-positive cases (from 13 June 2020 till 14 March 2023) with 95% confidence interval.

* Test-positive case data from [RIVM](https://data.rivm.nl/meta/srv/dut/catalog.search#/metadata/2c4357c8-76e4-4662-9574-1deb8a73f724): 8.6 million test-positive cases that were reported by the Public Health Services from 28 February 2020 until 31 March 2023; the age group and date of (imputed) symptom onset of each reported case is used.

* Genomic surveillance data from [RIVM](https://www.rivm.nl/corona/actueel/virusvarianten): weekly number of randomly sampled test-positive cases by SARS-CoV-2 variant from 30 November 2020 onwards; at the end of 2024 the data included almost 180,000 samples.

* [Oxford Stringency Index](https://github.com/OxCGRT): index between 0 and 100 indicating stringency of COVID-19 measures by country from 1 January 2020 until 31 December 2022; the average index per day in the Netherlands is used (see [Hale et al, 2021](https://doi.org/10.1038/s41562-021-01079-8)).


In the `data` folder:

* Serological survey data (`data_cumulative_infection_from_serosurvey.csv`) from [RIVM](https://www.rivm.nl/pienter-corona-onderzoek): the SARS-CoV-2 serological status of a representative sample of the Dutch population in 10 survey rounds from April 2020 until May 2023. The fraction of participants that have been infected at least once is determined by age group and survey round with 95% uncertainty. The code to calculate these aggregate data is provided in the script.

* Vaccination coverage data (`data_vaccination.csv`) from [RIVM](https://www.rivm.nl/corona/actueel/vaccinatiecijfers): the vaccination coverage of finishing the primary vaccination series (i.e. after one or two vaccine doses depending on the vaccine), per day and 10-year age group, from 10 January 2021 until 2 January 2022. The code to calculate these aggregate data is provided in the script.

* Validation data (`data_breakthrough_infections.csv` and `data_reinfections.csv`) from RIVM: For a part of the reported test-positive cases the vaccination status and the infection type (primary infection or reinfection) is known. The fractions of breakthrough infections and reinfections are calculated to compare with model results. The code to calculate these aggregate data is provided in the script.

## Usage

`scripts` contains the application script, and `R` contains functions that are loaded at the start of the script. The application script consists of several numbered R files that reproduce the results of the application manuscript when executed in order, as in `00_Main.R`.


## <a name = "requirements"></a> Requirements

The code has been developed and runs under the RIVM R-Studio servers.

```
R version 4.5.1 (2025-06-13) Great Square Root
Platform: x86_64-redhat-linux-gnu (64-bit)
Running under: Red Hat Cloud Infrastructure
```

Next to the R base packages, the following packages(_versions) were used

```
cbsodataR_1.1
ggnewscale_0.4.10
ISOweek_0.6-2
jsonlite_1.8.8
lubridate_1.9.3
nnet_7.3-20
patchwork_1.3.0
RColorBrewer_1.1-3
scales_1.3.0
tidyverse_2.0.0
zoo_1.8-12
```

## Funding

This study was funded by the Ministry of Health, Welfare and Sport (VWS) in the Netherlands. FM and JW received funding from European Union’s Horizon 2020 research and innovation programme - project ESCAPE (Grant agreement number 101095619). FM was supported by the Ministry of Education, Culture, Sports, Science and Technology, Japan (MEXT) to a project on Joint Usage/Research Center – Leading Academia in Marine and Environmental Pollution Research (LaMer). FM acknowledges fundings from Japan Society for the Promotion of Science (JSPS KAKENHI, 20J00793) and JST (JPMJPR23RA).

## Feedback

If you encounter a clear bug, please file an issue with a minimal reproducible example on GitHub.

