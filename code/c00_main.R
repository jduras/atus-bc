
library(readr)
library(magrittr)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(broom)
library(stringr)
library(tis)
library(lubridate)
library(tidyquant)
library(timetk)
library(janitor)
library(lmtest)
library(ggplot2)
library(ggridges)
library(lemon)
library(plotly)
library(scales)
library(RColorBrewer)
library(mFilter)
library(stargazer)
library(Hmisc)
library(questionr)

library(furrr)
library(tictoc)

theme_set(theme_bw())

# path to data, code, output
sdir_atus <- "C:/Drive/Dropbox/User/Academic/00Projects/02ATUS/code/"   # directory with main.R
ddir_atus <- "D:/Data/ATUS/data/unpacked/"                              # directory where ATUS data is located
edir_atus <- "D:/Data/ATUS/extracts/AguiarHurstKarabarbounis2013AER/"   # directory for extracts
odir_atus <- "C:/Drive/Dropbox/User/Academic/00Projects/02ATUS/output/" # directory for output

ddir_cps <- "D:/Data/CPS/data/basic/unpacked/"


# first and last period to be used
tfst <- 2003
tlst <- 2018

demographics <- FALSE
workage <- "1865"
# workage <- "all"

# function for clustered errors
source(str_c(sdir_atus, "cXX_funs.R"))

# choice <- menu(choices = c("parallel computation", "sequential computation"),
#                title = "Choose approach:")

choice <- 1

if (choice == 1) {
    parallel <- TRUE
    plan(multiprocess)
} else {
    parallel <- FALSE
    plan(sequential)
}

# US states codes
df_state_codes <-
    read_csv(str_c(ddir_atus, "other/state_codes_gestfips.csv")) %>%
    mutate(state = state_name %>% str_replace_all(fixed(" "), "") %>% str_to_lower())

# recession dates
rec_dates <-
    nberDates() %>%
    as_tibble() %>%
    mutate_all(list(. %>% as.character() %>% as.yearmon("%Y%m%d"))) %>%
    as_tibble()

quandl_api_key("DLk9RQrfTVkD4UTKc7op")

# extract needed variables from ATUS cps, roster, respondent and activity files
source(str_c(sdir_atus, "c01_extract_atus_data.R"))
# merge yearly files
source(str_c(sdir_atus, "c02_merge_atus_data.R"))
# generate variables
source(str_c(sdir_atus, "c03_generate_variables.R"))
## aggregate statistics, simple analysis, time series plots
source(str_c(sdir_atus, "c04_aggregate_statistics.R"))
# extract CPS data: unemploment
source(str_c(sdir_atus, "c05_extract_cps_data.R"))
# extract NIPA data: GDP and personal income
source(str_c(sdir_atus, "c06_extract_nipa_data.R"))
## state level regressions
source(str_c(sdir_atus, "c07_state_lvl_analysis.R"))
## individuals level regressions
source(str_c(sdir_atus, "c08_individual_lvl_analysis.R"))
# plot estimated coefficients for income group categorical variable
source(str_c(sdir_atus, "c09_individual_lvl_lm_figs.R"))
# report results using stargazer
source(str_c(sdir_atus, "c10_individual_lvl_lm_tbls.R"))
