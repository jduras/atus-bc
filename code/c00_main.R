
library(readr)
library(magrittr)
library(tibble)
library(dplyr)
library(purrr)
library(tidyr)
library(broom)
library(stringr)
library(lubridate)
library(tidyquant)
library(lmtest)
library(ggplot2)
library(RColorBrewer)
library(mFilter)
library(stargazer)
library(Hmisc)
library(questionr)

theme_set(theme_bw())

# path to data, code, output
sdir_atus <- "C:/Drive/Dropbox/User/Academic/00Projects/01GMsearch/ATUS/code/R/"    # directory with main.R
ddir_atus <- "D:/Data/ATUS/data/unpacked/"                                          # directory where ATUS data is located
edir_atus <- "D:/Data/ATUS/extracts/AguiarHurstKarabarbounis2013AER/R/"             # directory for extracts
odir_atus <- "C:/Drive/Dropbox/User/Academic/00Projects/01GMsearch/ATUS/output/R/"  # directory for output

# first and last period to be used
tfirst <- 2003
tlast  <- 2017

demographics <- FALSE
workage <- "1865"
# workage <- "all"

# function for clustered errors
source(paste0(sdir_atus, "cXX_funs.R"))

# extract needed variables from ATUS cps, roster, respondent and activity files
source(paste0(sdir_atus, "c01_extract_atus_data.R"))
# merge yearly files
source(paste0(sdir_atus, "c02_merge_atus_data.R"))
# generate variables
source(paste0(sdir_atus, "c03_generate_variables.R"))
## aggregate statistics, simple analysis, time series plots
source(paste0(sdir_atus, "c04_aggregate_statistics.R"))
# extract CPS data: unemploment
source(paste0(sdir_atus, "c05_extract_cps_data.R"))
# extract NIPA data: GDP and personal income
source(paste0(sdir_atus, "c06_extract_nipa_data.R"))
## state level regressions
source(paste0(sdir_atus, "c07_state_lvl_analysis.R"))
## individuals level regressions
source(paste0(sdir_atus, "c08_individual_lvl_analysis.R"))
# plot estimated coefficients for income group categorical variable
source(paste0(sdir_atus, "c09_individual_lvl_lm_figs.R"))
# report results using stargazer
source(paste0(sdir_atus, "c10_individual_lvl_lm_tbls.R"))
