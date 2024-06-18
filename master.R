################################################################################
# Purpose: master file
# Author: Anthony Matthews
# First created: 18/06/2022
# GitHub repo: https://github.com/tonymatthews/reduce-pp
################################################################################

# Packages ---------------------------------------------------------------------
library(here)
library(haven)
library(dplyr)
library(purrr)
library(tableone)
library(splitstackshape)
library(tidyr)
library(ggplot2)
library(survival)
library(survminer)
library(survey)
library(tictoc)
library(pammtools)
library(stringr)
library(gridExtra)
library(WeightIt)
library(cobalt)
library(splines)
library(rlang)
library(labelled)
library(berryFunctions)
library(ggsci)
library(rsample)
library(lobstr)
library(rms)
library(furrr)
library(future.apply)
library(parallel)

# Data -------------------------------------------------------------------------
dat_main <- readRDS(file.path(here() %>% dirname(), 'cs-transfer/adb_swe.RDS'))
dat_pdr <- read_sas(file.path(here() %>% dirname(), 'cs-transfer drugs/ut_r_lmed_29075_2022.sas7bdat'))


