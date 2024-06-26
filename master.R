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

# Load data --------------------------------------------------------------------
source(here("01_dat_load.R"))

# Calculate the length of each BB dispensation after randomization--------------
    # includes free text algorithm to identify how many pills per day
    # PROBLEMS: a couple of very long lengths, but likely due to non-specific info in free text 
                # (check after algorithm update)
source(here("02_cr_pdr_bblength.R"))


