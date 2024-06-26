# READ IN MAIN DATA NEEDED #


# 1.  Main REDUCE data ########################################################
#  (one missing SUBJECT_ID)

dat_main_base <- readRDS(file.path(here() %>% dirname(), 
                                   'cs-transfer/adb_swe.RDS')) %>% 
  mutate(SUBJECT_ID = as.numeric(SUBJECT_ID))


# Reference file to merge with PDR data
dat_ref <- read_sas(file.path(here() %>% dirname(), 
                              'cs-transfer drugs/sos_pnr_subjectid.sas7bdat'))

# Main REDUCE data with linked LopNr
dat_main_lopnr <- inner_join(dat_ref, dat_main_base, 
                             by=c("SUBJECT_ID_PSEUDO"="SUBJECT_ID"))

# Identify the person with missing SUBJECT_ID in main data
dat_main_miss<- dat_main_base %>% 
  filter(is.na(SUBJECT_ID)) %>% 
  select(-SUBJECT_ID)

# Identify LopNr with no match
dat_ref_anti <- anti_join(dat_ref, dat_main_base, 
                          by=c("SUBJECT_ID_PSEUDO"="SUBJECT_ID"))

# Bring together missing SUBJECT_ID and LopNr with no match
dat_miss <- cbind(dat_ref_anti, dat_main_miss)

# Bring together main data with extra patient 
dat_main <- rbind(dat_miss, dat_main_lopnr)

# PDR data #####################################################################
# has 4789 LopNrs, but REDUCE has only 4788 ppl 
# only from start of 2017

dat_pdr <- read_sas(file.path(here() %>% dirname(), 
                              'cs-transfer drugs/ut_r_lmed_29075_2022.sas7bdat')) %>% 
  filter(LopNr != 2132) # remove LopNr that has no data in main reduce data

rm(dat_main_base, dat_ref, dat_main_lopnr, dat_main_miss, dat_ref_anti, dat_miss)


# Patient register data ########################################################
# only from start of 2017

temp_inpatient <- read_sas(file.path(here() |> dirname(),
                                     "cs-transfer drugs/ut_r_par_sv_29075_2022.sas7bdat"))
temp_outpatient <- read_sas(file.path(here() |> dirname(),
                                     "cs-transfer drugs/ut_r_par_ov_29075_2022.sas7bdat"))

# Primary diagnosis 

func_primary <- function(x) {
  select(x, LopNr, INDATUM, hdia) %>% 
    rename(ICD10 = hdia) 
}

dat_patient_primary <- lapply(list(temp_inpatient, temp_outpatient), func_primary)
names(dat_patient_primary) <- c("inpatient", "outpatient")

# All diagnoses

func_all <- function(x) {
  select(x, LopNr, INDATUM, contains("DIA"), contains("op"), -hdia, -contains("OPD")) %>%
    pivot_longer(cols = -c(LopNr, INDATUM)) %>% 
    select(-name) %>% 
    rename(ICD10 = value) %>% 
    filter(!(ICD10==""))
}

dat_patient_all <- lapply(list(temp_inpatient, temp_outpatient), func_all)
names(dat_patient_all) <- c("inpatient", "outpatient")

rm(func_all, func_primary, temp_inpatient, temp_outpatient)
