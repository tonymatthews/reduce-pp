################################################################################

# To do 
#     - Don't censor if they have indication in no bb group
#     - Make sure the above is set on month i.e., if have indication in same month,
#       then not censored

################################################################################


# 1 - ADHERENCE IN BB ASSIGNED GROUP ###########################################

# A. Create all datasets needed ------------------------------------------------------------

# Create data frame with all assigned to BB and all following dispensations
temp_bb_alldispense <- select(dat_main, LopNr, INCLUSION_DATE,
                             STUDY_GROUP, BETA_BLOCK_S_DOSE,
                             BETA_BLOCK_T_DOSE) |> 
  filter(STUDY_GROUP == "Beta blocker") |> 
  left_join(cr_bblength, by = "LopNr") 
  

# Create data frame with all assigned to BB and first following dispensation
temp_bb_firstdisp <- select(dat_main, LopNr, INCLUSION_DATE,
                            STUDY_GROUP, BETA_BLOCK_S_DOSE,
                            BETA_BLOCK_T_DOSE) |> 
  filter(STUDY_GROUP == "Beta blocker") |> 
  left_join(cr_bblength, by = "LopNr") |> 
  group_by(LopNr) |> 
  arrange(EDATUM) |> 
  filter(row_number()==1) |> 
  ungroup() |> 
  mutate(timetofirst = as.integer(EDATUM-INCLUSION_DATE))                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                           
# TIME TO FIRST DISPENSATION
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
# 0.00    0.00    1.00   16.49    2.00 1428.00      48 

# B. Set censor date at 180 days after assignment if did not dispense --------------------------------
      # BB within 180 days of assignment to BB at study inclusion
temp_bb_primary <- temp_bb_firstdisp |> 
  mutate(pp_censor_date = INCLUSION_DATE + grace) |> 
  mutate(censor = ifelse(is.na(EDATUM), 1, 
                         ifelse(EDATUM > pp_censor_date, 1,
                                0))) |>
  filter(censor == 1) |> 
  select(LopNr, pp_censor_date)
  
  
# C. Set censor date at end date of last dispensation + 180 days------------------------------------
      # if dispensed within 180 days of assignment
temp_bb_secondary <- temp_bb_alldispense |> 
  anti_join(select(temp_bb_primary, LopNr), by = "LopNr") |>
  mutate(stop_date = EDATUM + length + grace) |>
  group_by(LopNr) |>
  arrange(EDATUM) |>
  mutate(n = row_number()) |>
  mutate(stop = ifelse(stop_date < lead(EDATUM), 1, 0)) |>
  mutate(stop = ifelse(is.na(stop), 1, stop)) |>
  ungroup() |>

  # keep first dispensation and dispensation before stopping
  group_by(LopNr, stop) |>
  mutate(min_n = min(n)) |>
  ungroup() |>
  filter(n == min_n) |>
  select(-c(n, stop, min_n)) |>

  # assemble in one row with start and end date
  group_by(LopNr) |>
  mutate(pp_censor_date = max(stop_date)) |>
  filter(row_number() == 1) |> 
  ungroup() |>
  select(LopNr, pp_censor_date)

# D. Identify those that would not be censored if have contraindication ------------------------ 

# Define ICD10 for contraindications
bradycardia <- c("R001", "I495")
avblock <- c("I441", "I442", "I443")
hypotension <- c("I95")
syncope <- c("R559", "T671")
asthma <- c("J45", "J46")
copd <- c("J44")
pacemaker <- c("FPE20", "FPE26", "TFP00")

patientreg <- list(bradycardia, avblock, hypotension, syncope,
                   asthma, copd, pacemaker)

# Identify first record of any contraindication after to inclusion
temp_contraind_patreg <- lapply(patientreg, function(x) {
  func_extract_patient(temp_bb, x,
                       prim = T, sec = T, inpat = T, outpat = T) |> 
    left_join(select(dat_main, LopNr, INCLUSION_DATE), by = "LopNr") |> 
    filter(INDATUM >= INCLUSION_DATE) |> 
    group_by(LopNr) |> 
    arrange(INDATUM, .by_group = T) |> 
    filter(row_number() == 1) |> 
    ungroup() |> 
    mutate(INDATUM = ifelse(INDATUM == INCLUSION_DATE, INDATUM+1, INDATUM)) |> 
    select(LopNr, INDATUM)
}) |>  reduce(full_join, by = "LopNr") |>  
  mutate(contraind_date = pmin(INDATUM, INDATUM.x, INDATUM.y, INDATUM.x.x, INDATUM.y.y,
                       INDATUM.x.x.x, INDATUM.y.y.y,
                       na.rm = T)) |> 
  mutate(contraind_date = as.Date(contraind_date, origin="1970-01-01")) |> 
  select(LopNr, contraind_date)

# Check only one record per person
if (!(length(unique(temp_contraind_patreg$LopNr)) == nrow(temp_contraind_patreg))) {
  "ERROR: MORE THAN ONE RECORD PER PERSON FOR FIRST CONTRAINDICTION"
}

# E. Bring all back together & create time variable --------------------------------------------

temp_bb <- rbind(temp_bb_primary, temp_bb_secondary) |> 
  # calculate time of censoring
  left_join(select(dat_main, LopNr, INCLUSION_DATE), by = "LopNr") |> 
  mutate(pp_censor_time = 
           as.numeric(ceiling((pp_censor_date - INCLUSION_DATE)/time_cut))) |> 
  select(-pp_censor_date) |> 
  # Merge in contraindication dates
  left_join(temp_contraind_patreg, by = "LopNr") |> 
  # calculate time of contraindication 
  mutate(contraind_time = 
           as.numeric(ceiling((contraind_date - INCLUSION_DATE)/time_cut))) |> 
  select(-contraind_date) |> 
  # Update pp_censor_time to missing if contraind is same time or before pp_censor
  mutate(pp_censor_time = ifelse(contraind_time <= pp_censor_time & !is.na(contraind_time),
                                 NA, pp_censor_time))


# F. Checks and clean ---------------------------------------------------------------------------------
# Check have everyone in the main  
if (!(nrow(dat_main |> filter(STUDY_GROUP == "Beta blocker")) == nrow(temp_bb))) {
  "ERROR: N ROWS DON'T MATCH IN BB ASSIGNED"}

# Remove all not needed
rm(temp_bb_alldispense, temp_bb_firstdisp, temp_bb_primary, temp_bb_secondary,
   bradycardia, avblock, hypotension, syncope, asthma, copd, pacemaker, 
   patientreg, temp_contraind_patreg)



# 2 - ADHERENCE IN NO BB ASSIGNED GROUP #########################################  
  
# Create data frame with all assigned to no BB and first following dispensation
temp_nobb_firstdisp <- select(dat_main, LopNr, INCLUSION_DATE,
                              STUDY_GROUP, BETA_BLOCK_S_DOSE,
                              BETA_BLOCK_T_DOSE) |> 
  filter(STUDY_GROUP == "No beta blocker") |> 
  left_join(cr_bblength, by = "LopNr") |> 
  group_by(LopNr) |> 
  arrange(EDATUM) |> 
  filter(row_number()==1) |> 
  ungroup() |> 
  mutate(timetofirst = as.integer(EDATUM-INCLUSION_DATE)) |> 
  # TIME TO FIRST DISPENSATION
  # Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
  # 0.0    14.0   101.5   345.6   570.0  2023.0    1774
  
# A. Set censor date at first dispensation of a BB
  mutate(EDATUM = ifelse(timetofirst == 0, EDATUM+1, EDATUM)) |> # Manually add 1 day to dispensations that happen on same day as assignment so they are in "month 1" 
  mutate(EDATUM = as.Date(EDATUM, origin="1970-01-01")) |> 
  mutate(pp_censor_date = EDATUM) |> 
  
  select(LopNr, EDATUM)

# B. Not censored if have contraindication prior to censoring date

# 3 - BRING TOGETHER AND CREATE "MONTH" VARIABLE

  
  