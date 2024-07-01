################################################################################

# To do 
#     - Don't censored if they have indication/contra 
#     - Make sure the above is set on month i.e., if have indication in same month,
#       then not censored

################################################################################


# 1 - ADHERENCE IN BB ASSIGNED GROUP ###########################################

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


# A. Set censor date at 180 days after assignment if did not dispense ----------
      # BB within 180 days of assignment to BB at study inclusion
temp_bb_primary <- temp_bb_firstdisp |> 
  mutate(pp_censor_date = INCLUSION_DATE + grace) |> 
  mutate(censor = ifelse(is.na(EDATUM), 1, 
                         ifelse(EDATUM > pp_censor_date, 1,
                                0))) |>
  filter(censor == 1) |> 
  select(LopNr, pp_censor_date)
  
  
# B. Set censor date at end date of last dispensation + 180 days----------------
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

# C. Bring back together 
temp_bb <- rbind(temp_bb_primary, temp_bb_secondary)

# Check have everyone 
if (nrow(dat_main |> filter(STUDY_GROUP != "Beta blocker")) == nrow(temp_bb)) {
  "ERROR: N ROWS DON'T MATCH IN BB ASSIGNED"}

rm(temp_bb_alldispense, temp_bb_firstdisp, temp_bb_primary, temp_bb_secondary)

  
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
  
  # Manually add 1 day to dispensations that happen on same day as assignment so
  # they are in "month 1" 
  mutate(EDATUM = ifelse(timetofirst == 0, EDATUM+1, EDATUM)) |> 
  mutate(EDATUM = as.Date(EDATUM, origin="1970-01-01")) |> 
  
  mutate(pp_censor_date = EDATUM) |> 
  select(LopNr, EDATUM)

  
  