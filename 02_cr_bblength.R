################################################################################
# Purpose: Calculate dates of non-adherence for participants of REDUCE
################################################################################

#  1. BB (bisprolol or metoprolol) in PDR that happen after study inclusion #####
temp_dat_pdr_bb <- select(dat_pdr, LopNr, ATC, FDATUM, EDATUM, ANTAL, antnum, 
                     styrknum, DOSER)  |> 
  filter(ATC %in% c("C07AB07", "C07AB02")) |> 
  inner_join(select(dat_main, LopNr, INCLUSION_DATE), by = "LopNr") |> 
  filter(FDATUM >= INCLUSION_DATE) |> 
  select(-INCLUSION_DATE) |> 
  
  
  
  
# 2. algorithm to convert prescription free text into number of tablets per day ##
  mutate(
  DOSERlower = gsub(",", ".", tolower(DOSER)),
  DOSERlower = gsub("½", "0.5", tolower(DOSERlower)),
  DOSERlower = gsub("1/2", "0.5", tolower(DOSERlower)),
  dos = # 1 tablett 1 gång
    ifelse(str_detect(
      DOSERlower, "1 tab|1tab|1 tbl|1 st tablett|1st tablett|1 depot|1 st|1 dagl|en tabl|en kapsel|
                       en t dagligen|1 film|1x1|1 om dagen|1\\+0\\+0|i depot|1  tabl|1  st|1 till frukost|1 pill")==T &
        str_count(
          DOSERlower, "1 tab|1tab|1 tbl|1 st tablett|1st tablett|1 depot|1 st|1 dagl|en tabl|en kapsel|
                           en t dagligen|1 film|1x1|1 om dagen|1\\+0\\+0|i depot|1  tabl|1  st|1 till frukost|1 pill")==1, 1, NA),
  dos = # 1 tablett 2 gånger
    ifelse(str_detect(
      DOSERlower, "1 tab|1tab|1 st tablett|1st tablett|1 depot|1 st|1 dagl|en tabl|en kapsel|
                     en t dagligen|1 film|1x1|1 om dagen|1\\+0\\+0|i depot|1  tabl|1  st|1 till frukost|1st morgon")==T &
        str_count(
          DOSERlower, "1 tab|1tab|1 st tablett|1st tablett|1 depot|1 st|1 dagl|en tabl|en kapsel|
                         en t dagligen|1 film|1x1|1 om dagen|1\\+0\\+0|i depot|1  tabl|1  st|1 till frukost|1st morgon")==2, 2, dos),
  dos = # 2 tablett 1 gånger
    ifelse(str_detect(
      DOSERlower, "2 tab|2 st tablett|2 depottablett|2 st depottablett|2 st|1x2|1\\+0\\+1")==T &
        str_count(
          DOSERlower, "2 tab|2 st tablett|2 depottablett|2 st depottablett|2 st|1x2|1\\+0\\+1")==1, 2, dos),
  dos = # 2 tablett 2 gånger
    ifelse(str_detect(
      DOSERlower, "2 tab|2 st tablett|2 depottablett|2 st depottablett|2 st|1x2|1\\+0\\+1")==T &
        str_count(
          DOSERlower, "2 tab|2 st tablett|2 depottablett|2 st depottablett|2 st|1x2|1\\+0\\+1")==2, 4, dos) ,
  dos = # 0.5 tablett 1 gång
    ifelse(str_detect(
      DOSERlower, "0.5 tab|0.5 depottablett|0.5 st|halv tablett|0.5 t|0.5x1|0.5 dagl|0.5st|
                       0.5   tablett|0.5  tablett|0.5    tablett|halv  tablett dagligen")==T &
        str_count(
          DOSERlower, "0.5 tab|0.5 depottablett|0.5 st|halv tablett|halv  tablett dagligen|0.5 t|0.5x1|0.5 dagl|0.5st|
                           0.5   tablett|0.5  tablett|0.5     tablett")==1, 0.5, dos),
  dos = # 0.5 tablett 2 gånger
    ifelse(str_detect(
      DOSERlower, "0.5 tab|0.5 depottablett|0.5 st|halv tablett|0.5 t|0.5x1|0.5 dagl|0.5st|
                       0.5   tablett|0.5  tablett|1st morgon")==T &
        str_count(
          DOSERlower, "0.5 tab|0.5 depottablett|0.5 st|halv tablett|0.5 t|0.5x1|0.5 dagl|0.5st|
                           0.5   tablett|0.5  tablett|1st morgon")==2, 1, dos),
  dos = # 1.5 tablett 1 gång
    ifelse(str_detect(
      DOSERlower, "1.5 tab|1.5 st tab|1.5 depottablett|1.5 st depottablett")==T &
        str_count(
          DOSERlower, "1.5 tab|1.5 st tab|1.5 depottablett|1.5 st depottablett")==1, 1.5, dos),
  dos = # 1.5 tablett 2 gånger
    ifelse(str_detect(
      DOSERlower, "1.5 tab|1.5 st tab|1.5 depottablett|1.5 st depottablett")==T &
        str_count(
          DOSERlower, "1.5 tab|1.5 st tab|1.5 depottablett|1.5 st depottablett")==2, 3, dos),
  dos = # 3 tablett 1 gång
    ifelse(str_detect(
      DOSERlower, "3 tabletter|3 depottabletter")==T &
        str_count(
          DOSERlower, "3 tabletter|3 depottabletter")==1, 3, dos),
  dos = # 4 tablett 1 gång
    ifelse(str_detect(
      DOSERlower, "4 tabletter")==T &
        str_count(
          DOSERlower, "4 tabletter")==1, 4, dos),
  dos = # fix 0.25st
    ifelse(str_detect(
      DOSERlower, "0.25 tabletter på morgonen för hjärtat")==T &
        str_count(
          DOSERlower, "0.25 tabletter på morgonen för hjärtat")==1, 0.5, dos),
  dos = # fix 0.5st
    ifelse(str_detect(
      DOSERlower, "0.5   tablett på morgonen. för hjärtat.")==T &
        str_count(
          DOSERlower, "0.5   tablett på morgonen. för hjärtat.")==1, 0.5, dos),
  dos = # fix 2st
    ifelse(str_detect(
      DOSERlower, "1 tablett 2 gånger dagligen|1 depottablett 2 gång|1  tablett 2 gånger daglige|1 tbl 2 gånger om dagen|1 tablett 2 gånger om dagen.|1 tablett på morgonen. 1 tablett på kvällen|1 tabletter vid behov. Max 1 st per dygn Tas vid behov på kvällen vid känningar av hjärtklappning.|1 depottablett 2 gånger dagligen|1 st tablett 2 gånger dagligen|sedan 1 tablett 2 gånger dagligen|2 depottabletter 1 gång dagligen. ordinationsorsak: för migrän och blodtrycket. = 50 mg. börjar att öka till 1.5 tabeltt och däref|1 st depottablett 2 gånger dagligen|1 tablett på morgonen och en på kvällen. för hjärtrytmen.|1 depottablett kl 08:00 och 1 depottablett kl 20:00 i 1 dygn därefter 1.5 depottabletter kl 08:00 och 1 depottablett kl 20:|1 depottablett kl 08:00 och 1 depottablett kl 20:00 tills vidare oralt 1 tablett morgon och kväll för hjärta och blodtry|en tab.morgon och kväll")==T &
        str_count(
          DOSERlower, "1 tablett 2 gånger dagligen|1 depottablett 2 gång|1  tablett 2 gånger daglige|1 tbl 2 gånger om dagen|1 tablett 2 gånger om dagen.|1 tablett på morgonen. 1 tablett på kvällen|1 tabletter vid behov. max 1 st per dygn Tas vid behov på kvällen vid känningar av hjärtklappning.|1 depottablett 2 gånger dagligen|1 st tablett 2 gånger dagligen|sedan 1 tablett 2 gånger dagligen|2 depottabletter 1 gång dagligen. ordinationsorsak: för migrän och blodtrycket. = 50 mg. börjar att öka till 1.5 tabeltt och däref|1 st depottablett 2 gånger dagligen|1 tablett på morgonen och en på kvällen. för hjärtrytmen.|1 depottablett kl 08:00 och 1 depottablett kl 20:00 i 1 dygn därefter 1.5 depottabletter kl 08:00 och 1 depottablett kl 20:|1 depottablett kl 08:00 och 1 depottablett kl 20:00 tills vidare oralt 1 tablett morgon och kväll för hjärta och blodtry|en tab.morgon och kväll")==1, 2, dos),
  dos = # fix 3st
    ifelse(str_detect(
      DOSERlower, "2  tabletter  på morgon och 1 tablett på kvällen|2 tabletter kl. 08. 1 tabletter kl. 17. dagligen|2 tabletter kl. 08. 1 tabletter kl. 17. Dagligen|2 tabletter kl 08 och 1 tablett kl 20 hjärtmedicin|2 tabletter på morgonen och 1 på kvällen enligt ny ordination. för hjärtrytmen")==T &
        str_count(
          DOSERlower, "2  tabletter  på morgon och 1 tablett på kvällen|2 tabletter kl. 08. 1 tabletter kl. 17. dagligen|2 tabletter kl. 08. 1 tabletter kl. 17. Dagligen|2 tabletter kl 08 och 1 tablett kl 20 hjärtmedicin|2 tabletter på morgonen och 1 på kvällen enligt ny ordination. för hjärtrytmen")==1, 3, dos),
  dos = # fix 1st
    ifelse(str_detect(
      DOSERlower, "1 tablett på morgonen kan ökas med 1 tablett om fortsatt hjärtklappning. max 4 tabletter per dygn|0.5-1 tablett vid behov. max 1 st per dygn mot hjärtklappning|0.5 tablett 2 gånger dagligen|1 tablett kl 08:00 tills vidare|0.5 depottablett 2 gånger dagligen|1 tablett på morgonen 0.5 tablett i 1 vecka. därefter 1 tablett dagligen|första veckan en halv tablett dagligen. 2a april ökar du till en tablett dagligen.|första veckan en halv tablett varje morgon. Därefter en tablett varjemorgon. hjärtskyddande.|första veckan en halv tablett. därefter en hel tablett dagligen.|1st morgon|0.5 st tablett 2 gånger dagligen. för hjärtrytmen|1 depottablett 1 gång dagligen. ordinationsorsak: sänker pulsen. du kan prova ta 0.5 tabl morgon och kväll för att få upp vilopulsen nå|1 tabletter vid behov. max 2 st per dygn vid behov vid huvudvärk enligt ordination från falu lasarett|0.5-1 tabletter vid behov. max 1 st per dygn mot hjärtklappning|1 depottablett till frukost tills vidare oralt förebygger komplikationer efter hjärtinfarkt 1 tablett på morgonen för hjärta|en tab på morgonen. för hjärtat.|0.5 tablett på morgonen i 5 dagar. därefter 0.5 tablett på morgonen och 0.5 tablett på kvällen. mot hjärtklappning.|i tablett dagligen mot högt blodtryck|en t dagligen|en om dan")==T &
        str_count(
          DOSERlower, "1 tablett på morgonen kan ökas med 1 tablett om fortsatt hjärtklappning. max 4 tabletter per dygn|0.5-1 tablett vid behov. max 1 st per dygn mot hjärtklappning|0.5 tablett 2 gånger dagligen|1 tablett kl 08:00 tills vidare|0.5 depottablett 2 gånger dagligen|1 tablett på morgonen 0.5 tablett i 1 vecka. därefter 1 tablett dagligen|första veckan en halv tablett dagligen. 2a april ökar du till en tablett dagligen.|första veckan en halv tablett varje morgon. Därefter en tablett varjemorgon. hjärtskyddande.|första veckan en halv tablett. därefter en hel tablett dagligen.|1st morgon|0.5 st tablett 2 gånger dagligen. för hjärtrytmen|1 depottablett 1 gång dagligen. ordinationsorsak: sänker pulsen. du kan prova ta 0.5 tabl morgon och kväll för att få upp vilopulsen nå|1 tabletter vid behov. max 2 st per dygn vid behov vid huvudvärk enligt ordination från falu lasarett|0.5-1 tabletter vid behov. max 1 st per dygn mot hjärtklappning|1 depottablett till frukost tills vidare oralt förebygger komplikationer efter hjärtinfarkt 1 tablett på morgonen för hjärta|en tab på morgonen. för hjärtat.|0.5 tablett på morgonen i 5 dagar. därefter 0.5 tablett på morgonen och 0.5 tablett på kvällen. mot hjärtklappning.|i tablett dagligen mot högt blodtryck|en t dagligen|en om dan")==1, 1, dos),
  dos = # fix 5st
    ifelse(str_detect(
      DOSERlower, "3 tabletter på morgonen. 2 tabletter till kvällen för hjärta och blodtryck")==T &
        str_count(
          DOSERlower, "3 tabletter på morgonen. 2 tabletter till kvällen för hjärta och blodtryck")==1, 5, dos),
  dos = # fix 1.5st
    ifelse(str_detect(
      DOSERlower, "1 tablett på morgonen. 0.5 tabletter till kvällen|1 depottablett kl 08:00 och 0.5 depottablett|0.5 tabletter på morgonen. 1 tablett på eftermiddagen|1 tablett kl. 08. 0.5 tablett kl. 20|1 tablett på morgonen och 0.5 tablett på kvällen|1 tablett kl 08:00 och 0.5 tablett kl 20:00 tills vidare mot förhöjt blodtryck|1 depottablett kl. 08. 0.5 depottablett kl. 20. dagligen|1 depottablett klockan 08. 0.5 depottablett klockan 20. dagligen|1 tablett kl 08:00 och 0.5 tablett kl 20:00 tills vidare oralt mot kärlkramp|1 st tablett till frukost. 0.5 st tablett till kvällen/natten. för hjärtrytmen. hjärtat och mot högt blodtryck.|0.5 tablett 3 gånger dagligen. ordinationsorsak: hjärtat och rytmen villkor förmån är uppfylld")==T &
        str_count(
          DOSERlower, "1 tablett på morgonen. 0.5 tabletter till kvällen|1 depottablett kl 08:00 och 0.5 depottablett|0.5 tabletter på morgonen. 1 tablett på eftermiddagen|1 tablett kl. 08. 0.5 tablett kl. 20|1 tablett på morgonen och 0.5 tablett på kvällen|1 tablett kl 08:00 och 0.5 tablett kl 20:00 tills vidare mot förhöjt blodtryck|1 depottablett kl. 08. 0.5 depottablett kl. 20. dagligen|1 depottablett klockan 08. 0.5 depottablett klockan 20. dagligen|1 tablett kl 08:00 och 0.5 tablett kl 20:00 tills vidare oralt mot kärlkramp|1 st tablett till frukost. 0.5 st tablett till kvällen/natten. för hjärtrytmen. hjärtat och mot högt blodtryck.|0.5 tablett 3 gånger dagligen. ordinationsorsak: hjärtat och rytmen villkor förmån är uppfylld")==1, 1.5, dos),
  dos = # fix 2.5st
    ifelse(str_detect(
      DOSERlower, "2.5 tabletter på morgonen")==T &
        str_count(
          DOSERlower, "2.5 tabletter på morgonen")==1, 2.5, dos),
  dos = # fix 1.75st
    ifelse(str_detect(
      DOSERlower, "ökad dos från")==T &
        str_count(
          DOSERlower, "ökad dos från")==1, 1.75, dos)
) |> 
  select(-DOSER,) |> 
  
# 3. Calculate dispensation length #############################################
  
  mutate(length = (ANTAL*antnum)/dos) |> 

  # Fix some obvious scenarios where length is missing due to missing dos (tablets per day)
  mutate(length = case_when(
    (is.na(length) & antnum == 250 & ANTAL == 0.056) ~ antnum*ANTAL, # clearly a pack of 14 and 1 per day
    (is.na(length) & antnum == 250 & ANTAL == 0.112) ~ antnum*ANTAL, # clearly a pack of 28 and 1 per day
    (is.na(length) & antnum == 100 & ANTAL == 3) ~ 100, # DOSER "1+2" which interpreted as 3 per day
    is.na(length) ~ antnum*ANTAL, # assume 1 per day for all others - mainly "take as prescribed" (only appox. 100)
    TRUE ~ length) 
  )  |>


  
# 4. Fix more than one dispensation on the same day
  # this happens for 2 reasons: 
            # 1) postive pack num followed by negative pack number 
                      # (dispensation immediately returned)
            # 2) several different pack with different length or strength, 
                      # intended to be taken at the same time OR intended to be taken one after another
  
  # identify the number of dispensations each day
  group_by(LopNr, EDATUM) |>
  mutate(num_disp_day = n()) |>
  ungroup() |> 

  # Identify if at least one dispensation was negative if there was more 
      # than one dispensation on the same day
  mutate(negative = ifelse(length < 0, 1, 0)) |> 
  group_by(LopNr, EDATUM) |> 
  mutate(negative_sameday = ifelse(num_disp_day>1, max(negative), 0)) |> 
  ungroup() |> 
    
  # identify where more than one dispensation on the same day, but none of 
      # them -ve 
  mutate(nonnegative_sameday = ifelse(num_disp_day>1 & negative_sameday==0, 1, 0)) 
  

# 4a. At least one negative on a day when there was more than one dispensation, 
      # SOLUTUON: consolidate the lengths (some cancel out, some have +ve, -ve, +ve on same day)
temp_bb_neg <- temp_dat_pdr_bb |> 
  filter(negative_sameday == 1) |> 
  group_by(LopNr, EDATUM) |> 
  summarise(length = sum(length)) |> 
  ungroup() |> 
  filter(length > 0)

# 4b. More than one dispensation on a day, # but none were -ve
temp_bb_nonneg <- temp_dat_pdr_bb |> 
  filter(nonnegative_sameday == 1) |> 
  select(-c(ATC, FDATUM, num_disp_day, negative, negative_sameday, nonnegative_sameday)) |> 
  
  # first, identify if there was the same or different length
  group_by(LopNr, EDATUM) |> 
  mutate(max_length = max(length)) |> 
  mutate(match1 = ifelse(length == max_length, 1, 0)) |> 
  mutate(match_length = min(match1)) |> 
  ungroup() |> 
  select(-c(match1, max_length)) |> 

  # then identify if they were same or different strength
  group_by(LopNr, EDATUM) |> 
  mutate(max_styrk = max(styrknum)) |> 
  mutate(match1 = ifelse(styrknum == max_styrk, 1, 0)) |> 
  mutate(match_strength = min(match1)) |> 
  ungroup() |> 
  select(-c(match1, max_styrk))


# 4b1. Further separate when there was more than one dispensation on a day, none were
      # -ve, and they were different lengths OR same length and same strength
      # SOLUTION: add the lengths together 
temp_bb_nonneg_difflength <- temp_bb_nonneg |> 
  filter((match_length == 0) | (match_length == 1 & match_strength == 1)) |> 
  group_by(LopNr, EDATUM) |>
  summarise(length = sum(length)) |> 
  ungroup()

# 4b2. Further separate when there was more than one dispensation on a day, none were
      # -ve, and they were same length and different strength 
      # SOLUTION: assume taken together, so only take the length of one
                  # Unless the one scenario where there are 4 on the same day, then
                  # There are two consecutive batches (so multiply length by 2)
temp_bb_nonneg_samelength_differentstrength <- temp_bb_nonneg |> 
  filter(match_length == 1 & match_strength ==0) |> 
  group_by(LopNr, EDATUM) |> 
  mutate(N= n()) |> 
  mutate(n = row_number()) |> 
  filter(n==1) |> 
  mutate(length = ifelse(N==4, length*2, length)) |> 
  select(LopNr, EDATUM, length)


# Bring back together temp_bb_nonneg
temp_bb_nonneg <- rbind(temp_bb_nonneg_difflength, temp_bb_nonneg_samelength_differentstrength)
rm(temp_bb_nonneg_difflength, temp_bb_nonneg_samelength_differentstrength)

# Bring all back together and do final -ve processing
cr_bblength <- temp_dat_pdr_bb |> 
  
  # remove -ve same day and non -ve same day (processed above)
  filter(negative_sameday == 0 & nonnegative_sameday == 0) |> 
  select(LopNr, EDATUM, length) |> 
  
  # bring back the negative same day rows (where didn't fully cancel out)
  rbind(temp_bb_neg) |> 

  # Still some -ves (some happened a day or more after initial dispensation)
  # If returned some days after a dispensation, subtract from first 
  # dispensation (this only happens to 5 people, and max time is 15 days)
  group_by(LopNr) |>
  arrange(EDATUM, -length) |> 
  mutate(edatum_1 = ifelse(length < 0, lag(EDATUM), as.Date("", origin="1970-01-01"))) |> 
  ungroup() |> 
  mutate(EDATUM = ifelse(!is.na(edatum_1), edatum_1, EDATUM)) |> 
  mutate(EDATUM = as.Date(EDATUM, origin="1970-01-01")) |>  
  group_by(LopNr, EDATUM) |> 
  summarise(length = sum(length)) |>
  ungroup() |>  
  filter(!length == 0) |> 

  # bring back non-negative same day rows
  rbind(temp_bb_nonneg) 

rm(temp_dat_pdr_bb, temp_bb_neg, temp_bb_nonneg)






  


  
  


  


  
