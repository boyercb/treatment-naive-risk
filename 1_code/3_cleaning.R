
# create analytic variables -----------------------------------------------

mesa <- 
  mesa %>%
  mutate(
    # baseline covariates
    
    # marital status
    married1 = if_else(marital1 == 1, 1, 0),
    
    # education
    educ_pri1 = if_else(educ1 %in% c(0, 1, 2), 1, 0),    # less than high school
    educ_sec1 = if_else(educ1 %in% c(3, 4, 5, 6), 1, 0), # high school or some college
    educ_col1 = if_else(educ1 %in% c(7, 8), 1, 0),       # bachelor's or postgraduate degree
    
    # race/ethnicity
    race_white1 = if_else(race1c == 1, 1, 0),
    race_black1 = if_else(race1c == 4, 1, 0),
    race_hisp1 = if_else(race1c == 3, 1, 0),
    race_asia1 = if_else(race1c == 2, 1, 0),
      
    # baseline employment
    employed1 = if_else(curjob1 %in% c(2, 3), 1, 0),    # full time
    retired1 = if_else(curjob1 %in% c(8, 9, 10), 1, 0), # retired
    
    # family history of cvd
    famhist1 = if_else(pmi1 == 1 | pstk1 == 1 | shrtatt1 == 1 | sstk1 == 1 | chrtatt1 == 1 | cstk1 == 1, 1, 0),
    
    # cancer history
    cancer1 = replace(cancer1, cancer1 == 9, NA),
    
    # self-reported high cholesterol
    hghchol1 = replace(hghchol1, hghchol1 == 9, NA),
    
    # diabetes 
    dm031c = as.numeric(dm031c > 0),
    
    # pregnancies 
    pregn1 = replace(pregn1, gender1 == 1, 0),
    pregn1 = replace(pregn1, preg1 == 0, 0),
    
    # hormone replacement therapy
    hrmrep1 = replace(hrmrep1, gender1 == 1, 0),
    
    # age of menopause 
    menoage1 = replace(menoage1, gender1 == 1, 0),
    menoage1 = replace(menoage1, mnpause1 == 0, 0),
    
    # birth control pills 
    bpillyr1 = replace(bpillyr1, gender1 == 1, 0),
    bpillyr1 = replace(bpillyr1, bcpills1 == 0, 0),
  
    # baseline risk using Framingham risk score
    ascvd_10yr_frs_risk1 = ascvd_10yr_frs(
      gender = if_else(gender1 == 1, "male", "female"), 
      age = age1c,
      totchol = chol1, 
      hdl = hdl1, 
      sbp = sbp1c,
      bp_med = htnmed1c, 
      smoker = cursmk1, 
      diabetes = dm031c
    ),
    
    # age started anti-hypertensives
    bphxage1 = replace(bphxage1, htnmed1c == 0, 0),
    
    # age started aspirin 
    aspsage1 = replace(aspsage1, aspirin1 == 0, 0),
    
    # baseline risk using PCE 
    # ascvd_10yr_pce_risk1 = ascvd_10yr_accaha(
    #   gender = if_else(gender1 == 1, 'female', 'male'), 
    #   race = if_else(race1c == 3, 'aa', 'white'),
    #   age = age1c,
    #   totchol = chol1, 
    #   hdl = hdl1, 
    #   sbp = sbp1c,
    #   bp_med = htnmed1c, 
    #   smoker = cursmk1, 
    #   diabetes = dm031c
    # ),
   
    # time-varying covariates
    
    # diabetes
    dm032c = as.numeric(dm032c > 0),
    dm033c = as.numeric(dm033c > 0),
    dm034c = as.numeric(dm034c > 0),
    dm035c = as.numeric(dm035c > 0),
    
    # calculate physical activity (NOTE: not measured at exam 4)
    exercise1 = exercm1c,
    exercise2 = rowSums(select(., q09wmcm2:q15cvcm2)),
    exercise3 = rowSums(select(., q09wmcm3:q15cvcm3)),
    exercise5 = rowSums(select(., q09wmcm5:q15cvcm5)),

    # calculate drinks per week
    dpw1 = replace(alcwk1c, alcohol1 == 0, 0),
    dpw2 = rowSums(select(., rwinewk2:liqwk2)),
    dpw3 = rowSums(select(., rwinewk3:liqwk3)),
    dpw4 = rowSums(select(., rwinewk4:liqwk4)),
    dpw5 = rowSums(select(., rwinewk5:liqwk5)),
    
    # employment status
    employed2 = if_else(empstat2 == 0, employed1, if_else(curjob2 %in% c(2, 3), 1, 0)), 
    employed3 = if_else(empstat3 == 0, employed1, if_else(curjob3 %in% c(2, 3), 1, 0)), 
    employed4 = if_else(empstat4 == 0, employed1, if_else(curjob4 %in% c(2, 3), 1, 0)), 
    employed5 = if_else(empstat5 == 0, employed1, if_else(curjob5 %in% c(2, 3), 1, 0)), 
    
    # retired
    retired2 = if_else(empstat2 == 0, employed1, if_else(curjob2 %in% c(8, 9, 10), 1, 0)), 
    retired3 = if_else(empstat3 == 0, employed1, if_else(curjob3 %in% c(8, 9, 10), 1, 0)), 
    retired4 = if_else(empstat4 == 0, employed1, if_else(curjob4 %in% c(8, 9, 10), 1, 0)), 
    retired5 = if_else(empstat5 == 0, employed1, if_else(curjob5 %in% c(8, 9, 10), 1, 0)), 
    
    # indicator for whether subject initiated statin therapy over follow up
    start_lipmed = pmax(lipid2c, lipid3c, lipid4c, lipid5c, na.rm = TRUE),
    start_lipmed_exam = case_when(
      start_lipmed == 1 & lipid2c == 1 ~ 2,
      start_lipmed == 1 & lipid3c == 1 ~ 3,
      start_lipmed == 1 & lipid4c == 1 ~ 4,
      start_lipmed == 1 & lipid5c == 1 ~ 5,
      TRUE ~ NA_real_
    ),
    start_lipmed_time = case_when(
      start_lipmed == 1 & lipid2c == 1 ~ e12dyc,
      start_lipmed == 1 & lipid3c == 1 ~ e13dyc,
      start_lipmed == 1 & lipid4c == 1 ~ e14dyc,
      start_lipmed == 1 & lipid5c == 1 ~ e15dyc,
      TRUE ~ NA_real_
    ),
    secondary_lipmed = if_else(cvda == 1 & start_lipmed_time >= cvdatt, 1, 0),
    primary_lipmed = if_else(start_lipmed == 1 & secondary_lipmed == 0, 1, 0),
      
    # indicator for whether subject initiated hypertensive therapy over follow up
    start_bpmed = pmax(htnmed2c, htnmed3c, htnmed4c, htnmed5c, na.rm = TRUE),
    start_bpmed_exam = case_when(
      start_bpmed == 1 & htnmed2c == 1 ~ 2,
      start_bpmed == 1 & htnmed3c == 1 ~ 3,
      start_bpmed == 1 & htnmed4c == 1 ~ 4,
      start_bpmed == 1 & htnmed5c == 1 ~ 5,
      TRUE ~ NA_real_
    ),
    start_bpmed_time = case_when(
      start_bpmed == 1 & htnmed2c == 1 ~ e12dyc,
      start_bpmed == 1 & htnmed3c == 1 ~ e13dyc,
      start_bpmed == 1 & htnmed4c == 1 ~ e14dyc,
      start_bpmed == 1 & htnmed5c == 1 ~ e15dyc,
      TRUE ~ NA_real_
    ),
    secondary_bpmed = if_else(cvda == 1 & start_bpmed_time >= cvdatt, 1, 0),
    primary_bpmed = if_else(start_bpmed == 1 & secondary_bpmed == 0, 1, 0),
    
    # indicator for whether subject initiated aspirin therapy over follow up
    start_aspmed = pmax(asacat2c, asacat3c, asacat4c, asacat5c, na.rm = TRUE),
    start_aspmed_exam = case_when(
      start_aspmed == 1 & asacat2c == 1 ~ 2,
      start_aspmed == 1 & asacat3c == 1 ~ 3,
      start_aspmed == 1 & asacat4c == 1 ~ 4,
      start_aspmed == 1 & asacat5c == 1 ~ 5,
      TRUE ~ NA_real_
    ),
    start_aspmed_time = case_when(
      start_aspmed == 1 & asacat2c == 1 ~ e12dyc,
      start_aspmed == 1 & asacat3c == 1 ~ e13dyc,
      start_aspmed == 1 & asacat4c == 1 ~ e14dyc,
      start_aspmed == 1 & asacat5c == 1 ~ e15dyc,
      TRUE ~ NA_real_
    ),
    secondary_aspmed = if_else(cvda == 1 & start_aspmed_time >= cvdatt, 1, 0),
    primary_aspmed = if_else(start_aspmed == 1 & secondary_aspmed == 0, 1, 0),
    
  )

# rename variables prior to dropping
mesa <- 
  mesa %>%
  rename(
    time2 = e12dyc,
    time3 = e13dyc,
    time4 = e14dyc,
    time5 = e15dyc
  ) 

# drop unused variables
mesa <-
  mesa %>%
  select(
    -starts_with("rwinewk"),
    -starts_with("wwinewk"),
    -starts_with("beerwk"),
    -starts_with("liqwk"),
    -matches("^q[0-9]+"),
    -starts_with("empstat"),
    -starts_with("curjob"),
    -alcwk1c,
    -alcohol1,
    -aspirin1,
    -bcpills1,
    -educ1,
    -exercm1c,
    -race1c,
    -marital1,
    -mnpause1,
    -pmi1,
    -preg1,
    -pstk1,
    -shrtatt1,
    -sstk1,
    -chrtatt1,
    -cstk1
  )

baseline_vars <- mesa %>% select(matches(".*1c?$")) %>% names()


# create long version -----------------------------------------------------

# pivot to longer format
mesa_long <- 
  mesa %>%
  pivot_longer(
    cols = matches(".*[2-5]c?$"),
    names_to = c("variable", "exam"),
    names_pattern = "(.*)([2-5])"
  ) %>%
  pivot_wider(
    names_from = "variable", 
    values_from = "value"
  )


# clean outcomes ----------------------------------------------------------

# mesa_long <-
#   mesa_long %>%
#   mutate(
#     
#   )