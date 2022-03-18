
# variable lists ----------------------------------------------------------

baseline_vars <- c(
  "gender1",
  "married1",
  "educ_sec1",
  "educ_col1",
  "race_black1",
  "race_hisp1",
  "race_asia1",
  "employed1",
  "retired1",
  "evsmk1",
  "cesd1c",
  "chrbu61c",
  "discry1c",
  "emot1c",
  "hassl1c",
  "splang1c",
  "splanx1c",
  "pregn1",
  "bpillyr1",
  "menoage1",
  "nprob1c",
  "famhist1",
  "agatpm1c",
  "ecglvh1c",
  "crp1",
  "il61"
)

log_transform <- c(
  "agatpm1c",
  "crp1",
  "il61",
  "exercise"
)

baseline_vars_long <- c(
  "bl_age",
  "bl_dm03",
  "bl_htn",
  "bl_cursmk",
  "bl_waistcm",
  "bl_htnmed",
  "bl_asacat",
  "bl_diur",
  "bl_diabins",
  "bl_anydep",
  "bl_vasoda",
  "bl_anara",
  "bl_hinone",
  "bl_sbp",
  "bl_dbp",
  "bl_ldl",
  "bl_hdl",
  "bl_trig",
  "bl_dpw",
  "bl_exercise"
)

# create list of time-varying covariates
tv_vars <- c(
  "age",
  "dm03",
  "htn",
  "cursmk",
  "waistcm",
  "htnmed",
  "asacat",
  "diur",
  "diabins",
  "anydep",
  "vasoda",
  "anara",
  "hinone",
  "sbp",
  "dbp",
  "ldl",
  "hdl",
  "trig", 
  "dpw",
  "exercise"
)

tv_vars_wide <- c(
  "age1c",
  "dm031c",
  "htn1c",
  "bmi1c",
  "htcm1",
  "wtlb1",
  "hipcm1",
  "waistcm1",
  "asacat1c",
  "lipid1c",
  "htnmed1c",
  "diabins1",
  "anydep1c",
  "anara1c",
  "vasoda1c",
  "income1",
  "hinone1",
  "cursmk1",
  "sbp1c",
  "dbp1c",
  "chol1",
  "hdl1",
  "ldl1",
  "trig1",
  "age2c",
  "dm032c",
  "htn2c",
  "bmi2c",
  "htcm2",
  "wtlb2",
  "hipcm2",
  "waistcm2",
  "asacat2c",
  "lipid2c",
  "htnmed2c",
  "diur2c",
  "diabins2",
  "anydep2c",
  "anara2c",
  "vasoda2c",
  "income2",
  "hinone2",
  "cursmk2",
  "sbp2c",
  "dbp2c",
  "chol2",
  "hdl2",
  "ldl2",
  "trig2",
  "age3c",
  "dm033c",
  "htn3c",
  "bmi3c",
  "htcm3",
  "wtlb3",
  "hipcm3",
  "waistcm3",
  "asacat3c",
  "lipid3c",
  "htnmed3c",
  "diur3c",
  "diabins3",
  "anydep3c",
  "anara3c",
  "vasoda3c",
  "income3",
  "hinone3",
  "cursmk3",
  "sbp3c",
  "dbp3c",
  "chol3",
  "hdl3",
  "ldl3",
  "trig3",
  "age4c",
  "dm034c",
  "htn4c",
  "bmi4c",
  "htcm4",
  "wtlb4",
  "hipcm4",
  "waistcm4",
  "asacat4c",
  "lipid4c",
  "htnmed4c",
  "diur4c",
  "diabins4",
  "anydep4c",
  "anara4c",
  "vasoda4c",
  "hinone4",
  "cursmk4",
  "sbp4c",
  "dbp4c",
  "chol4",
  "hdl4",
  "ldl4",
  "trig4",
  "age5c",
  "dm035c",
  "htn5c",
  "bmi5c",
  "htcm5",
  "wtlb5",
  "hipcm5",
  "waistcm5",
  "asacat5c",
  "lipid5c",
  "htnmed5c",
  "diur5c",
  "diabins5",
  "anydep5c",
  "anara5c",
  "vasoda5c",
  "income5",
  "hinone5",
  "cursmk5",
  "sbp5c",
  "dbp5c",
  "chol5",
  "hdl5",
  "ldl5",
  "trig5",
  "exercise1",
  "exercise2",
  "exercise3",
  "exercise4",
  "exercise5",
  "dpw1",
  "dpw2",
  "dpw3",
  "dpw4",
  "dpw5",
  "employed2",
  "employed3",
  "employed4",
  "employed5",
  "retired2",
  "retired3",
  "retired4",
  "retired5"
  )

outcome_vars <- c(
  "cvdatt",
  "cvda",
  "dth"
)

time_vars_wide <- c(
  "time2",
  "time3",
  "time4",
  "time5"
)

ref_vars <- c(
  "race_white1",
  "educ_pri1"
)

cont_vars <- c(
  "cesd1c", 
  "chrbu61c", 
  "discry1c",
  "emot1c",
  "hassl1c",
  "splang1c",
  "splanx1c",
  "pregn1", 
  "bpillyr1",
  "menoage1",
  "nprob1c",
  "agatpm1c",
  "crp1",
  "il61",
  "age",
  "waistcm",
  "sbp", 
  "dbp",
  "ldl",
  "hdl",
  "trig",
  "dpw",
  "exercise"
)

tv_adj_vars <- c(
  "lag1_dm03",
  "lag1_htn",
  "lag1_cursmk",
  "lag1_waistcm",
  "htnmed",
  "asacat",
  "diur",
  "diabins",
  "anydep",
  "vasoda",
  "anara",
  "lag1_hinone",
  "lag1_sbp",
  "lag1_dbp",
  "lag1_ldl",
  "lag1_hdl",
  "lag1_trig", 
  "lag1_dpw",
  "lag1_exercise"
)


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
    exercise4 = exercise3,
    exercise5 = rowSums(select(., q09wmcm5:q15cvcm5)),

    # calculate drinks per week
    rwinewk2 = replace(rwinewk2, curalc2 == 0, 0), 
    rwinewk3 = replace(rwinewk3, curalc3 == 0, 0), 
    rwinewk4 = replace(rwinewk4, curalc4 == 0, 0), 
    rwinewk5 = replace(rwinewk5, curalc5 == 0, 0), 
    wwinewk2 = replace(wwinewk2, curalc2 == 0, 0), 
    wwinewk3 = replace(wwinewk3, curalc3 == 0, 0), 
    wwinewk4 = replace(wwinewk4, curalc4 == 0, 0), 
    wwinewk5 = replace(wwinewk5, curalc5 == 0, 0),
    beerwk2 = replace(beerwk2, curalc2 == 0, 0), 
    beerwk3 = replace(beerwk3, curalc3 == 0, 0), 
    beerwk4 = replace(beerwk4, curalc4 == 0, 0), 
    beerwk5 = replace(beerwk5, curalc5 == 0, 0),
    liqwk2 = replace(liqwk2, curalc2 == 0, 0), 
    liqwk3 = replace(liqwk3, curalc3 == 0, 0), 
    liqwk4 = replace(liqwk4, curalc4 == 0, 0), 
    liqwk5 = replace(liqwk5, curalc5 == 0, 0),
    
    dpw1 = replace(alcwk1c, alcohol1 == 0, 0),
    dpw2 = rowSums(select(., rwinewk2:liqwk2), na.rm = T),
    dpw3 = rowSums(select(., rwinewk3:liqwk3), na.rm = T),
    dpw4 = rowSums(select(., rwinewk4:liqwk4), na.rm = T),
    dpw5 = rowSums(select(., rwinewk5:liqwk5), na.rm = T),
    
    # current smoker
    cursmk2 = replace(cursmk2, smkstat2 == 0, 0),
    cursmk3 = replace(cursmk3, smkstat3 == 0, 0),
    cursmk4 = replace(cursmk4, smkstat4 == 0, 0),
    cursmk5 = replace(cursmk5, smkstat5 == 0, 0),
    
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
    mesaid,
    all_of(time_vars_wide),
    all_of(outcome_vars), 
    all_of(baseline_vars),
    all_of(ref_vars),
    all_of(tv_vars_wide),
    prebase,
    lipid1c
  ) 

