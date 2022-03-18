

# create long version -----------------------------------------------------

mesa <- 
  mesa %>%
  mutate(
    exercise4 = exercise3,
    
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
      start_lipmed == 1 & lipid2c == 1 ~ time2,
      start_lipmed == 1 & lipid3c == 1 ~ time3,
      start_lipmed == 1 & lipid4c == 1 ~ time4,
      start_lipmed == 1 & lipid5c == 1 ~ time5,
      TRUE ~ NA_real_
    ),
    stop_lipmed_time = case_when(
      start_lipmed_exam == 2 & lipid3c == 0 ~ time3,
      start_lipmed_exam == 2 & lipid3c == 1 & lipid4c == 0 ~ time4,
      start_lipmed_exam == 2 & lipid3c == 1 & lipid4c == 1 & lipid5c == 0 ~ time5,
      start_lipmed_exam == 3 & lipid4c == 0 ~ time4,
      start_lipmed_exam == 3 & lipid4c == 1 & lipid5c == 0 ~ time5,
      start_lipmed_exam == 4 & lipid5c == 0 ~ time5,
      TRUE ~ NA_real_
    ),
    start_lipmed_time_alt = case_when(
      start_lipmed == 1 & lipid2c == 1 ~ 1,
      start_lipmed == 1 & lipid3c == 1 ~ time2 + 1,
      start_lipmed == 1 & lipid4c == 1 ~ time3 + 1,
      start_lipmed == 1 & lipid5c == 1 ~ time4 + 1,
      TRUE ~ NA_real_
    ),
    secondary_lipmed = if_else(cvda == 1 & start_lipmed_time >= cvdatt, 1, 0),
    secondary_lipmed_alt = if_else(cvda == 1 & start_lipmed_time_alt >= cvdatt, 1, 0),
    primary_lipmed = if_else(start_lipmed == 1 & secondary_lipmed == 0, 1, 0),
    primary_lipmed_alt = if_else(start_lipmed == 1 & secondary_lipmed_alt == 0, 1, 0),
    
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
      start_bpmed == 1 & htnmed2c == 1 ~ time2,
      start_bpmed == 1 & htnmed3c == 1 ~ time3,
      start_bpmed == 1 & htnmed4c == 1 ~ time4,
      start_bpmed == 1 & htnmed5c == 1 ~ time5,
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
      start_aspmed == 1 & asacat2c == 1 ~ time2,
      start_aspmed == 1 & asacat3c == 1 ~ time3,
      start_aspmed == 1 & asacat4c == 1 ~ time4,
      start_aspmed == 1 & asacat5c == 1 ~ time5,
      TRUE ~ NA_real_
    ),
    secondary_aspmed = if_else(cvda == 1 & start_aspmed_time >= cvdatt, 1, 0),
    primary_aspmed = if_else(start_aspmed == 1 & secondary_aspmed == 0, 1, 0)
   )


# create target trials  ---------------------------------------------------

# trial 1: follow-up starts at exam 2 and ends 10-years later
trial1 <- 
  mesa %>%
  filter(lipid1c == 0 & cvdatt > time2) %>%
  mutate(
    tx = lipid2c,
    cvda = replace(cvda, cvdatt - time2 > 3652, 0),
    cvdatt = replace(cvdatt - time2, cvdatt - time2 > 3652, 3652),
    cvdatt = replace(cvdatt, cvdatt < 3652 & cvda == 0 & dth == 1, 3652),
    cvdatt = round(cvdatt / (365/12)),
    cens = if_else(cvdatt < 3652 & cvda == 0 & dth == 0, 1, 0),
    age = age2c,
    cursmk = cursmk1,
    dm03 = dm031c,
    htn = htn1c,
    waistcm = waistcm1,
    htnmed = htnmed2c,
    asacat = asacat2c,
    diur = diur2c,
    diabins = diabins2,
    anydep = anydep2c,
    anara = anara2c,
    vasoda = vasoda2c,
    hinone = hinone1,
    sbp = sbp1c,
    dbp = dbp1c,
    ldl = ldl1,
    hdl = hdl1,
    trig = trig1,
    dpw = dpw1,
    chol = chol1,
    exercise = exercise1
  ) 

coxfit <- coxph(
  formula = Surv(cvdatt, cvda) ~ gender1 + age + I(age^2) + sbp + chol + hdl + 
    htnmed + cursmk + dm03 + age:sbp + age:chol + age:hdl + age:cursmk + htnmed:sbp,
  data = trial1
)

trial1 <- 
  trial1 %>%
  mutate(
    risk = compute_risk_score(coxfit, trial1, time = 120)
  )

# trial 2: follow-up starts at exam 3, but still ends 10-years after exam 2
trial2 <-
  mesa %>%
  filter(pmax(lipid1c, lipid2c, na.rm = TRUE) == 0 & cvdatt > time3 & !is.na(time2)) %>%
  mutate(
    tx = lipid3c,
    cvda = replace(cvda, cvdatt - time2 > 3652, 0),
    cvdatt = replace(cvdatt - time2, cvdatt - time2 > 3652, 3652),
    cvdatt = replace(cvdatt, cvdatt < 3652 & cvda == 0 & dth == 1, 3652),
    cvdatt = round((cvdatt - (time3 - time2)) / (365/12)),
    cens = if_else(cvdatt < 3652 & cvda == 0 & dth == 0, 1, 0),
    age = age3c,
    cursmk = cursmk2,
    dm03 = dm032c,
    htn = htn2c,
    waistcm = waistcm2,
    htnmed = htnmed3c,
    asacat = asacat3c,
    diur = diur3c,
    diabins = diabins3,
    anydep = anydep3c,
    anara = anara3c,
    vasoda = vasoda3c,
    hinone = hinone2,
    sbp = sbp2c,
    dbp = dbp2c,
    ldl = ldl2,
    hdl = hdl2,
    trig = trig2,
    dpw = dpw2,
    chol = chol2,
    exercise = exercise2
  ) 

trial2 <- 
  trial2 %>%
  mutate(
    risk = compute_risk_score(coxfit, trial2, time = 120)
  )


# trial 3: follow-up starts at exam 4, but still ends 10-years after exam 2
trial3 <-
  mesa %>%
  filter(pmax(lipid1c, lipid2c, lipid3c, na.rm = TRUE) == 0 & cvdatt > time4 & !is.na(time2)) %>%
  mutate(
    tx = lipid4c,
    cvda = replace(cvda, cvdatt - time2 > 3652, 0),
    cvdatt = replace(cvdatt - time2, cvdatt - time2 > 3652, 3652),
    cvdatt = replace(cvdatt, cvdatt < 3652 & cvda == 0 & dth == 1, 3652),
    cvdatt = round((cvdatt - (time4 - time2)) / (365/12)),
    cens = if_else(cvdatt < 3652 & cvda == 0 & dth == 0, 1, 0),
    age = age4c,
    cursmk = cursmk3,
    dm03 = dm033c,
    htn = htn3c,
    waistcm = waistcm3,
    htnmed = htnmed4c,
    asacat = asacat4c,
    diur = diur4c,
    diabins = diabins4,
    anydep = anydep4c,
    anara = anara4c,
    vasoda = vasoda4c,
    hinone = hinone3,
    sbp = sbp3c,
    dbp = dbp3c,
    ldl = ldl3,
    hdl = hdl3,
    trig = trig3,
    dpw = dpw3,
    chol = chol3,
    exercise = exercise3
  ) 

trial3 <- 
  trial3 %>%
  mutate(
    risk = compute_risk_score(coxfit, trial3, time = 120)
  )

# trial 4: follow-up starts at exam 5, but still ends 10-years after exam 2
trial4 <-
  mesa %>%
  filter(pmax(lipid1c, lipid2c, lipid3c, lipid4c, na.rm = TRUE) == 0 &  cvdatt > time5 & !is.na(time2)) %>%
  mutate(
    tx = lipid5c,
    cvda = replace(cvda, cvdatt - time2 > 3652, 0),
    cvdatt = replace(cvdatt - time2, cvdatt - time2 > 3652, 3652),
    cvdatt = replace(cvdatt, cvdatt < 3652 & cvda == 0 & dth == 1, 3652),
    cvdatt = round((cvdatt - (time5 - time2)) / (365/12)),
    cens = if_else(cvdatt < 3652 & cvda == 0 & dth == 0, 1, 0),
    age = age5c,
    cursmk = cursmk4,
    dm03 = dm034c,
    htn = htn4c,
    waistcm = waistcm4,
    htnmed = htnmed5c,
    asacat = asacat5c,
    diur = diur5c,
    diabins = diabins5,
    anydep = anydep5c,
    anara = anara5c,
    vasoda = vasoda5c,
    hinone = hinone4,
    sbp = sbp4c,
    dbp = dbp4c,
    ldl = ldl4,
    hdl = hdl4,
    trig = trig4,
    dpw = dpw4,
    chol = chol4,
    exercise = exercise4,
  ) 

trial4 <- 
  trial4 %>%
  mutate(
    risk = compute_risk_score(coxfit, trial4, time = 120)
  )


# combine nested trials into single dataset -------------------------------

# stack trials together
trials <- 
  bind_rows(trial1, trial2, trial3, trial4, .id = "trial") %>%
  mutate(
    trial2 = if_else(trial == 2, 1, 0),
    trial3 = if_else(trial == 3, 1, 0),
    trial4 = if_else(trial == 4, 1, 0)
  ) %>%
  select(
    starts_with("trial"),
    mesaid,
    tx,
    all_of(baseline_vars),
    all_of(tv_vars),
    cursmk,
    risk,
    race_white1,
    educ_pri1,
    cvdatt,
    cvda,
    cens
  ) 

# create person-time dataset
trials_pt <- 
  trials %>%
  uncount(cvdatt, .id = "time") %>%
  group_by(trial, mesaid) %>%
  mutate(cvda = replace(cvda, time != max(time), 0)) %>%
  ungroup()


# set up nested target trials ---------------------------------------------

create_long_trial <- function(data, exam_no) {
  lt <- data %>%
    pivot_longer(
      cols = matches(".*[2-5]c?$"),
      names_to = c("variable", "exam"),
      names_pattern = "(.*)([2-5])"
    ) %>%
    pivot_wider(
      names_from = "variable", 
      values_from = "value"
    ) %>%
    group_by(mesaid) %>%
    mutate(
      # create version of outcome where start of follow up is exam 1
      cvdatt_alt = cvdatt,
      
      # create version of outcome where start of follow up is exam 2
      cvdatt = cvdatt - first(time),
      
      # administratively censor at 10-years
      cvda = replace(cvda, cvdatt > 3652, 0),
      cvdatt = replace(cvdatt, cvdatt > 3652, 3652),
      cvdatt = replace(cvdatt, cvdatt < 3652 & cvda == 0 & dth == 1, 3652),
      cens = if_else(cvdatt < 3652 & cvda == 0 & dth == 0, 1, 0),
      
      cvdatt_alt = replace(cvdatt_alt, cvdatt_alt > 3652, 3652),
      
      # change to person-months
      cvdatt = round(cvdatt / (365/12)),
      cvdatt_alt = round(cvdatt_alt / (365/12)),
      
      # re-center time
      time = time - first(time),
      
      # change time to person months
      time = round(time / (365/12)),
      
      # lags
      across(all_of(c(tv_vars, "chol", "lipid")), list(lag1 = lag), n = 1, default = -999, .names = "{.fn}_{.col}"), 
      lag1_age = if_else(lag1_age == -999, age1c, lag1_age),
      lag1_dm03 = if_else(lag1_dm03 == -999, dm031c, lag1_dm03),
      lag1_htn = if_else(lag1_htn == -999, htn1c, lag1_htn),
      lag1_cursmk = if_else(lag1_cursmk == -999, cursmk1, lag1_cursmk),
      lag1_waistcm = if_else(lag1_waistcm == -999, waistcm1, lag1_waistcm),
      lag1_htnmed = if_else(lag1_htnmed == -999, htnmed1c, lag1_htnmed),
      lag1_asacat = if_else(lag1_asacat == -999, asacat1c, lag1_asacat),
      # lag1_diur = replace(lag1_diur, lag1_diur == -999, diur1c),
      lag1_diabins = if_else(lag1_diabins == -999, diabins1, lag1_diabins),
      lag1_anydep = if_else(lag1_anydep == -999, anydep1c, lag1_anydep),
      lag1_vasoda = if_else(lag1_vasoda == -999, vasoda1c, lag1_vasoda),
      lag1_anara = if_else(lag1_anara == -999, anara1c, lag1_anara),
      lag1_hinone = if_else(lag1_hinone == -999, hinone1, lag1_hinone),
      lag1_sbp = if_else(lag1_sbp == -999, sbp1c, lag1_sbp),
      lag1_dbp = if_else(lag1_dbp == -999, dbp1c, lag1_dbp),
      lag1_chol = if_else(lag1_chol == -999, chol1, lag1_chol),
      lag1_lipid = if_else(lag1_lipid == -999, lipid1c, lag1_lipid),
      lag1_ldl = if_else(lag1_ldl == -999, ldl1, lag1_ldl),
      lag1_hdl = if_else(lag1_hdl == -999, hdl1, lag1_hdl),
      lag1_trig = if_else(lag1_trig == -999, trig1, lag1_trig), 
      lag1_dpw = if_else(lag1_dpw == -999, dpw1, lag1_dpw),
      lag1_exercise = if_else(lag1_exercise == -999, exercise1, lag1_exercise)
    ) %>%
    filter(time < cvdatt) %>%
    filter(exam >= exam_no) %>%
    mutate(
      counts = if_else(exam == last(exam), cvdatt - time, lead(time, 1) - time),
      bl_age = first(age),
      bl_dm03 = first(lag1_dm03),
      bl_htn = first(lag1_htn),
      bl_cursmk  = first(lag1_cursmk),
      bl_waistcm = first(lag1_waistcm),
      bl_htnmed = first(htnmed),
      bl_asacat = first(asacat),
      bl_diur = first(diur),
      bl_diabins = first(diabins),
      bl_anydep = first(anydep),
      bl_vasoda = first(vasoda),
      bl_anara = first(anara),
      bl_hinone = first(lag1_hinone),
      bl_sbp = first(lag1_sbp),
      bl_dbp = first(lag1_dbp),
      bl_chol = first(lag1_chol),
      bl_ldl = first(lag1_ldl),
      bl_hdl = first(lag1_hdl),
      bl_trig = first(lag1_trig), 
      bl_dpw = first(lag1_dpw),
      bl_exercise = first(lag1_exercise),
      age = replace(age, exam == exam_no, 0),
      lag1_dm03 = replace(lag1_dm03, exam == exam_no, 0),
      lag1_htn = replace(lag1_htn, exam == exam_no, 0),
      lag1_cursmk = replace(lag1_cursmk, exam == exam_no, 0),
      lag1_waistcm = replace(lag1_waistcm, exam == exam_no, 0),
      htnmed = replace(htnmed, exam == exam_no, 0),
      asacat = replace(asacat, exam == exam_no, 0),
      diur = replace(diur, exam == exam_no, 0),
      diabins = replace(diabins, exam == exam_no, 0),
      anydep = replace(anydep, exam == exam_no, 0),
      vasoda = replace(vasoda, exam == exam_no, 0),
      anara = replace(anara, exam == exam_no, 0),
      lag1_hinone = replace(lag1_hinone, exam == exam_no, 0),
      lag1_sbp = replace(lag1_sbp, exam == exam_no, 0),
      lag1_dbp = replace(lag1_dbp, exam == exam_no, 0),
      lag1_ldl = replace(lag1_ldl, exam == exam_no, 0),
      lag1_hdl = replace(lag1_hdl, exam == exam_no, 0),
      lag1_trig = replace(lag1_trig, exam == exam_no, 0),
      lag1_dpw = replace(lag1_dpw, exam == exam_no, 0),
      lag1_exercise = replace(lag1_exercise, exam == exam_no, 0)
    ) %>%
    ungroup()
  
  lt <- lt %>%
    mutate(
      risk = compute_risk_score(
        coxfit, 
        newdata = data.frame(
          gender1 = lt$gender1,
          age = lt$bl_age,
          chol = lt$bl_chol, 
          hdl = lt$bl_hdl, 
          sbp = lt$bl_sbp,
          htnmed = lt$bl_htnmed, 
          cursmk = lt$bl_cursmk, 
          dm03 = lt$bl_dm03
        ), 
        time = 120
      )  
    )
}

# trial 1: follow-up starts at exam 2 and ends 10-years later
trial1_long <- 
  mesa %>%
  filter(lipid1c == 0 & cvdatt > time2) %>%
  mutate(tx = lipid2c) %>%
  create_long_trial(exam_no = 2)


# trial 2: follow-up starts at exam 3, but still ends 10-years after exam 2
trial2_long <-
  mesa %>%
  filter(pmax(lipid1c, lipid2c, na.rm = TRUE) == 0 & cvdatt > time3) %>%
  mutate(tx = lipid3c) %>%
  create_long_trial(exam_no = 3)

# trial 3: follow-up starts at exam 4, but still ends 10-years after exam 2
trial3_long <-
  mesa %>%
  filter(pmax(lipid1c, lipid2c, lipid3c, na.rm = TRUE) == 0 & cvdatt > time4) %>%
  mutate(tx = lipid4c) %>%
  create_long_trial(exam_no = 4)

# trial 4: follow-up starts at exam 5, but still ends 10-years after exam 2
trial4_long <-
  mesa %>%
  filter(pmax(lipid1c, lipid2c, lipid3c, lipid4c, na.rm = TRUE) == 0 &  cvdatt > time5) %>%
  mutate(tx = lipid5c) %>%
  create_long_trial(exam_no = 5)


# combine nested trials into single dataset -------------------------------

# stack trials together
trials_long <- 
  bind_rows(trial1_long, trial2_long, trial3_long, trial4_long, .id = "trial") %>%
  mutate(
    trial2 = if_else(trial == 2, 1, 0),
    trial3 = if_else(trial == 3, 1, 0),
    trial4 = if_else(trial == 4, 1, 0)
  ) %>%
  select(
    starts_with("trial"),
    mesaid,
    exam,
    counts,
    tx,
    lipid,
    all_of(baseline_vars),
    all_of(baseline_vars_long),
    all_of(tv_vars),
    starts_with("lag"),
    cursmk,
    risk,
    race_white1,
    educ_pri1,
    cvdatt,
    cvda,
    cens
  ) 

# censor when deviates from regime
trials_long <-
  trials_long %>%
  group_by(trial, mesaid) %>%
  mutate(
    id = str_c(trial, mesaid, sep = "_"), 
    exam = as.numeric(exam),
    drop = exam >= min(if_else(tx == 1 & lipid == 0 | tx == 0 & lipid == 1, exam, 10))
  ) %>%
  mutate(
    censor = max(as.numeric(drop)),
    cvda = replace(cvda, censor == 1, 0)
  )  %>%
  ungroup()

# create person-time dataset
trials_long_pt <- 
  trials_long %>%
  uncount(counts, .id = "time") %>%
  group_by(trial, mesaid) %>%
  mutate(
    time = row_number(),
    cvda = replace(cvda, time != max(replace(time, drop, 0)), 0),
    censor = replace(censor, time != max(replace(time, drop, 0)), 0)
  ) %>%
  filter(!drop) %>%
  ungroup()


trials_long_pt %>%
  count(
    trial, cvda, censor
  )
