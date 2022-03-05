
# set up nested target trials ---------------------------------------------

# trial 1: follow-up starts at exam 2 and ends 10-years later
trial1 <- 
  mesa %>%
  filter(lipid1c == 0 & cvdatt > time2) %>%
  mutate(
    tx = lipid2c,
    cvdatt = replace(cvdatt - time2, cvdatt - time2 > 3650, 3650),
    cvdatt = round(cvdatt / 30),
    age = age2c,
    dm = dm031c,
    htn = htn1c,
    waistcm = waistcm1,
    htnmed = htnmed2c,
    asacat = asacat2c,
    wtls = wtls2c,
    diur = diur2c,
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
    exercise = exercise1
  ) 
  
# trial 2: follow-up starts at exam 3, but still ends 10-years after exam 2
trial2 <-
  mesa %>%
  filter(pmax(lipid1c, lipid2c, na.rm = TRUE) == 0 & cvdatt > time3 & !is.na(time2)) %>%
  mutate(
    tx = lipid3c,
    cvdatt = replace(cvdatt - time2, cvdatt - time2 > 3650, 3650),
    cvdatt = round((cvdatt - (time3 - time2)) / 30),
    age = age3c,
    dm = dm032c,
    htn = htn2c,
    waistcm = waistcm2,
    htnmed = htnmed3c,
    asacat = asacat3c,
    wtls = wtls3c,
    diur = diur3c,
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
    exercise = exercise2
  ) 

# trial 3: follow-up starts at exam 4, but still ends 10-years after exam 2
trial3 <-
  mesa %>%
  filter(pmax(lipid1c, lipid2c, lipid3c, na.rm = TRUE) == 0 & cvdatt > time4 & !is.na(time2)) %>%
  mutate(
    tx = lipid4c,
    cvdatt = replace(cvdatt - time2, cvdatt - time2 > 3650, 3650),
    cvdatt = round((cvdatt - (time4 - time2)) / 30),
    age = age4c,
    dm = dm033c,
    htn = htn3c,
    waistcm = waistcm3,
    htnmed = htnmed4c,
    asacat = asacat4c,
    wtls = wtls4c,
    diur = diur4c,
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
    exercise = exercise3
  ) 

# trial 4: follow-up starts at exam 5, but still ends 10-years after exam 2
trial4 <-
  mesa %>%
  filter(pmax(lipid1c, lipid2c, lipid3c, lipid4c, na.rm = TRUE) == 0 &  cvdatt > time5 & !is.na(time2)) %>%
  mutate(
    tx = lipid5c,
    cvdatt = replace(cvdatt - time2, cvdatt - time2 > 3650, 3650),
    cvdatt = round((cvdatt - (time5 - time2)) / 30),
    age = age5c,
    dm = dm034c,
    htn = htn4c,
    waistcm = waistcm4,
    htnmed = htnmed5c,
    asacat = asacat5c,
    wtls = wtls5c,
    diur = diur5c,
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
    exercise = exercise3
  ) 


# combine nested trials into single dataset -------------------------------

# create list of time-varying covariates
tv_vars <- c(
  "age",
  "dm",
  "htn",
  "waistcm",
  "htnmed",
  "asacat",
  "wtls",
  "diur",
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

# stack trials together
trials <- 
  bind_rows(trial1, trial2, trial3, trial4, .id = "trial") %>%
  select(
    trial,
    mesaid,
    tx,
    baseline_vars,
    tv_vars,
    cvdatt,
    cvda
  ) 

# create person-time dataset
trials_pt <- 
  trials %>%
  uncount(cvdatt, .id = "time") %>%
  group_by(trial, mesaid) %>%
  mutate(cvda = replace(cvda, time != max(time), 0))


# compare baseline distributions ------------------------------------------

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

specd <- function(x, k) trimws(format(round(x, k), nsmall=k))

trials %>%
  select(all_of(baseline_vars), all_of(tv_vars), tx) %>%
  filter(!is.na(tx)) %>%
  group_by(tx) %>%
  summarise(
    across(
      .cols = c(all_of(baseline_vars), all_of(tv_vars)), 
      .fns = list(mean = mean, sd = sd), 
      na.rm = T
    ), 
    .groups = "drop"
  ) %>%
  pivot_longer(-tx) %>% 
  separate(name, c("name", "func"), sep = "_(?!(.|\n)*_)") %>%
  filter(!(func == "sd" & !(name %in% cont_vars))) %>%
  mutate(value = specd(if_else(name %in% cont_vars, value, value * 100), 1)) %>%
  pivot_wider(
    names_from = c("tx", "func"),
    values_from = "value",
    names_prefix = c("tx_", "tx_")
  ) %>% 
  mutate(
    tx_0 = if_else(
      name %in% cont_vars, 
      paste0(tx_0_mean, " (", tx_0_sd, ")"),
      tx_0_mean 
    ), 
    tx_1 = if_else(
      name %in% cont_vars, 
      paste0(tx_1_mean, " (", tx_1_sd, ")"),
      tx_1_mean 
    ), 
  ) %>%
  select(name, tx_1, tx_0) %>%
  mutate(
    name = case_when(
      name == "gender1" ~ "Male, %",
      name == "married1" ~ "Married, %",
      name == "educ_sec1" ~ "High school graduate",
      name == "educ_col1" ~ "College or postgraduate",
      name == "race_black1" ~ "Non-Hispanic black, %",
      name == "race_hisp1" ~ "Hispanic, %",
      name == "race_asia1" ~ "Asian, %",
      name == "employed1" ~ "Currently employed, %",
      name == "retired1" ~ "Retired, %",
      name == "evsmk1" ~ "Smoke at least 100 cigarettes, %",
      name == "cesd1c" ~ "CES Depression scale",
      name == "chrbu61c" ~ "Chronic burden scale",
      name == "discry1c" ~ "Discrimination scale",
      name == "emot1c" ~ "Emotional support scale",
      name == "hassl1c" ~ "Everyday hassles scale",
      name == "splang1c" ~ "Spielberger trait anger scale",
      name == "splanx1c" ~ "Spielberger trait anxiety scale",
      name == "pregn1" ~ "Pregnancies",
      name == "bpillyr1" ~ "Years on birth control pills",
      name == "menoage1" ~ "Age at menopause",
      name == "nprob1c" ~ "Neighborhood problems scale",
      name == "famhist1" ~ "Family history of CVD, %",
      name == "agatpm1c" ~ "Calcium score",
      name == "ecglvh1c" ~ "Left ventricular hypertrophy on ECG, %",
      name == "afib1c" ~ "Atrial Fibrillation on ECG, %",
      name == "crp1" ~ "C-reactive protein",
      name == "il61" ~ "Interleukin-6",
      name == "age" ~ "Age, years",
      name == "dm" ~ "Diabetes mellitus, %",
      name == "htn" ~ "Hypertension",
      name == "waistcm" ~ "Waist circumference, cm",
      name == "htnmed" ~ "Anti-hypertensive medication, %",
      name == "asacat" ~ "Daily aspirin use, %",
      name == "wtls" ~ "Weight loss drugs, %",
      name == "diur" ~ "Diuretics, %",
      name == "anydep" ~ "Any anti-depressants, %",
      name == "vasoda" ~ "Any vasodilator, %",
      name == "anara" ~ "Any anti-arrhytmic, %",
      name == "hinone" ~ "No health insurance, %",
      name == "sbp" ~ "Systolic blood pressure, mmHg",
      name == "dbp" ~ "Diastolic bood pressure, mmHg",
      name == "ldl" ~ "LDL cholesterol, mg/dL",
      name == "hdl" ~ "HDL cholesterol, mg/dL",
      name == "trig" ~ "Tryglycerides, mg/dL",
      name == "dpw" ~ "Drinks per week",
      name == "exercise" ~ "Exercise, MET/min"
    )
  ) %>%
  flextable() %>%
  set_header_labels(
    name = "", 
    tx_1 = paste0("Initiators\n(N = ", format(table(trials$tx)[2], big.mark = ","), ")"), 
    tx_0 = paste0("Non-initiators\n(N = ", format(table(trials$tx)[1], big.mark = ","), ")")
    ) %>%
  theme_booktabs(bold_header = TRUE) %>%
  align(j = c(2, 3), align = "center", part = "all") %>%
  autofit()


tx_ints <- paste0("tx:", c("dm", "ldl"))
  
fit <- list(
  glm(
    formula = reformulate(c("factor(trial)", "bs(time, 5)", "tx"), "cvda"),
    family = binomial(link = "logit"),
    data = na.omit(trials_pt)
  ),
  glm(
    formula = reformulate(c("factor(trial)", "bs(time, 5)", tv_vars, baseline_vars), "cvda"),
    family = binomial(link = "logit"),
    data = trials_pt
  ),
  glm(
    formula = reformulate(c("factor(trial)", "bs(time, 5)", tv_vars, baseline_vars, tx_ints), "cvda"),
    family = binomial(link = "logit"),
    data = trials_pt
  )
)

modelsummary(
  models = fit,
  #vcov = ~mesaid, 
  estimate = "{estimate}{stars} ({conf.low}, {conf.high})",
  statistic = "{p.value}",
  stars = TRUE,
  coef_omit = "^(?!tx.*)",
  gof_omit = "(AIC)|(BIC)|(Log.*)",
  exponentiate = TRUE
)

# geeglm(
#   formula = reformulate(c("factor(trial)", "bs(time, 5)", "tx"), "cvda"),
#   family = binomial(link = "logit"),
#   data = trials_pt,
#   id = mesaid
# ) %>% summary()
