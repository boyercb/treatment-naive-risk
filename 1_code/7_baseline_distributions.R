# compare baseline distributions ------------------------------------------

var_order <- c(
  "age",
  "gender1",
  "married1",
  "educ_pri1",
  "educ_sec1",
  "educ_col1",
  "race_white1",
  "race_black1",
  "race_hisp1",
  "race_asia1",
  "employed1",
  "retired1",
  "hinone",
  "evsmk1",
  "cursmk",
  "dpw",
  "exercise",
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
  "il61",
  "risk",
  "dm03",
  "htn",
  "waistcm",
  "sbp",
  "dbp",
  "ldl",
  "hdl",
  "trig",
  "htnmed",
  "diabins",
  "asacat",
  "diur",
  "anydep",
  "vasoda",
  "anara"
)


baseline_table <- 
  trials %>%
  select(all_of(var_order), tx) %>%
  filter(!is.na(tx)) %>%
  mutate(
    across(
      .cols = c("pregn1", 
                "bpillyr1",
                "menoage1"), 
      .fns = ~replace(., gender1 == 1, NA)
    )
  ) %>%
  group_by(tx) %>%
  summarise(
    across(
      .cols = c(all_of(var_order)), 
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
      name == "educ_pri1" ~ "Less than high school, %",
      name == "educ_sec1" ~ "High school graduate, %",
      name == "educ_col1" ~ "College or postgraduate, %",
      name == "race_white1" ~ "Non-Hispanic white, %",
      name == "race_black1" ~ "Non-Hispanic black, %",
      name == "race_hisp1" ~ "Hispanic, %",
      name == "race_asia1" ~ "Asian, %",
      name == "employed1" ~ "Currently employed, %",
      name == "retired1" ~ "Retired, %",
      name == "evsmk1" ~ "Smoked > 100 cigarettes in lifetime, %",
      name == "cursmk" ~ "Current smoker, %",
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
      # name == "afib1c" ~ "Atrial Fibrillation on ECG, %",
      name == "crp1" ~ "C-reactive protein",
      name == "il61" ~ "Interleukin-6",
      name == "age" ~ "Age, years",
      name == "risk" ~ "Baseline ASCVD risk",
      name == "dm03" ~ "Diabetes mellitus, %",
      name == "htn" ~ "Hypertension",
      name == "waistcm" ~ "Waist circumference, cm",
      name == "htnmed" ~ "Anti-hypertensive medication, %",
      name == "diabins" ~ "Insulin or oral hypoglycemics, %",
      name == "asacat" ~ "Daily aspirin use, %",
      name == "diur" ~ "Diuretics, %",
      name == "anydep" ~ "Any anti-depressants, %",
      name == "vasoda" ~ "Any vasodilator, %",
      name == "anara" ~ "Any anti-arrhytmic, %",
      name == "hinone" ~ "No health insurance, %",
      name == "sbp" ~ "Systolic blood pressure, mmHg",
      name == "dbp" ~ "Diastolic bood pressure, mmHg",
      name == "ldl" ~ "LDL cholesterol, mg/dL",
      name == "hdl" ~ "HDL cholesterol, mg/dL",
      name == "trig" ~ "Triglycerides, mg/dL",
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

