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
  "cesd1c",
  "chrbu61c",
  "discry1c",
  "emot1c",
  "hassl1c",
  "splang1c",
  "splanx1c",
  "nprob1c",
  "sbp",
  "dbp",
  "ldl",
  "hdl",
  "trig",
  "risk",
  "dm03",
  "htn",
  "waistcm",
  "evsmk1",
  "cursmk",
  "dpw",
  "exercise",
  "famhist1",
  "agatpm1c",
  "ecglvh1c",
  "crp1",
  "il61",
  "pregn1",
  "bpillyr1",
  "menoage1",
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
    names_prefix = "tx_"
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
      name == "evsmk1" ~ "Smoked <100 cigarettes in lifetime, %",
      name == "cursmk" ~ "Current smoker, %",
      name == "cesd1c" ~ "CES Depression scale (0-60)",
      name == "chrbu61c" ~ "Chronic burden scale (0-5)",
      name == "discry1c" ~ "Perceived discrimination scale (0-4)",
      name == "emot1c" ~ "Emotional support scale (0-30)",
      name == "hassl1c" ~ "Everyday hassles scale (0-54)",
      name == "splang1c" ~ "Spielberger trait anger scale (0-40)",
      name == "splanx1c" ~ "Spielberger trait anxiety scale (0-40)",
      name == "pregn1" ~ "Number of pregnancies",
      name == "bpillyr1" ~ "Years on birth control pills",
      name == "menoage1" ~ "Age at menopause, years",
      name == "nprob1c" ~ "Neighborhood problems scale (0-28)",
      name == "famhist1" ~ "Family history of CVD, %",
      name == "agatpm1c" ~ "Calcium score",
      name == "ecglvh1c" ~ "Left ventricular hypertrophy on ECG, %",
      # name == "afib1c" ~ "Atrial Fibrillation on ECG, %",
      name == "crp1" ~ "C-reactive protein, mg/dL",
      name == "il61" ~ "Interleukin-6, pg/mL",
      name == "age" ~ "Age, years",
      name == "risk" ~ "Baseline ASCVD risk, %",
      name == "dm03" ~ "Diabetes mellitus, %",
      name == "htn" ~ "Hypertension, %",
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
    ),
    name = escape_latex(name),
    name = str_replace(name, fixed("<"), "$<$") 
  ) 



baseline_table_gt <- 
  baseline_table %>%
  gt() %>%
  cols_label(
    name = "",
    tx_1 = md(paste0("**(N = ", format(table(trials$tx)[2], big.mark = ","), ")**")),
    tx_0 = md(paste0("**(N = ", format(table(trials$tx)[1], big.mark = ","), ")**"))
  ) %>%
  # tab_style(
  #   style = list(
  #     cell_borders("all", weight = NULL),
  #     cell_text(indent = px(20))
  #   ), 
  #   locations = list(
  #     cells_row_groups(),
  #     cells_body(columns = name)
  #   )
  # ) %>%
  tab_spanner(label = md("**Initiators**"), columns = tx_1) %>%
  tab_spanner(label = md("**Non-initiators**"), columns = tx_0) %>%
  cols_align(align = "center", columns = c(tx_1, tx_0)) %>%
  tab_row_group(
    label = md("*Medications*"),
    rows = 43:49
  ) %>%
  tab_row_group(
    label = md("*CVD risk factors*"),
    rows = 22:42
  ) %>%
  tab_row_group(
    label = md("*Demographics*"),
    rows = 1:21
  )

# baseline_table_gt %>% 
#   as_latex() %>%
#   as.character() %>%
#   remove_escape_latex() %>%
#   str_remove(., fixed("\n\\cmidrule(lr){2-2} \\cmidrule(lr){3-3}")) %>%
#   str_remove_all(., fixed("\n\\midrule\n")) %>%
#   str_replace(., fixed("\\multicolumn{1}{l}{\\emph{Demographics}}"), "\n\\midrule\n\\multicolumn{1}{l}{\\emph{Demographics}}") %>%
#   str_replace(., fixed(">"), "$>$") %>%
#   write_file("2_tables/baseline.tex")


# just make a kable version dummy!
kable(
  x = baseline_table,
  format = "latex",
  col.names = c(
    "",
    paste0("(N = ", format(table(trials$tx)[2], big.mark = ","), ")"),
    paste0("(N = ", format(table(trials$tx)[1], big.mark = ","), ")")
  ),
  align = "lcc",
  escape = FALSE,
  booktabs = TRUE,
  linesep = "",
  caption = "Baseline characteristics of initiators and non-initiators in emulated nested trials"
) %>%
  kable_styling(font_size = 8) %>%
  row_spec(0, bold = TRUE) %>%
  add_header_above(c(" ", "Initiators", "Non-initiators"), line = FALSE, bold = TRUE) %>%
  group_rows("Demographics", 1, 21, italic = TRUE, bold = FALSE) %>%
  group_rows("CVD risk factors", 22, 42, italic = TRUE, bold = FALSE) %>%
  group_rows("Medications", 43, 49, italic = TRUE, bold = FALSE) %>%
  save_kable(file = "2_tables/baseline.tex")  


trials_pt <-
  trials_pt %>%
  mutate(
    across(all_of(log_transform), ~log(.x + 1)),
  )
trials_long_pt <-
  trials_long_pt %>%
  mutate(
    across(c(str_replace(log_transform, "exercise", "bl_exercise"), "lag1_exercise"), ~log(.x + 1))
  )