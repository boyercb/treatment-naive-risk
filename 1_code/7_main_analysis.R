
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
  "afib1c",
  "crp1",
  "il61",
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
      name == "afib1c" ~ "Atrial Fibrillation on ECG, %",
      name == "crp1" ~ "C-reactive protein",
      name == "il61" ~ "Interleukin-6",
      name == "age" ~ "Age, years",
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


# intention to treat analysis ---------------------------------------------

baseline_vars <- baseline_vars[!baseline_vars %in% c("afib1c")]

tx_ints <- paste0("tx:", c("dm", "ldl"))
  
fit <-
  list(
    "All" = glm(
      formula = reformulate(c("factor(trial)", "bs(time, 5)", tv_vars, baseline_vars, "tx"), "cvda"),
      family = binomial(link = "logit"),
      data = trials_pt
    ),
    "< 2 years" = glm(
      formula = reformulate(c("factor(trial)", "bs(time, 5)", tv_vars, baseline_vars, "tx"), "cvda"),
      family = binomial(link = "logit"),
      data = filter(trials_pt, time < 24)
    ),
    "< 4 years" = glm(
      formula = reformulate(c("factor(trial)", "bs(time, 5)", tv_vars, baseline_vars, "tx"), "cvda"),
      family = binomial(link = "logit"),
      data = filter(trials_pt, time < 48)
    ),
    "< 6 years" = glm(
      formula = reformulate(c("factor(trial)", "bs(time, 5)", tv_vars, baseline_vars, "tx"), "cvda"),
      family = binomial(link = "logit"),
      data = filter(trials_pt, time < 72)
    ),
    "< 8 years" = glm(
      formula = reformulate(c("factor(trial)", "bs(time, 5)", tv_vars, baseline_vars, "tx"), "cvda"),
      family = binomial(link = "logit"),
      data = filter(trials_pt, time < 96)
    )
  )

fit <- lapply(fit, function(x) {
  class(x) <- c("custom", class(x))
  return(x)
})

tidy.custom <- function (x, conf.int = FALSE, conf.level = 0.95, conf.method = "wald", exponentiate = TRUE, 
                            ...) 
{
  ret <- as_tibble(summary.glm(x)$coefficients, rownames = "term")
  colnames(ret) <- c("term", "estimate", "std.error", "statistic", 
                     "p.value")
  coefs <- tibble::enframe(stats::coef(x), name = "term", 
                           value = "estimate")
  ret <- dplyr::left_join(coefs, ret, by = c("term", "estimate"))
  if (conf.int) {
    if(conf.method == "plik") 
      ci <- suppressMessages(confint(x, ...))
    else
      ci <- suppressMessages(confint.default(x, ...))
    
    if (is.null(dim(ci))) {
      ci <- matrix(ci, nrow = 1)
      rownames(ci) <- names(coef(x))[1]
    }
    
    ci <- as_tibble(ci, rownames = "term")
    names(ci) <- c("term", "conf.low", "conf.high")
    ret <- dplyr::left_join(ret, ci, by = "term")
  }
  if (exponentiate) {
    ret <- mutate_at(ret, vars(estimate), exp)
    if ("conf.low" %in% colnames(ret)) {
      ret <- mutate_at(ret, vars(conf.low, conf.high), exp)
    }
  }
  # ret <- mutate(ret, across(where(is.numeric), round, 2))
  ret
}

glance.custom <- function(x, ...) {
  out <- broom:::as_glance_tibble(null.deviance = x$null.deviance, df.null = x$df.null, 
                   logLik = as.numeric(stats::logLik(x)), AIC = stats::AIC(x), 
                   BIC = stats::BIC(x), deviance = stats::deviance(x), 
                   df.residual = stats::df.residual(x), nobs = stats::nobs(x), 
                   na_types = "rirrrrii")
  return(out)
}

modelsummary(
  models = fit,
  #vcov = ~mesaid, 
  fmt = '%.2f',
  estimate = "{estimate} ({conf.low}, {conf.high})",
  statistic = "{p.value}",
  # stars = TRUE,
  coef_omit = "^(?!tx.*)",
  gof_omit = "(AIC)|(BIC)|(Log.*)|(RMSE)"
)


# adherence-adjusted analysis ---------------------------------------------

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

# weights models
always_treat_model <- glm(
  #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
  formula = reformulate(c("factor(trial)", "bs(time, 3)", tv_adj_vars, baseline_vars, baseline_vars_long), "censor"),
  family = binomial(link = "logit"),
  data = filter(trials_long_pt, tx == 1),
)

always_treat_model_numerator <- glm(
  #formula = reformulate(c("bs(time, 3)", baseline_vars), "censor"),
  formula = reformulate(c("factor(trial)", "bs(time, 3)", baseline_vars, baseline_vars_long), "censor"),
  family = binomial(link = "logit"),
  data = filter(trials_long_pt, tx == 1),
)

never_treat_model <- glm(
  #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
  formula = reformulate(c("factor(trial)", "bs(time, 3)", tv_adj_vars, baseline_vars, baseline_vars_long), "censor"),
  family = binomial(link = "logit"),
  data = filter(trials_long_pt, tx == 0),
)

never_treat_model_numerator <- glm(
  #formula = reformulate(c("bs(time, 3)", baseline_vars), "censor"),
  formula = reformulate(c("factor(trial)", "bs(time, 3)", baseline_vars, baseline_vars_long), "censor"),
  family = binomial(link = "logit"),
  data = filter(trials_long_pt, tx == 0),
)


# trials_long_pt <- trials_long_pt %>%
#   group_by(trial, mesaid) %>%
#   mutate(
#     id = row_number(),
#     trial2 = as.numeric(trial == "2"),
#     trial3 = as.numeric(trial == "3"),
#     trial4 = as.numeric(trial == "4")) %>%
#   ungroup()

# library(ipw)
# always_treat_model_pkg <- ipwtm(
#   exposure = censor,
#   family = "binomial",
#   link = "logit",
#   # numerator = ~ bs(time, 3) + gender1 + married1 + educ_sec1 +
#   #   educ_col1 + race_black1 + race_hisp1 + race_asia1 + employed1 +
#   #   retired1 + evsmk1 + cesd1c + chrbu61c + discry1c + emot1c +
#   #   hassl1c + splang1c + splanx1c + pregn1 + bpillyr1 + menoage1 +
#   #   nprob1c + famhist1 + agatpm1c + ecglvh1c + crp1 + il61,
#   denominator = ~ bs(time, 3) + age + lag1_dm03 + 
#     lag1_htn + lag1_cursmk + lag1_waistcm + 
#     htnmed + asacat + diur + anydep + vasoda + anara + lag1_hinone + 
#     lag1_sbp + lag1_dbp + lag1_ldl + lag1_hdl + lag1_trig + lag1_dpw + 
#     lag1_exercise + gender1 + married1 + educ_sec1 + educ_col1 + 
#     race_black1 + race_hisp1 + race_asia1 + employed1 + retired1 + 
#     evsmk1 + cesd1c + chrbu61c + discry1c + emot1c + hassl1c + 
#     splang1c + splanx1c + pregn1 + bpillyr1 + menoage1 + nprob1c + 
#     famhist1 + agatpm1c + ecglvh1c + crp1 + il61,
#   id = id,
#   timevar = time,
#   type = "first",
#   data = data.frame(filter(trials_long_pt, tx == 1))
# )
# 
# never_treat_model <- ipwtm(
#   exposure = censor,
#   family = "binomial",
#   link = "logit",
#   numerator = ~ bs(time, 3) + trial2 + trial3 + trial4 + gender1 + married1 + educ_sec1 +
#     educ_col1 + race_black1 + race_hisp1 + race_asia1 + employed1 +
#     retired1 + evsmk1 + cesd1c + chrbu61c + discry1c + emot1c +
#     hassl1c + splang1c + splanx1c + pregn1 + bpillyr1 + menoage1 +
#     nprob1c + famhist1 + agatpm1c + ecglvh1c + afib1c + crp1 + il61,
#   denominator = ~ bs(time, 3) + trial2 + trial3 + trial4 + age + lag1_dm03 +
#     lag1_htn + lag1_cursmk + lag1_waistcm + 
#     htnmed + asacat + diur + anydep + vasoda + anara + lag1_hinone + 
#     lag1_sbp + lag1_dbp + lag1_ldl + lag1_hdl + lag1_trig + lag1_dpw + 
#     lag1_exercise + gender1 + married1 + educ_sec1 + educ_col1 + 
#     race_black1 + race_hisp1 + race_asia1 + employed1 + retired1 + 
#     evsmk1 + cesd1c + chrbu61c + discry1c + emot1c + hassl1c + 
#     splang1c + splanx1c + pregn1 + bpillyr1 + menoage1 + nprob1c + 
#     famhist1 + agatpm1c + ecglvh1c + afib1c + crp1 + il61,
#   id = id,
#   timevar = time,
#   type = "first",
#   data = data.frame(filter(trials_long_pt, tx == 0))
# )

# weights

trials_long_pt <- 
  trials_long_pt %>%
  mutate(
    p_num = 0,
    p_den = 0,
    ipw = 0
  )

# weights
trials_long_pt$p_num[trials_long_pt$tx == 1] <-
  1 - predict(always_treat_model_numerator, type = "response")

trials_long_pt$p_num[trials_long_pt$tx == 0] <-
  1 - predict(never_treat_model_numerator, type = "response")

trials_long_pt$p_den[trials_long_pt$tx == 1] <-
  1 - predict(always_treat_model, type = "response")

trials_long_pt$p_den[trials_long_pt$tx == 0] <-
  1 - predict(never_treat_model, type = "response")

trials_long_pt <- 
  trials_long_pt %>%
  group_by(trial, mesaid) %>%
  mutate(
    p_num = if_else(
      censor == 1, 
      1 - p_num * cumprod(lag(p_num, 1, default = 1)),
      p_num * cumprod(lag(p_num, 1, default = 1)),
    ),
    p_den = if_else(
      censor == 1, 
      1 - p_den * cumprod(lag(p_den, 1, default = 1)),
      p_den * cumprod(lag(p_den, 1, default = 1)),
    ),
    ipw = p_num / p_den
  )

tx_ints <- paste0("tx:", c("bl_dm03", "bl_ldl", "bl_sbp", "bl_hdl", "bl_age", "gender1", "bl_htnmed", "bl_cursmk"))


adherence_msm <- list(
  glm(
    formula = reformulate(c("factor(trial)", "bs(time, 3)", baseline_vars, baseline_vars_long, "tx"), "cvda"),
    family = binomial(link = "logit"),
    weights = ipw,
    data = trials_long_pt
  ),
  glm(
    formula = reformulate(c("factor(trial)", "bs(time, 3)", baseline_vars, baseline_vars_long, "tx", tx_ints), "cvda"),
    family = binomial(link = "logit"),
    weights = ipw,
    data = trials_long_pt
  )
)

p <- lmtest::waldtest(adherence_msm[[1]], adherence_msm[[2]])$`Pr(>F)`[2]

adherence_msm <- lapply(adherence_msm, function(x) {
  class(x) <- c("custom", class(x))
  return(x)
})


modelsummary(
  models = adherence_msm,
  #vcov = ~mesaid, 
  fmt = '%.2f',
  estimate = "{estimate} ({conf.low}, {conf.high})",
  statistic = NULL,
  coef_rename = c(
    'tx' = 'lipid-lowering medication',
    ''
  ),
  # stars = TRUE,
  add_rows = tribble(~term, ~`Model 1`, ~`Model 2`, 'P-value for heterogeneity', '', as.character(round(p, 3))),
  coef_omit = "^(?!.*tx.*)",
  gof_omit = "(AIC)|(BIC)|(Log.*)|(RMSE)"
)

# modelsummary(
#   models = fit,
#   #vcov = ~mesaid, 
#   fmt = '%.3f',
#   estimate = "{estimate}{stars} ({conf.low}, {conf.high})",
#   statistic = "{p.value}",
#   stars = TRUE,
#   coef_omit = "^(?!tx.*)",
#   gof_omit = "(AIC)|(BIC)|(Log.*)",
#   exponentiate = TRUE
# )

test_fits <- list(
  glm(
    formula = reformulate(c("bs(time, 3)", baseline_vars, "age", "tx"), "cvda"),
    family = binomial(link = "logit"),
    weights = ipw,
    data = filter(trials_long_pt, censor == 0 & trial == 1)
  ),
  glm(
    formula = reformulate(c("bs(time, 3)", baseline_vars, "age", "tx"), "cvda"),
    family = binomial(link = "logit"),
    weights = ipw,
    data = filter(trials_long_pt, censor == 0 & trial == 2)
  ),
  glm(
    formula = reformulate(c("bs(time, 3)", baseline_vars, "age", "tx"), "cvda"),
    family = binomial(link = "logit"),
    weights = ipw,
    data = filter(trials_long_pt, censor == 0 & trial == 3)
  ),
  glm(
    formula = reformulate(c("bs(time, 3)", baseline_vars, "age", "tx"), "cvda"),
    family = binomial(link = "logit"),
    weights = ipw,
    data = filter(trials_long_pt, censor == 0 & trial == 4)
  )
)
modelsummary(test_fits,fmt = 3)

test_fits <- list(
  glm(
    #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    family = binomial(link = "logit"),
    data = filter(trials_long_pt, tx == 1 & trial == 1),
  ),
  glm(
    #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    family = binomial(link = "logit"),
    data = filter(trials_long_pt, tx == 1 & trial == 2),
  ),
  glm(
    #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    family = binomial(link = "logit"),
    data = filter(trials_long_pt, tx == 1 & trial == 3),
  ),
  glm(
    #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    family = binomial(link = "logit"),
    data = filter(trials_long_pt, tx == 1 & trial == 4),
  )
)
modelsummary(test_fits,fmt = 3)

test_fits <- list(
  glm(
    #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    family = binomial(link = "logit"),
    data = filter(trials_long_pt, tx == 0 & trial == 1),
  ),
  glm(
    #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    family = binomial(link = "logit"),
    data = filter(trials_long_pt, tx == 0 & trial == 2),
  ),
  glm(
    #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    family = binomial(link = "logit"),
    data = filter(trials_long_pt, tx == 0 & trial == 3),
  ),
  glm(
    #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
    family = binomial(link = "logit"),
    data = filter(trials_long_pt, tx == 0 & trial == 4),
  )
)
modelsummary(test_fits,fmt = 3)# geeglm(
#   formula = reformulate(c("factor(trial)", "bs(time, 5)", "tx"), "cvda"),
#   family = binomial(link = "logit"),
#   data = trials_pt,
#   id = mesaid
# ) %>% summary()
