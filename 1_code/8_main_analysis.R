
# intention to treat analysis ---------------------------------------------

tx_ints <-
  paste0("tx:",
         c("gender1", "age", "dm03", "sbp", "cursmk", "hdl", "ldl", "htnmed"))

trials_pt <-
  trials_pt %>%
  mutate(
    across(log_transform, ~log(.x + 1)),
  )

itt_fits <-
  list(
    "(1)" = glm(
      formula = reformulate(
        termlabels = c("trial2", "trial3", "trial4", "bs(time, 5)", tv_vars, baseline_vars, "tx"), 
        response = "cvda"
        ),
      family = binomial(link = "logit"),
      data = trials_pt
    ),
    "(2)" = glm(
      formula = reformulate(
        termlabels = c("trial2", "trial3", "trial4", "bs(time, 5)", tv_vars, baseline_vars, "tx", tx_ints), 
        response = "cvda"
        ),
      family = binomial(link = "logit"),
      data = trials_pt
    ),
    "(3)" = glm(
      formula = reformulate(
        termlabels = c("trial2", "trial3", "trial4", "bs(time, 5)", tv_vars, baseline_vars, "tx", "tx:risk"), 
        response = "cvda"
        ),
      family = binomial(link = "logit"),
      data = trials_pt
    )
  )

itt_fits <- lapply(itt_fits, function(x) {
  class(x) <- c("custom", class(x))
  return(x)
})


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

trials_long_pt <-
  trials_long_pt %>%
  mutate(
    across(c(str_replace(log_transform, "exercise", "bl_exercise"), "lag1_exercise"), ~log(.x + 1))
  )


tx_ints <-
  paste0(
    "tx:",
    c(
      "gender1",
      "bl_age",
      "bl_dm03",
      "bl_sbp",
      "bl_cursmk",
      "bl_hdl",
      "bl_ldl",
      "bl_htnmed"
    )
  )

adherence_msms <-
  coxmsm(
    treatment = "tx",
    msm = list(
      reformulate(
        termlabels = c("trial2", "trial3", "trial4", "bs(time, 5)", baseline_vars, baseline_vars_long, "tx"), 
        response = "cvda"
      ),
      reformulate(
        termlabels = c("trial2", "trial3", "trial4", "bs(time, 5)", baseline_vars, baseline_vars_long, "tx", tx_ints), 
        response = "cvda"
      ),
      reformulate(
        termlabels = c("trial2", "trial3", "trial4", "bs(time, 5)", baseline_vars, baseline_vars_long, "tx", tx_ints),
        response = "cvda"
      )
    ),
    numerator = reformulate(
      termlabels = c("trial2", "trial3", "trial4", "bs(time, 5)", baseline_vars, baseline_vars_long), 
      response = "censor"
    ),
    denominator = reformulate(
      termlabels = c("trial2", "trial3", "trial4", "bs(time, 5)", tv_adj_vars, baseline_vars, baseline_vars_long), 
      response = "censor"
    ),
    id = "id",
    time = "time",
    data = trials_long_pt
  )


adherence_msms_fits <- lapply(adherence_msms$msm, function(x) {
  class(x) <- c("custom", class(x))
  return(x)
})

het_p <- map2(
  .x = list(
    itt_fits[[1]],
    itt_fits[[1]],
    adherence_msms_fits[[1]],
    adherence_msms_fits[[1]]
  ),
  .y = list(
    itt_fits[[2]],
    itt_fits[[3]],
    adherence_msms_fits[[2]],
    adherence_msms_fits[[3]]
  ),
  function(x, y) waldtest(x, y)$`Pr(>F)`[2]
)


modelsummary(
  models = c(itt_fits, adherence_msms_fits),
  #vcov = ~mesaid, 
  fmt = '%.2f',
  estimate = "{estimate} ({conf.low}, {conf.high})",
  statistic = NULL,
  # coef_rename = c(
  #   'tx' = 'lipid-lowering medication',
  #   ''
  # ),
  # add_rows = tribble(
  #   ~term, ~`Model 1`, ~`Model 2`,
  #   'P-value for heterogeneity', '', as.character(round(p, 3)))
  # ,
  coef_omit = "^(?!.*tx.*)",
  gof_omit = "(AIC)|(BIC)|(Log.*)|(RMSE)"
)



# test_fits <- list(
#   glm(
#     formula = reformulate(c("bs(time, 3)", baseline_vars, "age", "tx"), "cvda"),
#     family = binomial(link = "logit"),
#     weights = ipw,
#     data = filter(trials_long_pt, censor == 0 & trial == 1)
#   ),
#   glm(
#     formula = reformulate(c("bs(time, 3)", baseline_vars, "age", "tx"), "cvda"),
#     family = binomial(link = "logit"),
#     weights = ipw,
#     data = filter(trials_long_pt, censor == 0 & trial == 2)
#   ),
#   glm(
#     formula = reformulate(c("bs(time, 3)", baseline_vars, "age", "tx"), "cvda"),
#     family = binomial(link = "logit"),
#     weights = ipw,
#     data = filter(trials_long_pt, censor == 0 & trial == 3)
#   ),
#   glm(
#     formula = reformulate(c("bs(time, 3)", baseline_vars, "age", "tx"), "cvda"),
#     family = binomial(link = "logit"),
#     weights = ipw,
#     data = filter(trials_long_pt, censor == 0 & trial == 4)
#   )
# )
# modelsummary(test_fits,fmt = 3)
# 
# test_fits <- list(
#   glm(
#     #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     family = binomial(link = "logit"),
#     data = filter(trials_long_pt, tx == 1 & trial == 1),
#   ),
#   glm(
#     #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     family = binomial(link = "logit"),
#     data = filter(trials_long_pt, tx == 1 & trial == 2),
#   ),
#   glm(
#     #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     family = binomial(link = "logit"),
#     data = filter(trials_long_pt, tx == 1 & trial == 3),
#   ),
#   glm(
#     #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     family = binomial(link = "logit"),
#     data = filter(trials_long_pt, tx == 1 & trial == 4),
#   )
# )
# modelsummary(test_fits,fmt = 3)
# 
# test_fits <- list(
#   glm(
#     #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     family = binomial(link = "logit"),
#     data = filter(trials_long_pt, tx == 0 & trial == 1),
#   ),
#   glm(
#     #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     family = binomial(link = "logit"),
#     data = filter(trials_long_pt, tx == 0 & trial == 2),
#   ),
#   glm(
#     #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     family = binomial(link = "logit"),
#     data = filter(trials_long_pt, tx == 0 & trial == 3),
#   ),
#   glm(
#     #formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     formula = reformulate(c("bs(time, 3)", tv_adj_vars, baseline_vars), "censor"),
#     family = binomial(link = "logit"),
#     data = filter(trials_long_pt, tx == 0 & trial == 4),
#   )
# )
# 
# 
# 
# 
# 
# modelsummary(test_fits,fmt = 3)# geeglm(
# #   formula = reformulate(c("factor(trial)", "bs(time, 5)", "tx"), "cvda"),
# #   family = binomial(link = "logit"),
# #   data = trials_pt,
# #   id = mesaid
# # ) %>% summary()
