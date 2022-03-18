
# intention to treat analysis ---------------------------------------------

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
    "(2)" = snaftm(
      model = Surv(cvdatt, cvda) ~ tx,
      tx.formula = reformulate(
        termlabels = c("bs(time, 5)", tv_adj_vars, baseline_vars, baseline_vars_long),
        response = "tx"
      ),
      tx.family = binomial(link = "logit"),
      data = trial1_long,
      id = "mesaid",
      time = "time",
      K.max = 120,
      conf.int = TRUE
    ),
    "(3)" = snaftm(
      model = Surv(cvdatt, cvda) ~ tx,
      tx.formula = reformulate(
        termlabels = c("bs(time, 5)", tv_adj_vars, baseline_vars, baseline_vars_long), 
        response = "tx"
      ),
      tx.family = binomial(link = "logit"),
      dr.formula = reformulate(
        termlabels = c("bs(time, 5)", tv_adj_vars, baseline_vars, baseline_vars_long), 
        response = "Surv(X.psi, delta.psi)",
      ),
      dr.family = "coxph",
      data = trial1_long,
      id = "mesaid",
      time = "time",
      K.max = 120,
      conf.int = TRUE
    )
  )

T0_weibull_fit_itt <- 
  survreg(
    Surv(H_psi, cvda) ~ 1,
    dist = 'weibull',
    data = trial1_long %>%
      mutate(
        H_psi = H(
          psi = itt_fits[[2]]$estimate, 
          data = as.matrix(select(trial1_long, mesaid, time, cvdatt, tx)), 
          id = "mesaid",
          time = "time",
          eventtime = "cvdatt"
        ),
        H_psi = pmin(H_psi, 120)
      ) %>% filter(exam == 2)
      # filter(!(cvda == 1 & delta_psi == 0))
  )

T0_weibull_fit_itt_dr <- 
  survreg(
    Surv(H_psi, cvda) ~ 1,
    dist = 'weibull',
    data = trial1_long %>%
      mutate(
        H_psi = H(
          psi = itt_fits[[3]]$estimate, 
          data = as.matrix(select(trial1_long, mesaid, time, cvdatt, tx)), 
          id = "mesaid",
          time = "time",
          eventtime = "cvdatt"
        ),
        H_psi = pmin(H_psi, 120)
      ) %>% filter(exam == 2)
    # filter(!(cvda == 1 & delta_psi == 0))
  )

# class(itt_fits[[1]]) <- c("custom", class(itt_fits[[1]]))
class(itt_fits[[2]]) <- c("snaftm", class(itt_fits[[2]]))
class(itt_fits[[3]]) <- c("snaftm", class(itt_fits[[3]]))


# adherence-adjusted analysis ---------------------------------------------

ipcw_fit <- ipcw(
  formula = reformulate(
    termlabels = c("trial2", "trial3", "trial4", "bs(time, 5)", baseline_vars, baseline_vars_long, "tx"), 
    response = "cvda"
  ),
  treatment = "tx",
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

fit <- 
  glm(
    formula = reformulate(
      termlabels = c("trial2", "trial3", "trial4", "bs(time, 5)", baseline_vars, baseline_vars_long, "tx"), 
      response = "cvda"
    ),
    family = binomial(link = "logit"),
    data = trials_long_pt,
    weights = ipcw_fit$ipcw
  )

adherence_fits <-
  list(
    "(4)" = fit,
    "(5)" = snaftm(
      model = Surv(cvdatt, cvda) ~ lipid,
      tx.formula = reformulate(
        termlabels = c("bs(time, 5)", tv_adj_vars, baseline_vars, baseline_vars_long), 
        response = "lipid"
      ),
      tx.family = binomial(link = "logit"),
      data = trial1_long,
      id = "mesaid",
      time = "time",
      K.max = 120,
      conf.int = TRUE
    ),
    "(6)" = snaftm(
      model = Surv(cvdatt, cvda) ~ lipid,
      tx.formula = reformulate(
        termlabels = c("bs(time, 5)", tv_adj_vars, baseline_vars, baseline_vars_long), 
        response = "lipid"
      ),
      tx.family = binomial(link = "logit"),
      dr.formula = reformulate(
        termlabels = c("bs(time, 5)", tv_adj_vars, baseline_vars, baseline_vars_long), 
        response = "Surv(X.psi, delta.psi)",
      ),
      dr.family = "coxph",
      data = trial1_long,
      id = "mesaid",
      time = "time",
      K.max = 120,
      conf.int = TRUE
    )
  )


T0_weibull_fit_adherence <- 
  survreg(
    Surv(H_psi, cvda) ~ 1, 
    dist = 'weibull',
    data = 
      trial1_long %>%
      mutate(
        H_psi = H(
          psi = adherence_fits[[2]]$estimate, 
          data = as.matrix(select(trial1_long, mesaid, time, cvdatt, lipid)), 
          id = "mesaid",
          time = "time",
          eventtime = "cvdatt"
        ),
        H_psi = pmin(H_psi, 120)
      ) %>% filter(exam == 2)
  )

T0_weibull_fit_adherence_dr <- 
  survreg(
    Surv(H_psi, cvda) ~ 1, 
    dist = 'weibull',
    data = 
      trial1_long %>%
      mutate(
        H_psi = H(
          psi = adherence_fits[[3]]$estimate, 
          data = as.matrix(select(trial1_long, mesaid, time, cvdatt, lipid)), 
          id = "mesaid",
          time = "time",
          eventtime = "cvdatt"
        ),
        H_psi = pmin(H_psi, 120)
      ) %>% filter(exam == 2)
  )


adherence_fits[[2]]$term <- "tx"
adherence_fits[[3]]$term <- "tx"

# class(adherence_fits[[1]]) <- c("custom", class(adherence_fits[[1]]))
class(adherence_fits[[2]]) <- c("snaftm", class(adherence_fits[[2]]))
class(adherence_fits[[3]]) <- c("snaftm", class(adherence_fits[[3]]))


t1 <- tbl_regression(
  x = itt_fits[[1]],
  label = list(
    tx ~ "lipid-lowering drugs"
  ),
  include = "tx",
  exponentiate = TRUE,
  tidy_fun = partial(
    tidy_robust,
    vcov_estimation = "CL",
    vcov_args = list(cluster = trials_pt$mesaid),
    ci_method = "wald"
  )
) %>%
  modify_header(label = "", estimate = "**HR**")

t2 <- tbl_regression(
  x = itt_fits[[2]] %>% mutate(across(where(is.numeric), function (x) x / T0_weibull_fit_itt$scale)),
  label = list(
    tx ~ "lipid-lowering drugs"
  ),
  include = "tx",
  exponentiate = TRUE
) %>%
  modify_header(label = "", estimate = "**HR**") %>%
  add_glance_table(
    label = list(
      scale ~ "scale $\\kappa$"
    ),
    fmt_fun = list(
      scale ~ function(x) style_sigfig(x, digits = 2)
    ),
    glance_fun = function(x) {
      tibble(scale = T0_weibull_fit_itt$scale)
    }
  )

t3 <- tbl_regression(
  x = itt_fits[[3]] %>% mutate(across(where(is.numeric), function (x) x / T0_weibull_fit_itt_dr$scale)),
  label = list(
    tx ~ "lipid-lowering drugs"
  ),
  include = "tx",
  exponentiate = TRUE
) %>%
  modify_header(label = "", estimate = "**HR**") %>%
  add_glance_table(
    label = list(
      scale ~ "scale $\\kappa$"
    ),
    fmt_fun = list(
      scale ~ function(x) style_sigfig(x, digits = 2)
    ),
    glance_fun = function(x) {
      tibble(scale = T0_weibull_fit_itt_dr$scale)
    }
  )

t4 <- tbl_regression(
  x = adherence_fits[[1]],
  label = list(
    tx ~ "lipid-lowering drugs"
  ),
  include = "tx",
  exponentiate = TRUE,
  tidy_fun = partial(
    tidy_robust,
    vcov_estimation = "CL",
    vcov_args = list(cluster = trials_pt$mesaid),
    ci_method = "wald"
   )
) %>%
  modify_header(label = "", estimate = "**HR**")

t5 <- tbl_regression(
  x = adherence_fits[[2]] %>% mutate(across(where(is.numeric), function (x) x / T0_weibull_fit_adherence$scale)),
  label = list(
    tx ~ "lipid-lowering drugs"
  ),
  include = "tx",
  exponentiate = TRUE
) %>%
  modify_header(label = "", estimate = "**HR**") %>%
  add_glance_table(
    label = list(
      scale ~ "scale $\\kappa$"
    ),
    fmt_fun = list(
      scale ~ function(x) style_sigfig(x, digits = 2)
    ),
    glance_fun = function(x) {
      tibble(scale = T0_weibull_fit_adherence$scale)
    }
  )

t6 <- tbl_regression(
  x = adherence_fits[[3]] %>% mutate(across(where(is.numeric), function (x) x / T0_weibull_fit_adherence_dr$scale)),
  label = list(
    tx ~ "lipid-lowering drugs"
  ),
  include = "tx",
  exponentiate = TRUE
) %>%
  modify_header(label = "", estimate = "**HR**") %>%
  add_glance_table(
    label = list(
      scale ~ "scale $\\kappa$"
    ),
    fmt_fun = list(
      scale ~ function(x) style_sigfig(x, digits = 2)
    ),
    glance_fun = function(x) {
      tibble(scale = T0_weibull_fit_adherence_dr$scale)
    }
  )


row1 <-
  tbl_merge(
    tbls = list(t1, t2, t3),
    tab_spanner = c("**IPCW**", "**g-estimation**", "**DR g-estimation**")
  ) %>%
  modify_table_styling(
    column = c(p.value_1),
    hide = TRUE
  )
row2 <-
  tbl_merge(
    tbls = list(t4, t5, t6),
    tab_spanner = c("**IPCW**", "**g-estimation**", "**DR g-estimation**")
  ) %>%
  modify_table_styling(
    column = c(p.value_1),
    hide = TRUE
  )


tbl_stack(tbls = list(row1, row2), group_header = gt::md(c("*ITT*", "*Adherence-adjusted*"))) %>%
  as_gt() %>%
  as_latex() %>%
  as.character() %>%
  write_file("2_tables/trial_results.tex")

# tab <-
# #   modelsummary(
# #     models = c(itt_fits, adherence_fits),
# #     output = "gt",
# #     vcov = list(~mesaid, "classical", ~mesaid, "classical"),
# #     fmt = '%.2f',
# #     estimate = "{estimate} ({conf.low}, {conf.high})",
# #     statistic = NULL,
# #     coef_rename = c(
# #       'tx' = 'lipid-lowering medication'
# #     ),
# #     add_rows = tribble(
# #       ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
# #       'Weibull scale ($\\kappa$)', '', as.character(specd(T0_weibull_fit_itt$scale, 2)), '', as.character(specd(T0_weibull_fit_adherence$scale, 2))
# #       ),
# #     coef_omit = "^(?!.*tx.*)",
# #     gof_omit = "(AIC)|(BIC)|(Log.*)|(RMSE)|(Num.*)",
# #     exponentiate = TRUE
# #   )
# 
#   
# 
# adj <- list(
#   snaftm(
#     rpsm = Surv(cvdatt, cvda) ~ lipid,
#     formula = reformulate(
#       termlabels = c("bs(time, 5)"), 
#       response = "lipid"
#     ),
#     family = binomial(link = "logit"),
#     data = trial1_long,
#     id = "mesaid",
#     time = "time"
#   ),
#   snaftm(
#     rpsm = Surv(cvdatt, cvda) ~ lipid,
#     formula = reformulate(
#       termlabels = c("bs(time, 5)", c("gender1", "bl_age", "bl_dm03", "bl_cursmk", "bl_htnmed",  "bl_sbp",  "bl_ldl", "bl_hdl")), 
#       response = "lipid"
#     ),
#     family = binomial(link = "logit"),
#     data = trial1_long,
#     id = "mesaid",
#     time = "time"
#   ),
#   snaftm(
#     rpsm = Surv(cvdatt, cvda) ~ lipid,
#     formula = reformulate(
#       termlabels = c("bs(time, 5)", baseline_vars, baseline_vars_long), 
#       response = "lipid"
#     ),
#     family = binomial(link = "logit"),
#     data = trial1_long,
#     id = "mesaid",
#     time = "time"
#   ),
#   snaftm(
#     rpsm = Surv(cvdatt, cvda) ~ lipid,
#     formula = reformulate(
#       termlabels = c("bs(time, 5)", tv_adj_vars, baseline_vars, baseline_vars_long), 
#       response = "lipid"
#     ),
#     family = binomial(link = "logit"),
#     data = trial1_long,
#     id = "mesaid",
#     time = "time"
#   )
# )
# 
# tbl_regression(
#   x = bind_rows(adj, .id = "term"),
#   tidy_fun = function(x, conf.int, conf.level) {x}
# )
# 
# tbl_regression(x = adj[[1]])

