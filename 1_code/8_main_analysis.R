
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
      rpsm = Surv(cvdatt, cvda) ~ tx,
      formula = reformulate(
        termlabels = c("bs(time, 5)", tv_adj_vars, baseline_vars, baseline_vars_long), 
        response = "tx"
      ),
      family = binomial(link = "logit"),
      data = trial1_long,
      id = "mesaid",
      time = "time"
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

class(itt_fits[[1]]) <- c("custom", class(itt_fits[[1]]))
class(itt_fits[[2]]) <- c("snaftm", class(itt_fits[[2]]))


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

adherence_fits <-
  list(
    "(3)" = ipcw_fit$msm[[1]],
    "(4)" = snaftm(
      rpsm = Surv(cvdatt, cvda) ~ lipid,
      formula = reformulate(
        termlabels = c("bs(time, 5)", tv_adj_vars, baseline_vars, baseline_vars_long), 
        response = "lipid"
      ),
      family = binomial(link = "logit"),
      data = trial1_long,
      id = "mesaid",
      time = "time"
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


adherence_fits[[2]]$term <- "tx"

class(adherence_fits[[1]]) <- c("custom", class(adherence_fits[[1]]))
class(adherence_fits[[2]]) <- c("snaftm", class(adherence_fits[[2]]))

modelsummary(
  models = c(itt_fits, adherence_fits),
  #vcov = ~mesaid, 
  fmt = '%.2f',
  estimate = "{estimate} ({conf.low}, {conf.high})",
  statistic = NULL,
  coef_rename = c(
    'tx' = 'lipid-lowering medication'
  ),
  add_rows = tribble(
    ~term, ~`(1)`, ~`(2)`, ~`(3)`, ~`(4)`,
    'Weibull scale', '', as.character(specd(T0_weibull_fit_itt$scale, 2)), '', as.character(specd(T0_weibull_fit_adherence$scale, 2))
    ),
  coef_omit = "^(?!.*tx.*)",
  gof_omit = "(AIC)|(BIC)|(Log.*)|(RMSE)"
)


