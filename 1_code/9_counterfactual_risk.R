
# prepare data for risk prediction ----------------------------------------

# start with trial 1 data (same as fit for g-estimation)
risk_df <- 
  trial1_long %>%
  mutate(
    # calculate H(psi) using psi^* from adherence adjusted g-estimator
    H_psi = H(
      psi = adherence_fits[[2]]$estimate,     # using non-DR adherence estimate for now (think about best option)
      data = as.matrix(select(trial1_long, mesaid, time, cvdatt, lipid)), 
      id = "mesaid",
      time = "time",
      eventtime = "cvdatt"
    ),
    
    # convert continuous risk factors to larger units to be more readily interpretable
    bl_age = bl_age / 5,    # age in 5 year increments
    bl_sbp = bl_sbp / 10,   # SBP in 10 mmHg increments
    bl_hdl = bl_hdl / 10,   # HDL in 10 mg/dL increments
    bl_chol = bl_chol / 10,  # total cholesterol in 10 mg/dL increments
    
  ) %>%
  # make predictions just based on baseline values
  filter(exam == 2) %>%
  select(all_of(baseline_vars_long),
         all_of(outcome_vars),
         bl_chol,
         gender1,
         H_psi)

# record risk factor means prior to censoring
risk_factor_means <- 
  risk_df %>%
  summarise(across(all_of(c(baseline_vars_long, "bl_chol")), mean))
  
# center risk factors prior to fitting
risk_df <- 
  risk_df %>%
  mutate(across(c(bl_age, bl_sbp, bl_chol, bl_hdl), scale, scale = FALSE))

  
# fit risk prediction models ----------------------------------------------

risk_formulas <- list(
  "Framingham" = ~ bl_age + gender1 + bl_cursmk + bl_dm03 + bl_sbp + bl_hdl + bl_chol + bl_htnmed + bl_sbp:bl_htnmed
)

fit_prediction_models <- function(formula, outcome, data) {
  coxph(update(formula, paste0(outcome, " ~ .")), data = data, model = TRUE, x = TRUE, y = TRUE)
}

fit_fct <- 
  map(risk_formulas, ~fit_prediction_models(.x, "Surv(cvdatt, cvda)", risk_df))
  
names(fit_fct) <- "Factual"

fit_cft <- 
  map(risk_formulas, ~fit_prediction_models(.x, "Surv(H_psi, cvda)", risk_df))

names(fit_cft) <- "Counterfactual"

# create output table
tbl_merge(
  tbls = map(c(fit_fct, fit_cft), ~tbl_regression(., exponentiate = TRUE)),
  tab_spanner = c("**Factual<br>Model**", "**Counterfactual<br>Model**")
) %>%
  as_gt() # %>%
  # as_latex() %>%
  # as.character() %>%
  # write_file("2_tables/models.tex")

# plot of marginal counterfactual vs. factual risk
# select(risk_df, H_psi, cvdatt, cvda) %>%
#   pivot_longer(-cvda) %>%
#   survfit(Surv(value, cvda) ~ name, data = .) %>%
#   broom::tidy(.) %>%
#   ggplot(., aes(x = time, y = 1 - estimate, linetype = strata)) +
#   geom_step() +
#   theme_classic() +
#   # xlim(c(0, 120)) +
#   # scale_linetype_discrete(name = "", labels = c("Non-initators", "Initiators")) +
#   labs(
#     x = "\nMonth of follow up",
#     y = "Probability of CVD\n"
#   ) +
#   theme(legend.position = c(0.8, 0.8))


# model performance statistics --------------------------------------------

# discrimination

# calibration

# treatment thresholds ----------------------------------------------------

# first let's look at thresholds based on MESA risk distribution
preds <- map_dfc(c(fit_fct, fit_cft), ~compute_risk_score(.x, risk_df, 120))

risk_df_long <-
  risk_df %>%
  bind_cols(preds) %>%
  pivot_longer(c(Factual, Counterfactual))

p3 <- 
  ggplot(risk_df_long, aes(x = value, color = name)) +
  geom_line(aes(y = 1 - ..y..), stat='ecdf') +
  theme_classic() +
  # annotate("text", x = 0.085, y = 0.54, label = "0.53", ) +
  scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.05), minor_breaks = T) +
  scale_color_brewer(name = "", palette = "Set1") +
  labs(
    x = "\nRisk treshold for treatment",
    y = "Proportion treated\n"
  ) +
  theme(legend.position = c(0.8, 0.8)) +
  geom_vline(xintercept = c(0.075, 0.10, 0.15), linetype = "dotted")

ggsave("3_figures/treatment_curves.pdf", p3, device = "pdf", width = 6, height = 4)


# now let's look at a US representative population from 2017-2018 NHANES

nhanes_data <-
  nhanes_data %>%
  mutate(
    # convert continuous risk factors to larger units to be more readily interpretable
    bl_age = as.numeric(bl_age) / 5,    # age in 5 year increments
    bl_sbp = bl_sbp / 10,   # SBP in 10 mmHg increments
    bl_hdl = bl_hdl / 10,   # HDL in 10 mg/dL increments
    bl_chol = bl_chol / 10,  # total cholesterol in 10 mg/dL increments
    
    bl_age = bl_age - risk_factor_means[['bl_age']],
    bl_sbp = bl_sbp - risk_factor_means[['bl_sbp']],
    bl_hdl = bl_hdl - risk_factor_means[['bl_hdl']],
    bl_chol = bl_chol - risk_factor_means[['bl_chol']]
  ) 

preds <- map_dfc(c(fit_fct, fit_cft), ~compute_risk_score(.x, nhanes_data, 120))

nhanes_data_long <- 
  nhanes_data %>%
  bind_cols(preds) %>%
  pivot_longer(c(Factual, Counterfactual))


svydesign(id = ~SDMVPSU, strata = ~SDMVSTRA, weights = ~WTINT2YR, data = nhanes_data_long, nest = T)

p4 <-
  ggplot(nhanes_data_long, aes(x = value, color = name)) +
  geom_line(aes(y = 1 - ..y..), stat='ecdf') +
  theme_classic() +
  # annotate("text", x = 0.085, y = 0.54, label = "0.53", ) +
  scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.05), minor_breaks = T) +
  scale_color_brewer(name = "", palette = "Set1") +
  labs(
    x = "\nRisk treshold for treatment",
    y = "Proportion treated\n"
  ) +
  theme(legend.position = c(0.8, 0.8)) +
  geom_vline(xintercept = c(0.075, 0.10, 0.15), linetype = "dotted")

ggsave("3_figures/treatment_curves_nhanes.pdf", p4, device = "pdf", width = 6, height = 4)

nhanes_design <-
  svydesign(
    id = ~ SDMVPSU,
    strata = ~ SDMVSTRA,
    weights = ~ WTINT2YR,
    data = nhanes_data_long,
    nest = TRUE
  )

cdf_cft <- svycdf(~value, subset(nhanes_design, name == "Counterfactual"))
cdf_fct <- svycdf(~value, subset(nhanes_design, name == "Factual"))

expand_grid(
  name = c("Factual", "Counterfactual"),
  x = seq(0, 1, by = 0.0005),
) %>%
  mutate(
    y = 1 - if_else(name == "Factual", cdf_fct$value(x), cdf_cft$value(x))
  ) %>%
  ggplot(., aes(x = x, y = y, color = name)) +
  geom_step() +
  theme_classic() +
  # annotate("text", x = 0.085, y = 0.54, label = "0.53", ) +
  scale_x_continuous(limits = c(0, 0.6), breaks = seq(0, 0.6, by = 0.05), minor_breaks = T) +
  scale_color_brewer(name = "", palette = "Set1") +
  labs(
    x = "\nRisk treshold for treatment",
    y = "Proportion treated\n"
  ) +
  theme(legend.position = c(0.8, 0.8)) +
  geom_vline(xintercept = c(0.075, 0.10, 0.15), linetype = "dotted")





