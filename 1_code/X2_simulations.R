# create simulation grid --------------------------------------------------

# the goal of these simulations is to show that:
# - snaftm consistently estimates the STR parameter (bias, variance, coverage)
# - snaftm-based risk prediction has correct mse (compare with oracle)
# - single vs. doubly-robust estimation?

N_sims <- 100

grid <- expand_grid(
  sim = 1:N_sims,
  psi = c(0, -0.25, -0.5)
)

sim_results <- 
  pmap(as.list(grid), function(sim, psi) {
    # generate data 
    df <- generate_snaftm(
      N = 5000, 
      P = 1,
      K = 10,
      gamma = function(L, A, k) {
        exp(psi * A)
      }
    )
    
    # convert to proper format
    df <-
      df %>%
      pivot_snaftm_longer()
    
    # singly robust estimation
    sr <- snaftm(
      model = Surv(T, event) ~ A,
      tx.formula = A ~ L1 + lag1_A,
      tx.family = binomial(link = "logit"),
      data = df,
      time = "time",
      id = "id",
      K.max = 10,
      psi.range = c(-4, 4),
      conf.int = TRUE
    )
    
    # doubly robust estimation
    dr <- snaftm(
      model = Surv(T, event) ~ A,
      tx.formula = A ~ L1 + lag1_A,
      tx.family = binomial(link = "logit"),
      dr.formula = Surv(X.psi, delta.psi) ~ L1 + lag1_L1,
      dr.family = "coxph",
      data = df,
      time = "time",
      id = "id",
      K.max = 10,
      psi.range = c(-4, 4),
      conf.int = TRUE
    )
    
    # join result and return
    res <- left_join(sr, dr, by = "term", suffix = c("_sr", "_dr"))
    return(res)
    
  })

bind_rows(sim_results) %>% 
  bind_cols(grid) %>%
  pivot_longer(
    cols = matches("_[sd]r"),
    names_to = c(".value", "type"),
    names_pattern = "(.*)_(.*)"
  ) %>%
  ggplot(., aes(x = type, y = estimate, fill = factor(psi))) +
    geom_violin(trim = FALSE, position=position_dodge(1)) +
    geom_boxplot(width = 0.1, position=position_dodge(1)) +
    geom_hline(yintercept = c(-0.5, -0.25, 0), linetype = "dotted") +
    scale_fill_brewer(palette = "Set1") +
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major = element_blank()
    )
  


 # old code 


# with_progress({
#   ests1 <- vector(length = 200)
#   ests2 <- vector(length = 200)
#   p <- progressor(steps = length(ests1))
#   
#   for (i in 1:length(ests1)) {
#     df <- generate_snaftm(N = 5000)
#     
#     df <-
#       df %>%
#       as_tibble() %>%
#       pivot_longer(
#         cols = matches("(^C)|(^D)|(^Y)|(^A)|(^L1)"), 
#         names_to = c(".value", "time"),
#         names_pattern = c("(.*)_(.*)")
#       ) %>%
#       group_by(id) %>%
#       mutate(
#         time = as.integer(time),
#         event = as.numeric(T < 10),
#         eventtime = if_else(event == 1, as.integer(T + 1), as.integer(T)),
#         Y = lead(Y, 1),
#         lag1_L1 = lag(L1, 1, default = 0),
#         lag1_A = lag(A, 1, default = 0)
#       ) %>%
#       filter(time < eventtime) %>%
#       ungroup()
#     
#     ests1[i] <- snaftm(
#       model = Surv(T, event) ~ A,
#       tx.formula = A ~ L1 + lag1_A,
#       tx.family = binomial(link = "logit"),
#       data = df,
#       time = "time",
#       id = "id",
#       K.max = 10,
#       psi.range = c(-2.75, 1.75)
#     )
#     
#     ests2[i] <- snaftm(
#       model = Surv(T, event) ~ A,
#       tx.formula = A ~ L1 + lag1_A,
#       tx.family = binomial(link = "logit"),
#       dr.formula = Surv(X.psi, delta.psi) ~ L1 + lag1_L1,
#       dr.family = "coxph",
#       data = df,
#       time = "time",
#       id = "id",
#       K.max = 10,
#       psi.range = c(-2.75, 1.75)
#     )
#     p()
#   }
#   
# })

# tibble(
#   ests1 = ests1,
#   ests2 = ests2
# ) %>%
#   pivot_longer(everything()) %>%
#   ggplot(., aes(x = name, y = value, fill = name)) +
#   geom_violin(trim = FALSE, ) + 
#   geom_boxplot(width = 0.1, fill = "white") + 
#   geom_hline(yintercept = -0.5, linetype = "dotted") +
#   scale_fill_brewer(palette = "Set1") +
#   theme_bw() +
#   theme(
#     panel.grid.minor = element_blank(),
#     panel.grid.major = element_blank()
#   )
