
# impute ------------------------------------------------------------------

run_imps <- FALSE

if (run_imps) {
  
  # baseline imputations
  v <- c(baseline_vars, time_vars_wide, tv_vars_wide[grepl(".*1c?$", tv_vars_wide)])
  baseline_imps <- mice(
    data = mesa[, v], 
    m = 1, 
    seed = 34513
  )
  
  mesa[, v] <- complete(baseline_imps)
  
  # exam 2 imputations
  v <- c(baseline_vars, tv_vars_wide[grepl(".*2c?$", tv_vars_wide)])
  exam2_imps <- mice(
    data = mesa[, v], 
    m = 1, 
    seed = 34514
  )
  
  mesa[, v] <- complete(exam2_imps)
  
  # exam 3 imputations
  v <- c(baseline_vars, tv_vars_wide[grepl(".*[2-3]c?$", tv_vars_wide)])
  exam3_imps <- mice(
    data = mesa[, v], 
    m = 1, 
    seed = 34515
  )
  
  mesa[, v] <- complete(exam3_imps)
  
  # exam 4 imputations
  v <- c(baseline_vars, tv_vars_wide[grepl(".*[2-4]c?$", tv_vars_wide)])
  exam4_imps <- mice(
    data = mesa[, v], 
    m = 1, 
    seed = 34516
  )
  
  mesa[, v] <- complete(exam4_imps)
  
  # exam 5 imputations
  v <- c(baseline_vars, tv_vars_wide[grepl(".*[2-5]c?$", tv_vars_wide)])
  exam5_imps <- mice(
    data = mesa[, v], 
    m = 1, 
    seed = 34517
  )
  
  mesa[, v] <- complete(exam5_imps)
  
  write_rds(mesa, "0_data/imps.rds")
} else {
  mesa <- read_rds("0_data/imps.rds")
}
