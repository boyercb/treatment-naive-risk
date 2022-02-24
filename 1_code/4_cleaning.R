
# create long version -----------------------------------------------------

mesa <- 
  mesa %>%
  rename(
    time2 = e12dyc,
    time3 = e13dyc,
    time4 = e14dyc,
    time5 = e15dyc
  ) 

mesa <- 
  mesa %>%
  mutate(
    ascvd_10yr_frs_risk = ascvd_10yr_frs(
      gender = if_else(gender1 == 1, "female", "male"), 
      age = age1c,
      totchol = chol1, 
      hdl = hdl1, 
      sbp = sbp1c,
      bp_med = htnmed1c, 
      smoker = cursmk1, 
      diabetes = diabet1
    ),
    initiator = pmax(lipid2c, lipid3c, lipid4c, lipid5c, na.rm = TRUE)
  )

# pivot to longer format
mesa_long <- 
  mesa %>%
  pivot_longer(
    cols = matches(".*[2-5]c?$"),
    names_to = c("variable", "exam"),
    names_pattern = "(.*)([2-5])"
  ) %>%
  pivot_wider(
    names_from = "variable", 
    values_from = "value"
  )


# clean baseline and time-varying covariates ------------------------------

mesa_long <- 
  mesa_long %>%
  mutate(
    # baseline covariates
    
    # marital status
    married = if_else(marital1 == 1, 1, 0),
    
    # education
    educ = case_when(
      educ1 %in% c(0, 1, 2) ~ 1,    # less than high school
      educ1 %in% c(3, 4, 5, 6) ~ 2, # high school or some college
      educ1 %in% c(7, 8) ~ 3        # bachelor's or postgraduate degree
    ),
    
    # baseline employment
    employed1 = if_else(curjob1 == 2, 1, 0),            # full time
    retired1 = if_else(curjob1 %in% c(8, 9, 10), 1, 0), # retired
    
    # baseline exercise
    exercise1 = exercm1c,
    
    # baseline drinks per week
    dpw1 = alcwk1c,
    
    # time-varying covariates
    
    # calculate physical activity
    exercise = rowSums(select(.data, q09wmcm:q15cvcm)),
    
    # calculate drinks per week
    dpw = rowSums(select(.data, rwinewk:liqwk))
    
  )


# clean outcomes ----------------------------------------------------------

# mesa_long <-
#   mesa_long %>%
#   mutate(
#     
#   )