
# load SAS datasets -------------------------------------------------------

file_list <- c(
  "Exam1/Data/mesae1dres06192012.sas7bdat",
  "Exam2/Data/mesae2dres06222012.sas7bdat",
  "Exam3/Data/mesae3dres06222012.sas7bdat",
  "Exam4/Data/mesae4dres06222012.sas7bdat",
  "Exam5/Data/mesae5_drepos_20210920.sas7bdat",
  "Events/CVD/Data/mesaevthr2015_drepos_20200330.sas7bdat"
  # "Events/NonCVD/Data/mesaevnoncvddres06192012.sas7bdat"
)

dats <- lapply(file_list, function (x) {
  df <- read_sas(get_data(x))
  names(df) <- tolower(names(df))
  return(df)
}) 

names(dats) <- c(
  "exam1",
  "exam2",
  "exam3",
  "exam4",
  "exam5",
  "cvd"
  # "noncvd"
)


# merge into one analytic dataset -----------------------------------------

exam1_vars <- c(
  "age1c",      # age 
  "gender1",    # gender
  "race1c",     # race/ethnicity
  "dm031c",     # diabetes (calc)
  "frncep1c",   # framingham risk score (NCEP)
  "htn1c",      # hypertension (calc)
  "htnstg1c",   # hypertension stage (calc)
  "bmi1c",      # bmi
  "highbp1",    # high blood pressure (self-reported)
  "bphxage1",   # age started bp meds
  "hghchol1",   # high cholesterol (self-reported)
  "cholage1",   # age started chol meds
  "diabet1",    # diabetes (self-reported)
  "aspnow1",    # aspirin use
  "aspsage1",   # aspirin use now
  "htnmed1c",   # anti-hypertensive meds
  "lipid1c",    # lipid-lowering meds
  "marital1",   # marital status
  "educ1",      # educational attainment
  "curjob1",    # employment status
  "income1",    # income
  "hinone1",    # no health insurance
  "evsmk1",     # ever smoked
  "cursmk1",    # current cigarette smoker
  "alcwk1c",    # alcoholic drinks per week
  "sbp1c",      # mean sbp
  "dbp1c",      # mean dbp
  "chol1",      # total cholesterol
  "hdl1",       # HDL cholesterol
  "ldl1",       # LDL cholesterol
  "trig1",      # Triglycerides
  "crp1",       # CRP
  "exercm1c"    # exercise per week
)

exam2_vars <- c(
  "age2c",      # age 
  "dm032c",     # diabetes (calc)
  "frncep2c",   # framingham risk score (NCEP)
  "htn2c",      # hypertension (calc)
  "bmi2c",      # bmi
  "aspirin2",   # aspirin use
  "lipid2c",    # lipid-lowering meds
  "htnmed2c",   # anti-hypertensive meds
  "empstat2",   # change in employment status?
  "curjob2",    # employment status
  "income2",    # income
  "hinone2",    # no health insurance
  "cursmk2",    # current cigarette smoker
  "rwinewk2",   # glasses of red wine per week
  "wwinewk2",   # glasses of white wine per week
  "beerwk2",    # beers per week
  "liqwk2",     # drinks of liquor per week
  "sbp2c",      # mean sbp
  "dbp2c",      # mean dbp
  "chol2",      # total cholesterol
  "hdl2",       # HDL cholesterol
  "ldl2",       # LDL cholesterol
  "trig2",      # Triglycerides
  "q09wmcm2",   # exercise per week
  "q10smcm2",
  "q11svcm2",
  "q12svcm2",
  "q13smcm2",
  "q14cmcm2",
  "q15cvcm2"
)

exam3_vars <- str_replace(str_replace(exam2_vars, "2$", "3"), "2c$", "3c")
exam4_vars <- str_replace(str_replace(exam2_vars, "2$", "4"), "2c$", "4c")
exam5_vars <- str_replace(str_replace(exam2_vars, "2$", "5"), "2c$", "5c")
  
exam4_vars <- exam4_vars[!exam4_vars %in% c("income4", 
                                            "q09wmcm4",
                                            "q10smcm4",
                                            "q11svcm4",
                                            "q12svcm4",
                                            "q13smcm4",
                                            "q14cmcm4",
                                            "q15cvcm4")]

cvd_vars <- c(
  "fuptt",
  "prebase",
  "exall",
  "ang",
  "angtype",
  "angtt",
  "dth",
  "dthtype",
  "dthtext",
  "dthtt",
  "chdh",
  "chdhtt",
  "chda",
  "chdatt",
  "cvdh",
  "cvdhtt",
  "cvda",
  "cvdatt"
)

mesa_vars <-
  list(exam1_vars,
       exam2_vars,
       exam3_vars,
       exam4_vars,
       exam5_vars,
       cvd_vars)

id_vars <- c("mesaid")
time_vars <- c(
  "e12dyc",     
  "e13dyc",
  "e14dyc",
  "e15dyc"
)

dats <- lapply(seq(1, length(dats)), function(x) {
  if (x == 1) {
    dats[[x]][, c(id_vars, mesa_vars[[x]])]
  } else if (x <= 5) {
    dats[[x]][, c(id_vars, time_vars[x-1], mesa_vars[[x]])]
  } else {
    dats[[x]][, c(id_vars, mesa_vars[[x]])]
  }
})

mesa <- Reduce(function(df1, df2) left_join(df1, df2, by = id_vars), dats)

rm(dats)


# create long version -----------------------------------------------------

mesa <- 
  mesa %>%
  rename(
    time2 = e12dyc,
    time3 = e13dyc,
    time4 = e14dyc,
    time5 = e15dyc
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


