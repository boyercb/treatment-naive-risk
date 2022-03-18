
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
#  "frncep1c",   # framingham risk score (NCEP)
  "htn1c",      # hypertension (calc)
  "htnstg1c",   # hypertension stage (calc)
  "bmi1c",      # bmi
  "htcm1",      # height
  "wtlb1",      # weight
  "hipcm1",     # hip circumference
  "waistcm1",   # waist circumference
  "cesd1c",     # CESD depression scale
  "chrbu61c",   # chronic burden > 6 months
  "discry1c",   # perceived discrimination 1 yr
  "emot1c",     # emotional support
  "hassl1c",    # hassle scale
  "splang1c",   # anger scale
  "splanx1c",   # anxiety scale 
  "preg1",      # ever been pregnant
  "pregn1",     # number of pregnancies
  "bcpills1",   # ever birth control pills
  "bpillyr1",   # birth control pill years
  "mnpause1",   # ever went through menopause
  "menoage1",   # menopause age
  "hrmrep1",    # hormone replacement therapy (ever)
  
  "cancer1",    # history of cancer
  "pmi1",       # family history of MI: parent
  "pstk1",      # family history of stroke: parent
  "shrtatt1",   # family history of MI: sibling
  "sstk1",      # family history of stroke: sibling
  "chrtatt1",   # family history of MI: child
  "cstk1",      # family history of stroke: child
  
  "diabins1",   # insulin or oral hypoglycemics
  "anydep1c",   # any anti-depressant
  "anara1c",    # any anti-arrythmic
  "vasoda1c",   # any vasodilator

  "nprob1c",    # neighborhood problems scale
  "highbp1",    # high blood pressure (self-reported)
  "bphxage1",   # age started bp meds
  "hghchol1",   # high cholesterol (self-reported)
  "aspirin1",   # aspirin ever
  "asacat1c",   # aspirin use
  "aspsage1",   # age started aspirin
  "htnmed1c",   # anti-hypertensive meds
  "lipid1c",    # lipid-lowering meds
  "marital1",   # marital status
  "educ1",      # educational attainment
  "curjob1",    # employment status
  "income1",    # income
  "hinone1",    # no health insurance
  "evsmk1",     # ever smoked
  "cursmk1",    # current cigarette smoker
  "alcohol1",   # ever drank alcohol
  "alcwk1c",    # alcoholic drinks per week
  "sbp1c",      # mean sbp
  "dbp1c",      # mean dbp
  "chol1",      # total cholesterol
  "hdl1",       # HDL cholesterol
  "ldl1",       # LDL cholesterol
  "trig1",      # Triglycerides
  "crp1",       # CRP
  "il61",       # IL-6
  "agatpm1c",   # CAC score
  "ecglvh1c",   # left ventricular hypertrophy
  "afib1c",     # afib on ECG
  "exercm1c"    # exercise per week
)

exam2_vars <- c(
  "age2c",      # age 
  "dm032c",     # diabetes (calc)
  "frncep2c",   # framingham risk score (NCEP)
  "htn2c",      # hypertension (calc)
  "bmi2c",      # bmi
  "htcm2",      # height
  "wtlb2",      # weight
  "hipcm2",     # hip circumference
  "waistcm2",   # waist circumference
  "asacat2c",   # aspirin use
  "lipid2c",    # lipid-lowering meds
  "htnmed2c",   # anti-hypertensive meds
  "diabins2",   # insulin or oral hypoglycemics
  "wtls2c",     # any weight loss drugs
  "diur2c",     # any diuretic
  "anydep2c",   # any anti-depressant
  "anara2c",    # any anti-arrythmic
  "vasoda2c",   # any vasodilator
  "empstat2",   # change in employment status?
  "curjob2",    # employment status
  "income2",    # income
  "hinone2",    # no health insurance
  "smkstat2",   # smoking status
  "cursmk2",    # current cigarette smoker
  "curalc2",    # drinking status
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
  "q09wmcm2",   # exercise: moderate walking
  "q10smcm2",   # exercise: moderate dance
  "q11svcm2",   # exercise: vigorous team sports
  "q12svcm2",   # exercise: vigorous dual sports
  "q13smcm2",   # exercise: moderate individual activities
  "q14cmcm2",   # exercise: moderate conditioning
  "q15cvcm2"    # exercise: vigorous conditioning
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


# NHANES ------------------------------------------------------------------


# load dataframes for NHANES files and variable names
data_files <- nhanes_data_files(cache = TRUE)
variables <- nhanes_variables(cache = TRUE)

# pull out just medication files, bp & lipid questionnaire, and lipid labs
orphan_exams <- c("LAB13AM", "L13AM_C", "L13AM_B", "LAB13", "L13_B", "L13_C")
search_string <- "(BPQ)|(TRIGLY)|(RXQ_RX)|((BPX$)|(BPX_))|(HDL)|(TCHOL)|((SMQ$)|(SMQ_))|(DIQ)|(MCQ)"
nhanes_data_files <- filter(
  data_files,
  str_detect(data_file_name, search_string) | data_file_name %in% orphan_exams
)

# remove weird pandemic exam cycle  
nhanes_data_files <- filter(nhanes_data_files, cycle == "2017-2018")

# add variable indicating the data type
nhanes_data_files <- mutate(
  nhanes_data_files,
  data_type = case_when(
    str_detect(data_file_name, "TRIGLY") | data_file_name %in% orphan_exams[1:3] ~ "TRIGLY",
    str_detect(data_file_name, "TCHOL") | data_file_name %in% orphan_exams[4:6] ~ "TCHOL",
    str_detect(data_file_name, "HDL") ~ "HDL",
    str_detect(data_file_name, "BPQ") ~ "BPQ",
    str_detect(data_file_name, "RXQ") ~ "RXQ",
    str_detect(data_file_name, "BPX") ~ "BPX",
    str_detect(data_file_name, "SMQ") ~ "SMQ",
    str_detect(data_file_name, "DIQ") ~ "DIQ",
    str_detect(data_file_name, "MCQ") ~ "MCQ"
  )
)

# sort by type and cycle
nhanes_data_files <- arrange(nhanes_data_files, data_type, cycle)

# limit to just variables of interest
nhanes_data_files <- select(nhanes_data_files, data_type, cycle, data_file_name)

# pivot to wider to make merging easier
nhanes_data_files <- pivot_wider(
  data = nhanes_data_files,
  names_from = data_type, 
  values_from = data_file_name
)

nhanes_data_files$HDL[is.na(nhanes_data_files$HDL)] <- orphan_exams[4:6]


# create lists of variables names to select -------------------------------

id_vars <- c("SEQN")
design_vars <- c("cycle", "SDMVPSU", "SDMVSTRA", "WTINT2YR")
demo_vars <- c("RIDAGEYR", "RIAGENDR", "RIDRETH1")

BPQ_vars <- c(
  id_vars,
  design_vars,
  demo_vars,
  "BPQ100D",
  "BPQ050A"
)

RXQ_vars <- c(
  id_vars,
  "RXDDRGID"
)

BPX_vars <- c(
  id_vars,
  "BPXSY1",
  "BPXSY2",
  "BPXSY3",
  "BPXSY4"
)

TRIGLY_vars <- c(
  id_vars,
  "LBDLDL"
  #"WTSAF2YR"
)

HDL_vars <- c(
  id_vars,
  "LBDHDD",
  "LBDHDL"
)

TCHOL_vars <- c(
  id_vars,
  "LBXTC"
)

SMQ_vars <- c(
  id_vars,
  "SMQ040"
)

DIQ_vars <- c(
  id_vars,
  "DIQ010"
)

MCQ_vars <- c(
  id_vars,
  "MCQ160C",
  "MCQ160D",
  "MCQ160E",
  "MCQ160F"
)

RX_LIST <- c(
  "d04105",
  "d00746",
  "d00280",
  "d00348",
  "d04851",
  "d07637",
  "d05348",
  "d03183"
)


# download and merge data -------------------------------------------------

nhanes_data <- pmap_dfr(as.list(nhanes_data_files), download_and_merge,
                       rx_list = RX_LIST,
                       BPQ_vars = BPQ_vars,
                       RXQ_vars = RXQ_vars,
                       BPX_vars = BPX_vars,
                       TRIGLY_vars = TRIGLY_vars,
                       HDL_vars = HDL_vars,
                       TCHOL_vars = TCHOL_vars,
                       SMQ_vars = SMQ_vars,
                       DIQ_vars = DIQ_vars,
                       MCQ_vars = MCQ_vars)

nhanes_data <- 
  rename(nhanes_data,
         age = RIDAGEYR,
         gender = RIAGENDR,
         raceethnicity = RIDRETH1,
         totchol = LBXTC,
         hdl = LBDHDD,
         ldl = LBDLDL,
         liprx = BPQ100D,
         hrx = BPQ050A,
         smoker = SMQ040,
         diabetes = DIQ010)


# limit to adults over 40 and less than 79
nhanes_data <- filter(nhanes_data, age >= 40 & age < 80)

# create useful variables
nhanes_data <- mutate(
  nhanes_data,
  liprx = if_else(liprx == "No" | is.na(liprx), 0, 1),
  liprx = if_else(liprx == 1 | on_statin == 1, 1, 0),
  hrx = if_else(hrx == "No" | is.na(hrx), 0, 1),
  diabetes = if_else(diabetes == "No", 0, 1),
  smoker = if_else(!is.na(smoker), 1, 0),
  gender = tolower(gender),
  # hdl = if_else(!is.na(LBDHDL), LBDHDL, hdl),
  age = if_else(age %in% c("80 years of age and over", "81", "82", "83", "84"), "80", age),
  prior_ascvd = if_else(MCQ160C == "Yes" | MCQ160D == "Yes" | MCQ160E == "Yes" | MCQ160F == "Yes", 1, 0),
  sbp = rowMeans(select(nhanes_data, "BPXSY1", "BPXSY2", "BPXSY3", "BPXSY4"), na.rm = TRUE)
) %>%
  select(
    age,
    gender,
    raceethnicity,
    totchol,
    sbp,
    hdl,
    ldl,
    liprx,
    hrx,
    smoker,
    diabetes,
    prior_ascvd,
    SEQN,
    cycle,
    SDMVPSU,
    SDMVSTRA,
    WTINT2YR
  ) %>%
  filter(prior_ascvd == 0) %>%
  drop_na(-ldl)
