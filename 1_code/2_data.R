file_list <- c(
  "Exam1/Data/mesae1dres06192012.sas7bdat",
  "Exam2/Data/mesae2dres06222012.sas7bdat",
  "Exam3/Data/mesae3dres06222012.sas7bdat",
  "Exam4/Data/mesae4dres06222012.sas7bdat",
  "Exam5/Data/mesae5_drepos_20210920.sas7bdat",
  "Events/CVD/Data/mesaevthr2015_drepos_20200330.sas7bdat",
  "Events/NonCVD/Data/mesaevnoncvddres06192012.sas7bdat"
)

dats <- lapply(file_list, function (x) {
  df <- read_sas(get_data(x))
  names(df) <- tolower(names(df))
  return(df)
}) 

names(dats) <- gsub(".sas7bdat", "", file_list)
