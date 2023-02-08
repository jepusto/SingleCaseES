# Load data
library(readxl)
library(janitor)

dat <- 
  read_excel(path = "{user_path}", sheet = "{user_sheet}") %>%  # Modify the path to the full location of your file
  clean_names(case = "parsed")
