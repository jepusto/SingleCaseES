library(tidyverse)
library(readxl)

Lane_2007 <- 
  read_excel("auxiliary/Lane.2007.EC.74.1.47.xlsx") %>%
  mutate(
    Metric = if_else(OutcomeName == "Academic Engagement", "percentage", "rate")
  )

LaneECES <- 
  batch_calc_ES(
    dat = Lane_2007,
    grouping = c(CaseName, OutcomeName),
    condition = CondNum,
    outcome = Y,
    ES = c("LRRi","LRRd","NAP"),
    improvement = Direction,
    scale = Metric,
    bias_correct = TRUE,
    confidence = NULL,
    format = "wide"
  )

LaneECES
