context("Format arguments work properly.")

library(dplyr)
library(tidyr)

A_data <- rbinom(5, 14:18, 0.3) * 100 / (14:18)
B_data <- rbinom(5, 15, 0.2) * 100 / 15

ES_long <- 
  calc_ES(
    A_data = A_data, B_data = B_data, ES = "all", 
    scale = "percentage", improvement = "decrease",
    goal = 10,
    confidence = .90
  ) %>%
  arrange(ES) 

test_that("Long format works properly.", {

  expect_identical(names(ES_long), c("ES","Est","SE","CI_lower","CI_upper", "baseline_SD"))
  expect_true(all(is.na(with(ES_long, SE[ES %in% c("IRD","PAND","PND","PEM","Tau-U")]))))
  expect_true(all(!is.na(with(ES_long, SE[ES %in% c("LRRd","LRRi","LOR","SMD","NAP","Tau", "Tau-BC", "PoGO")]))))
  expect_identical(ES_long$ES, sort(c("LRRd","LRRi","LOR","SMD","LRM","NAP","IRD","PAND","PND","PEM","Tau","Tau-U","Tau-BC", "PoGO")))

})

test_that("Wide format works for ES = 'all' when confidence is specified", {
  
  ES_wide <- calc_ES(A_data = A_data, B_data = B_data, ES = "all",
                     scale = "percentage", improvement = "decrease",
                     goal = 10,
                     confidence = .90, format = "wide")
  
  ES_to_long <- 
    ES_wide %>%
    gather("q","v") %>%
    separate(q, into = c("ES","q"), sep = "_", extra = "merge") %>%
    spread(q, v, fill = NA) %>%
    select(ES, Est, SE, CI_lower, CI_upper, ends_with("_SD"))
  
  expect_identical(ES_long, ES_to_long)
})

test_that("Wide format works for ES = 'all', when confidence is NULL", {
  
  ES_wide <- calc_ES(A_data = A_data, B_data = B_data, ES = "all",
                     scale = "percentage", improvement = "decrease", 
                     goal = 10,
                     confidence = NULL, format = "wide")
  
  ES_to_long <- 
    ES_wide %>%
    gather("q","v") %>%
    separate(q, into = c("ES","q"), sep = "_", extra = "merge") %>%
    spread(q, v, fill = NA) 
  
  expect_identical(ES_long$ES, ES_to_long$ES)
  expect_identical(ES_long$Est, ES_to_long$Est)
  expect_identical(ES_long$SE, ES_to_long$SE)
})

test_that("Wide format works for ES = 'parametric', when confidence is specified", {
  
  parametric_long_pooled_SD <- 
    calc_ES(
      A_data = A_data, B_data = B_data, ES = "all", 
      scale = "percentage", improvement = "decrease",
      goal = 10,
      std_dev = "pooled", confidence = .90
    ) %>%
    arrange(ES) %>% 
    dplyr::filter(ES %in% c("LOR","LRRd","LRRi","SMD","LRM","PoGO"))
  
  ES_wide <- calc_ES(A_data = A_data, B_data = B_data, ES = "parametric",
                     scale = "percentage", improvement = "decrease", 
                     goal = 10,
                     std_dev = "pooled", confidence = .90, format = "wide")
  
  ES_to_long <- 
    ES_wide %>%
    gather("q","v") %>%
    separate(q, into = c("ES","q"), sep = "_", extra = "merge") %>%
    spread(q, v, fill = NA) %>%
    select(ES, Est, SE, CI_lower, CI_upper, ends_with("_SD"))
  
  expect_identical(parametric_long_pooled_SD, ES_to_long)
})

test_that("Wide format works for ES = 'parametric', when confidence is NULL", {
  
  ES_wide <- calc_ES(A_data = A_data, B_data = B_data, ES = "parametric",
                     scale = "percentage", improvement = "decrease", 
                     goal = 10,
                     confidence = NULL, format = "wide")
  
  ES_to_long <- 
    ES_wide %>%
    gather("q","v") %>%
    separate(q, into = c("ES","q"), sep = "_", extra = "merge") %>%
    spread(q, v, fill = NA) %>%
    select(ES, Est, SE)
  
  parametric_long <- ES_long %>% dplyr::filter(ES %in% c("LOR","LRRd","LRRi","SMD","LRM", "PoGO"))
  
  expect_identical(parametric_long$ES, ES_to_long$ES)
  expect_identical(parametric_long$Est, ES_to_long$Est)
  expect_identical(parametric_long$SE, ES_to_long$SE)
})

test_that("Wide format works for ES = 'NOM', when confidence is specified", {
  
  ES_wide <- calc_ES(A_data = A_data, B_data = B_data, ES = "NOM",
                     scale = "percentage", improvement = "decrease", 
                     confidence = .90, format = "wide")
  
  ES_to_long <- 
    ES_wide %>%
    gather("q","v") %>%
    separate(q, into = c("ES","q"), sep = "_", extra = "merge") %>%
    spread(q, v, fill = NA)  %>%
    select(ES, Est, SE, CI_lower, CI_upper)
  
  NOM_long <- 
    ES_long %>% 
    dplyr::filter(ES %in% c("NAP","IRD","PAND","PND","PEM","Tau","Tau-U","Tau-BC")) %>% 
    dplyr::select(-ends_with("_SD"))
  
  expect_identical(NOM_long, ES_to_long)
})


test_that("Wide format works for ES = 'NOM', when confidence is NULL", {
  
  ES_wide <- calc_ES(A_data = A_data, B_data = B_data, ES = "NOM",
                     scale = "percentage", improvement = "decrease", 
                     confidence = NULL, format = "wide")
  
  ES_to_long <- 
    ES_wide %>%
    gather("q","v") %>%
    separate(q, into = c("ES","q"), sep = "_", extra = "merge") %>%
    spread(q, v, fill = NA)
  
  NOM_long <- 
    ES_long %>% 
    dplyr::filter(ES %in% c("NAP","IRD","PAND","PND","PEM","Tau","Tau-U","Tau-BC")) %>% 
    dplyr::select(-ends_with("_SD"))
  
  expect_identical(NOM_long$ES, ES_to_long$ES)
  expect_identical(NOM_long$Est, ES_to_long$Est)
  expect_identical(NOM_long$SE, ES_to_long$SE)
})


test_that("Wide format works for other NOMs.", {
  
  other_noms <- c("IRD","PAND","PND","PEM","Tau-U")
  
  ES_wide <- calc_ES(A_data = A_data, B_data = B_data, ES = other_noms,
                     scale = "percentage", improvement = "decrease", 
                     confidence = NULL, format = "wide")
  
  expect_identical(names(ES_wide), paste(other_noms, "Est", sep = "_"))
  
  ES_to_long <- 
    ES_wide %>%
    gather("q","v") %>%
    separate(q, into = c("ES","q"), sep = "_", extra = "merge") %>%
    spread(q, v, fill = NA)
  
  NOM_long <- ES_long %>% dplyr::filter(ES %in% c("IRD","PAND","PND","PEM","Tau-U"))
  
  expect_identical(NOM_long$ES, ES_to_long$ES)
  expect_identical(NOM_long$Est, ES_to_long$Est)
  
  ES_wide_CI <- calc_ES(A_data = A_data, B_data = B_data, ES = other_noms,
                        scale = "percentage", improvement = "decrease", 
                        confidence = .90, format = "wide")
  expect_identical(ES_wide_CI, ES_wide)
  
})
