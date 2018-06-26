context("Pustejovsky (2015) examples.")

library(dplyr)

data("Shogren")

Shogren <- Shogren %>%
  group_by(Study, Case, Measure) %>%
  mutate(
    session_number = row_number(),
    scale = ifelse(Recording_procedure == "EC", "count", "proportion"),
    intervals = Session_length * 60 / interval_length,
    smd_improvement = ifelse(direction == "increase", "decrease", "increase")
  ) %>%
  ungroup()

ShogrenSMD <- batch_calc_ES(dat = Shogren,
                            condition = "Phase",
                            outcome = "outcome",
                            session_number = "session_number",
                            grouping_vars = c("Study", "Measure","Case"),
                            improvement = "smd_improvement",
                            ES = "SMD",
                            scale = "scale",
                            intervals = "intervals",
                            observation_length = "Session_length",
                            format = "wide",
                            std_dev = "both",
                            bias_correct = FALSE) %>%
  arrange(Study, Case)

Shogren_LRR_PND <- 
  batch_calc_ES(dat = Shogren,
                condition = "Phase",
                outcome = "outcome",
                session_number = "session_number",
                grouping_vars = c("Study", "Measure", "Case"),
                improvement = "direction",
                ES = c("PND", "LRRd"),
                scale = "scale",
                intervals = "intervals",
                observation_length = "Session_length",
                format = "wide") %>%
  arrange(Study, Case)

ShogrenLOR <- 
  Shogren %>%
  filter(scale == "proportion") %>%
  batch_calc_ES(
    condition = "Phase",
    outcome = "outcome",
    session_number = "session_number",
    grouping_vars = c("Study", "Measure", "Case"),
    improvement = "direction",
    ES = "LOR",
    scale = "scale",
    intervals = "intervals",
    observation_length = "Session_length",
    format = "wide",
    D_const = 0
  ) %>%
  arrange(Study, Case)

# Calculate ES by hand 

Shogren <- 
  Shogren %>%
  mutate(outcome = ifelse(Measure == "Engagement", 1 - outcome, outcome))

logit <- function(x) log(x) - log(1 - x)

ES <- 
  Shogren %>%
  group_by(Study, Measure, Case, Phase) %>%
  summarise(
    interval_length = first(interval_length),
    y_bar = mean(outcome), 
    s_sq = var(outcome), 
    n_phase = length(outcome)
  ) %>%
  mutate(
    interval_length = ifelse(is.na(interval_length), 0, interval_length),
    adj_log = log(y_bar) + s_sq / (2 * n_phase * y_bar^2),
    V_log = s_sq / (n_phase * y_bar^2),
    adj_logit = ifelse(y_bar < 1, logit(y_bar) - s_sq * (2 * y_bar - 1) / (2 * n_phase * y_bar^2 * (1 - y_bar)^2), NA),
    V_logit = ifelse(y_bar < 1, s_sq / (n_phase * y_bar^2 * (1 - y_bar)^2), NA)
  ) %>%
  summarise(
    log_RR2 = -diff(adj_log),
    SE_lRR = sqrt(sum(V_log)),
    lOR2 = -diff(adj_logit),
    SE_lOR = sqrt(sum(V_logit)),
    MD = -diff(y_bar),
    S_pool = sqrt(sum((n_phase - 1) * s_sq) / sum(n_phase - 1)),
    V_MD = sum(1 / n_phase),
    df = sum(n_phase - 1)
  ) %>%
  mutate(
    SMD = MD / S_pool,
    SE_SMD = sqrt(V_MD + SMD^2 / (2 * df))
  )

ES <- 
  Shogren %>%
  group_by(Study, Measure, Case) %>%
  summarise(
    PND = mean(outcome[Phase == "Choice"] < min(outcome[Phase == "No Choice"]))
  ) %>%
  full_join(ES, by = c("Study", "Measure", "Case")) %>%
  full_join(Shogren_LRR_PND, by = c("Study", "Measure", "Case")) %>%
  full_join(ShogrenSMD, by = c("Study", "Measure", "Case")) %>%
  full_join(ShogrenLOR, by = c("Study", "Measure", "Case"))



test_that("LRRd, LOR, SMD, and PND are correct for the Pustejovsky (2015) data", {
  expect_equal(ES$LRRd_Est, ES$log_RR2)
  expect_equal(ES$LRRd_SE, ES$SE_lRR)
  expect_equal(ES$LOR_Est, -ES$lOR2)
  expect_equal(ES$LOR_SE, ES$SE_lOR)
  expect_equal(ES$SMD_Est, ES$SMD)
  expect_equal(ES$SMD_SE, ES$SE_SMD)
  expect_equal(ES$PND_Est, ES$PND)
})
