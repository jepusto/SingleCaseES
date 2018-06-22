library(reshape)
library(dplyr)

data("Shogren")

Shogren <- Shogren %>%
  group_by(Study, Case, Measure) %>%
  mutate(session_number = 1:n(),
         scale = ifelse(Recording_procedure == "EC", "count", "proportion"),
         intervals = Session_length * 60/interval_length,
         smd_improvement = ifelse(direction == "increase", "decrease", "increase")) %>%
  ungroup()

ShogrenSMD <- batch_calc_ES(dat = Shogren,
                            condition = "Phase",
                            outcome = "outcome",
                            session_number = "session_number",
                            grouping_vars = c("Study", "Case"),
                            improvement = "smd_improvement",
                            ES = c("SMD"),
                            scale = "scale",
                            intervals = "intervals",
                            observation_length = "Session_length",
                            format = "wide",
                            std_dev = "both",
                            bias_correct = FALSE) %>%
  arrange(Study, Case)

ShogrenLRR <- batch_calc_ES(dat = Shogren,
                            condition = "Phase",
                            outcome = "outcome",
                            session_number = "session_number",
                            grouping_vars = c("Study", "Case"),
                            improvement = "direction",
                            ES = c("LRRd"),
                            scale = "scale",
                            intervals = "intervals",
                            observation_length = "Session_length",
                            format = "wide") %>%
  arrange(Study, Case)

ShogrenPND <- batch_calc_ES(dat = Shogren,
                            baseline_phase = "No Choice",
                            condition = "Phase",
                            outcome = "outcome",
                            session_number = "session_number",
                            grouping_vars = c("Study", "Case"),
                            improvement = "direction",
                            ES = c("PND"),
                            scale = "scale",
                            intervals = "intervals",
                            observation_length = "Session_length",
                            format = "wide") %>%
  arrange(Study, Case)

ShogrenLOR <- filter(Shogren, scale == "proportion") %>%
              batch_calc_ES(condition = "Phase",
                            outcome = "outcome",
                            session_number = "session_number",
                            grouping_vars = c("Study", "Case"),
                            ES = c("LOR"),
                            scale = "scale",
                            intervals = "intervals",
                            observation_length = "Session_length",
                            format = "wide",
                            D_const = 0) %>%
  arrange(Study, Case)

Shogren <- Shogren %>%
  mutate(outcome = ifelse(Measure == "Engagement", 1 - outcome, outcome))

y_bar <- cast(Shogren, Study + Case ~ Phase, value = "outcome", fun.aggregate = mean)[,3:4]
s_sq <- cast(Shogren, Study + Case ~ Phase, value = "outcome", fun.aggregate = var)[,3:4]
n_phase <- cast(Shogren, Study + Case ~ Phase, value = "outcome", fun.aggregate = length)[,3:4]

Case_data <- Shogren %>%
  group_by(Study, Case) %>%
  summarize(interval_length = first(interval_length))

len <- with(Case_data, ifelse(is.na(interval_length), 0, interval_length))

# calculate effect sizes

adj_log <- log(y_bar) + s_sq / (2 * n_phase * y_bar^2) 
log_RR2 <- adj_log[,1] - adj_log[,2]
SE_lRR <- sqrt(rowSums(s_sq / (n_phase * y_bar^2)))

logit <- function(x) log(x) - log(1 - x)
adj_logit <- logit(y_bar) - s_sq * (2 * y_bar - 1) / (2 * n_phase * y_bar^2 * (1 - y_bar)^2)
lOR2 <- adj_logit[,1] - adj_logit[,2]
SE_lOR <- sqrt(rowSums(s_sq / (n_phase * y_bar^2 * (1 - y_bar)^2)))
  
cll <- function(x) log(-log(1-x))
adj_cll <- cll(y_bar) + s_sq * (log(1 - y_bar) + 1) / (2 * n_phase * (1 - y_bar)^2 * log(1 - y_bar)^2)
cLLR2 <- adj_cll[,1] - adj_cll[,2]
SE_cll <- sqrt(rowSums(s_sq / (n_phase * (1 - y_bar)^2 * log(1 - y_bar)^2)))
 
lambert <- function(x, mu, l) ifelse(x > 0 & x < 1, 
  uniroot(function(lambda) 1 - lambda * exp(- l / lambda) / (mu + lambda) - x, 
          interval = c(10^-6,10^6))$root, NA)
mu_star <- rep(10, length(len))
lambda <- apply(y_bar, 2, function(y) mapply(lambert, x = y, mu = mu_star, l = len))
psi_P <- log(lambda[,2]) - log(lambda[,1])
V_piece <- (s_sq / n_phase) * lambda^2 * (mu_star + lambda)^2 / 
  ((1 - y_bar)^2 * (mu_star * lambda + len * (mu_star + lambda))^2)
SE_psi <- sqrt(rowSums(V_piece))

SMD <- (y_bar[,1] - y_bar[,2]) / sqrt(rowSums((n_phase - 1) * s_sq) / rowSums(n_phase - 1))
SE_SMD <- sqrt(rowSums(1 / n_phase) + SMD^2 / (2 * rowSums(n_phase - 1)))

PND <- unlist(by(Shogren, with(Shogren, list(Case, Study)), function(x)
  mean(x$outcome[x$Phase=="Choice"] < min(x$outcome[x$Phase=="No Choice"])), simplify=FALSE))

# compile ES data
ES <- cbind(as.data.frame(Case_data[,1:2]), log_RR2, SE_lRR, lOR2, SE_lOR, cLLR2, SE_cll, psi_P, SE_psi, SMD, SE_SMD, PND) %>%
  arrange(Study, Case)


test_that("LRRi, SMD, and PND are correct for the Pustejovksy data",{
  expect_equal(ShogrenLRR$LRRd_Est, ES$log_RR2)
  expect_equal(ShogrenSMD$SMD_Est, ES$SMD)
  expect_equal(ShogrenPND$PND_Est, ES$PND)
  expect_equal(ShogrenLOR$LOR_Est, ES$lOR2[!is.na(ES$lOR2)])
})
