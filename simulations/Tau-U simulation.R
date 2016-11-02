library(stringr)
library(tidyr)
library(dplyr, warn.conflicts = FALSE)
library(multidplyr)
library(ggplot2)
library(SingleCaseES)

rm(list = ls())

m <- 10
n <- 10
beta <- 0.05
delta <- 0.5
iterations <- 1000

yA <- rnorm(m) + beta * (1:m)
yB <- rnorm(n, mean = delta) + beta * (m + 1:n)
y <- c(yA, yB)
x <- c(m + 1 - (1:m), rep(m + 1, n))
cor(x, y, method = "kendall")
cor(1:m, yA) * (m * (m - 1) / 2)
Tau(yA, yB)$Est - cor(1:m, yA) * (m * (m - 1) / 2) / (m * n)
Tau_U(yA, yB)
Tau_U(yA, yB) * (m * n) / (m * (2 * n + m - 1) / 2)
m * n
m * (m - 1) / 2

A_data <- c(2, 3, 5, 3, 4, 6)
B_data <- c(4, 3, 5, 5, 4, 8, 7, 6)
improvement <- "increase"

Tau_U_expanded <- function(A_data, B_data, improvement = "increase") {
  
  if (improvement=="decrease") {
    A_data <- -1 * A_data
    B_data <- -1 * B_data
  }
  
  A_data <- A_data[!is.na(A_data)]
  B_data <- B_data[!is.na(B_data)]
  m <- length(A_data)
  n <- length(B_data)
  
  Q_P <- sapply(B_data, function(j) (j > A_data) - (j < A_data))
  Q_B <- sapply(A_data, function(j) (j > A_data) - (j < A_data))
  lower_tri <- lower.tri(Q_B)
  Q_B[lower_tri] <- -1 * Q_B[lower_tri]
  
  Tau_A <- sum(Q_B) / (m * (m - 1))
  Tau_P <- sum(Q_P) / (m * n)
  TauU <- Tau_P - Tau_A * (m - 1) / (2 * n)
  
  S1_sq <- sum((colSums(Q_B) / (m - 1) - Tau_A)^2) / (m - 1)
  S2_sq <- (sum(Q_B^2) - m * (m - 1) * Tau_A^2) / (m * (m - 1) - 1)
  V_A <- (4 * (m - 2) * S1_sq + 2 * S2_sq) / (m * (m - 1))
  
  Q1 <- 4 * sum(rowSums((Q_P + 1) / 2)^2) / (m * n^2)
  Q2 <- 4 * sum(colSums((Q_P + 1) / 2)^2) / (m^2 * n)
  V_Tau <- ((1 + Tau_P) * (1 - Tau_P) 
            + (n - 1) * (Q1 - (1 + Tau_P)^2) + 
              (m - 1) * (Q2 - (1 + Tau_P)^2)) / (m * n)
  
  V_TauU <- V_Tau + V_A * (m - 1)^2 / (4 * n^2)
  
  c(Tau_A = Tau_A, Tau_P = Tau_P, Tau_U = TauU,
    V_A = V_A, V_P = V_Tau, V_U = V_TauU)
}

Tau_U_expanded(A_data, B_data)
Tau(A_data, B_data)$Est
Tau(A_data, B_data)$SE^2
Tau_U(A_data, B_data)

beta <- 0.2
delta <- 0.5
m <- 5
n <- 10
iterations <- 100

sample_TauU <- function(beta, delta, m, n, iterations) {
  Tau_Us <- replicate(iterations, {
    yA <- rnorm(m) + beta * (1:m)
    yB <- rnorm(n, mean = delta) + beta * (m + 1:n)
    Tau_U_expanded(yA, yB)
  })
  data.frame(stat = row.names(Tau_Us), M = rowMeans(Tau_Us), V = apply(Tau_Us, 1, var), stringsAsFactors = FALSE)
}

sample_TauU(beta = 0.2, delta = 0.5, m = 10, n = 10, iterations = 2000)

cluster <- create_cluster(parallel::detectCores() - 1)
set_default_cluster(cluster)
cluster %>% 
  cluster_assign_value("Tau_U_expanded", Tau_U_expanded) %>%
  cluster_assign_value("sample_TauU", sample_TauU)

beta <- c(0, 0.05, 0.10)
delta <- seq(0, 2, 0.1)
m <- c(5, 10, 15, 20, 30)
n <- c(5, 10, 15, 20, 30)

params <- expand.grid(beta = beta, delta = delta, m = m, n = n)
nrow(params)

TauU_sim <- 
  params %>%
  group_by(beta, delta, m, n) %>%
  partition(beta, delta, m, n) %>%
  do(sample_TauU(beta = .$beta, delta = .$delta, m = .$m, n = .$n, iterations = 2000)) %>%
  collect()

levels(TauU_sim$stat) <- c("Tau_A", "Tau_U", "Tau_P","V_A","V_P","V_U")

TauU_RB <- 
  TauU_sim %>%
  mutate(mom = ifelse(str_extract(stat, "[:alpha:]+_") == "Tau_", "est","var"),
         stat = str_sub(str_extract(stat, "_[APU]"),2,2),
         val = ifelse(mom == "est", V, M)) %>%
  select(-M, -V) %>%
  spread(mom, val) %>%
  mutate(RB = est / var,
         se = sqrt(var),
         se_null = sqrt((m + n + 1) / (3 * m * n)))

TauU_RB %>%
  filter(stat == "U") %>%
ggplot(aes(delta, se, color = factor(beta))) + 
  geom_smooth(se = FALSE) + 
  geom_hline(aes(yintercept = se_null), linetype = "dashed") + 
  facet_grid(n ~ m, labeller = "label_both") + 
  theme_bw() + theme(legend.position = "bottom") + 
  labs(color = "Slope (per session)", x = "Treatment Effect", y = "SD(Tau-U)")

TauU_RB %>%
  filter(stat == "U") %>%
  ggplot(aes(delta, var, color = factor(beta))) + 
  geom_smooth(se = FALSE) + 
  geom_smooth(aes(x = delta, y = est), se = FALSE, linetype = "dashed") + 
  geom_hline(aes(yintercept = se_null^2), linetype = "dashed") + 
  facet_grid(n ~ m, labeller = "label_both", scales = "free_y") + 
  theme_bw() + theme(legend.position = "bottom") + 
  labs(color = "Slope (per session)", x = "Treatment Effect", y = "Var(Tau-U)")

TauU_RB %>%
  filter(stat == "U") %>%
  ggplot(aes(delta, RB, color = factor(beta))) + 
  geom_smooth(se = FALSE) + 
  facet_grid(n ~ m, labeller = "label_both") + 
  theme_bw() + theme(legend.position = "bottom") + 
  labs(color = "Slope (per session)", x = "Treatment Effect", y = "Relative bias of V_Tau-U")
