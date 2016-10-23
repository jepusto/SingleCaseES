library(tidyr)
library(dplyr, warn.conflicts = FALSE)
library(multidplyr)
library(ggplot2)
library(SingleCaseES)

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
Tau_U(yA, yB)
Tau_U(yA, yB) * (m * n) / (m * (2 * n + m - 1) / 2)
m * n
m * (m - 1) / 2

sample_TauU <- function(beta, delta, m, n, iterations) {
  Tau_Us <- replicate(iterations, {
    yA <- rnorm(m) + beta * (1:m)
    yB <- rnorm(n, mean = delta) + beta * (m + 1:n)
    Tau_U(yA, yB)
  })
  data.frame(sd = sd(Tau_Us))
}


cluster <- create_cluster(parallel::detectCores() - 1)
set_default_cluster(cluster)
cluster %>% 
  cluster_library("SingleCaseES") %>%
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
  collect() %>%
  mutate(se_null = sqrt((m + n + 1) / (3 * m * n))) %>% 
  arrange(beta, delta, m , n)

ggplot(TauU_sim, aes(delta, sd, color = factor(beta))) + 
  geom_smooth(se = FALSE) + 
  geom_hline(aes(yintercept = se_null), linetype = "dashed") + 
  facet_grid(n ~ m, labeller = "label_both") + 
  theme_bw() + theme(legend.position = "bottom") + 
  labs(color = "Slope (per session)", x = "Treatment Effect", y = "SD(Tau-U)")
