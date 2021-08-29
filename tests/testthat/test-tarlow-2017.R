context("Tarlow (2017).")

A <- c(1, 2, 3, 4, 5)
B <- c(6, 7, 8, 9, 10)

NOMs <- calc_ES(A, B, improvement = "increase", 
                ES = c("Tau", "Tau_U", "Tau_BC"),
                SE = "none", confidence = NULL)

test_that("Tau is correct.", {
  tau <- Tau(A, B, improvement = "increase", SE = "none", confidence = NULL)
  expect_equal(1, tau$Est)
  expect_equal(tau, subset(NOMs, ES == "Tau"), check.attributes = FALSE)
})

test_that("Tau-U is correct.", {
  tauU <- Tau_U(A, B, improvement = "increase")
  expect_equal(0.6, tauU$Est)
  expect_equal(tauU, subset(NOMs, ES == "Tau-U"), check.attributes = FALSE)
})

test_that("Tau-BC is correct.", {
  tauBC <- Tau_BC(A, B, improvement = "increase", SE = "none", confidence = NULL)
  # calculate Tau-BC on http://ktarlow.com/stats/tau
  expect_equal(0, tauBC$Est)
  expect_equal(tauBC, subset(NOMs, ES == "Tau-BC"), check.attributes = FALSE)
})
