context("Test logic of trunc_constant")

test_that("LRRi, LRRd, SMD, Tau and their standard errors are positive for count data.", {
  
  set.seed(20210323)
  
  A_dat <- rpois(10, 3)
  B_dat <- rep(0, 5)
  C_dat <- rep(10, 5)
  D_dat <- rpois(5, 3)
  E_dat <- rep(10, 10)
  F_dat <- rep(2, 5)
  G_dat <- rep(10, 5)
  
  
  ABd <- calc_ES(A_data = A_dat, B_data = B_dat, 
                 improvement = "decrease", scale = "count", 
                 ES = c("LRRd","LRRi","SMD","Tau"))
  ABd <- rbind(ABd, Tau(A_dat, B_dat, improvement = "decrease", SE = "Hanley"))
  ABi <- calc_ES(A_data = A_dat, B_data = B_dat, 
                 improvement = "increase", scale = "count", 
                 ES = c("LRRd","LRRi","SMD","Tau"))
  ABi <- rbind(ABi, Tau(A_dat, B_dat, improvement = "increase", SE = "Hanley"))
  
  expect_equal(ABd$Est, -ABi$Est)
  expect_equal(ABd$SE, ABi$SE)
  expect_equal(ABd$CI_lower, -ABi$CI_upper)
  expect_equal(ABd$CI_upper, -ABi$CI_lower)
  expect_true(all(ABd$SE > 0))
  
  CDd <- calc_ES(A_data = C_dat, B_data = D_dat, 
                 improvement = "decrease", scale = "count", std_dev = "pool",
                 ES = c("LRRd","LRRi","SMD","Tau"))
  CDd <- rbind(CDd, Tau(C_dat, D_dat, improvement = "decrease", SE = "Hanley"))
  CDi <- calc_ES(A_data = C_dat, B_data = D_dat, 
                 improvement = "increase", scale = "count", std_dev = "pool",
                 ES = c("LRRd","LRRi","SMD","Tau"))
  CDi <- rbind(CDi, Tau(C_dat, D_dat, improvement = "increase", SE = "Hanley"))
  
  
  expect_equal(CDd$Est, -CDi$Est)
  expect_equal(CDd$SE, CDi$SE)
  expect_equal(CDd$CI_lower, -CDi$CI_upper)
  expect_equal(CDd$CI_upper, -CDi$CI_lower)
  expect_true(all(CDd$SE > 0))
  expect_error(SMD(A_data = C_dat, B_data = D_dat, std_dev = "baseline"))
  

  EFd <- calc_ES(A_data = E_dat, B_data = F_dat, 
                 improvement = "decrease", scale = "count", 
                 ES = c("LRRd","LRRi","Tau"))
  EFd <- rbind(EFd, Tau(E_dat, F_dat, improvement = "decrease", SE = "Hanley"))
  EFi <- calc_ES(A_data = E_dat, B_data = F_dat, 
                 improvement = "increase", scale = "count", 
                 ES = c("LRRd","LRRi","Tau"))
  EFi <- rbind(EFi, Tau(E_dat, F_dat, improvement = "increase", SE = "Hanley"))
  
  expect_equal(EFd$Est, -EFi$Est)
  expect_equal(EFd$SE, EFi$SE)
  expect_equal(EFd$CI_lower, -EFi$CI_upper)
  expect_equal(EFd$CI_upper, -EFi$CI_lower)
  expect_true(all(EFd$SE > 0))
  expect_error(SMD(A_data = E_dat, B_data = F_dat, std_dev = "baseline"))
  expect_error(SMD(A_data = E_dat, B_data = F_dat, std_dev = "pool"))
  
  EGd <- calc_ES(A_data = E_dat, B_data = G_dat, 
                 improvement = "decrease", scale = "count", 
                 ES = c("LRRd","LRRi","Tau"))
  EGd <- rbind(EGd, Tau(E_dat, G_dat, improvement = "decrease", SE = "Hanley"))
  EGd <- rbind(EGd, Tau(E_dat, G_dat, improvement = "decrease", SE = "null"))
  EGi <- calc_ES(A_data = E_dat, B_data = G_dat, 
                 improvement = "increase", scale = "count", 
                 ES = c("LRRd","LRRi","Tau"))
  EGi <- rbind(EGi, Tau(E_dat, G_dat, improvement = "increase", SE = "Hanley"))
  EGi <- rbind(EGi, Tau(E_dat, G_dat, improvement = "increase", SE = "null"))
  
  expect_equal(EGd$Est, -EGi$Est)
  expect_equal(EGd$SE, EGi$SE)
  expect_equal(EGd$CI_lower, -EGi$CI_upper)
  expect_equal(EGd$CI_upper, -EGi$CI_lower)
  expect_true(all(EGd$SE > 0))
  expect_error(SMD(A_data = E_dat, B_data = G_dat, std_dev = "baseline"))
  expect_error(SMD(A_data = E_dat, B_data = G_dat, std_dev = "pool"))
  
})
