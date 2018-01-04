# Pre-loaded data info
# path describes where to find the data
# varnames are the Treatment variable name, Outcome variable name, Session numbering, and then any numer of clustering variable names
# family is the family for the GLM
# Link is the link functions

example_list <- c("Thorne and Kamps (2008)" = "Thorne", "Schmidt and Stitcher (2012)" = "Schmidt2012")

exampleMapping <- list(
  Thorne = list(path = "data/Thorne.csv",
                     varnames = c("Trt", "Outcome", "Session_number", "Measure", "Case"),
                     m = 10,
                     transform = FALSE,
                     family = "quasipoisson",
                     link = "log"),
  Schmidt2012 = list(path = "data/Schmidt.csv",
                 varnames = c("Trt", "Outcome", "Session_num", "Behavior", "Case"),
                 m = 4,
                 transform = TRUE,
                 family = "quasibinomial",
                 link = "logit")
)
