# Pre-loaded data info
# path describes where to find the data
# varnames are the Phase identifier variable name, Outcome variable name, and then any number of clustering variable names
# phase vals are the values corresponding to the baseline and treatment phase, respectively
# direction describes direction type
# direction_var is the name of the variable that applies the direction.

example_list <- c("McKissick et al. (2010)" = "McKissick", 
                  "Schmidt (2007)" = "Schmidt2007",
                  "Wright & McCathren (2012)" = "Wright2012")

exampleMapping <- list(
  Schmidt2007 = list(condition = "Condition",
                     outcome = "Outcome",
                     cluster_vars = c("Case_pseudonym", "Behavior_type"),
                     aggregate_vars = c("Phase_num"),
                     phase_vals = c("A", "B"),
                     direction = "series",
                     direction_var = "direction",
                     session_num = "Session_number",
                     scale = "series",
                     scale_var = "Metric",
                     intervals = "n_Intervals",
                     observation_length = "Session_length"),
  McKissick = list(condition = "Condition",
                   outcome = "Outcome",
                   cluster_vars = "Case_pseudonym",
                   phase_vals = c("A", "B"),
                   direction = "decrease",
                   session_num = "Session_number",
                   scale = "count",
                   intervals = NA,
                   observation_length = "Session_length"),
  Wright2012 = list(condition = "Condition",
                   outcome = "Prosocial_behavior",
                   cluster_vars = "Participant",
                   phase_vals = c("baseline", "intervention A", "intervention B"),
                   direction = "increase",
                   session_num = "Session",
                   scale = "count",
                   intervals = NA,
                   observation_length = NA)
  
)
