library(stringr)
#--------------------
# Schmidt
#--------------------

Schmidt2012 <- read.csv("auxilliary/Schmidt.csv", stringsAsFactors = FALSE)
str(Schmidt2012)
save(Schmidt2012, file = "data/Schmidt2012.RData", compress = TRUE)

#--------------------
# Thorne
#--------------------

Thorne <- read.csv("auxilliary/Thorne.csv", stringsAsFactors = FALSE)
str(Thorne)
save(Thorne, file = "data/Thorne.RData", compress = TRUE)

#-------------------
# McKissick
#-------------------

McKissick <- read.csv("auxilliary/McKissick.csv", stringsAsFactors = FALSE)[-1]
str(McKissick)
save(McKissick, file = "data/McKissick.RData", compress = TRUE)

#-------------------
#Schmidt 2007
#-------------------

Schmidt2007 <- read.csv("auxilliary/Schmidt2007.csv", stringsAsFactors = FALSE)

Schmidt2007$direction <- ifelse(Schmidt2007$Outcome_descriptor == "Disruptive Behavior", "decrease", "increase")
Schmidt2007$Interval_length <- as.integer(ifelse(Schmidt2007$Interval_length == "N/A", NA, Schmidt2007$Interval_length))
Schmidt2007$n_Intervals <- with(Schmidt2007, 60 * Session_length / Interval_length)
Schmidt2007$Metric <- ifelse(Schmidt2007$Metric == "Natural Count", "count", "percentage")
names(Schmidt2007)[1] <- "Behavior_type"

save(Schmidt2007, file = "data/Schmidt2007.RData", compress = TRUE)
