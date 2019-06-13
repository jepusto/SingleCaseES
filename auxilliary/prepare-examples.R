library(stringr)
#--------------------
# Schmidt
#--------------------

Schmidt2012 <- read.csv("auxilliary/Schmidt.csv", stringsAsFactors = FALSE)
Schmidt2012 <- Schmidt2012 %>%
  mutate(Session_length = 10,
         Procedure = "other")
str(Schmidt2012)
save(Schmidt2012, file = "data/Schmidt2012.RData", compress = TRUE)

#--------------------
# Thorne
#--------------------

Thorne <- read.csv("auxilliary/Thorne.csv", stringsAsFactors = FALSE)
Thorne <- Thorne %>%
  mutate(Session_length = 15,
         Procedure = ifelse(Measure == "Academic Engagement", "other", "count"))
str(Thorne)
save(Thorne, file = "data/Thorne.RData", compress = TRUE)

#-------------------
# McKissick
#-------------------

McKissick <- read.csv("auxilliary/McKissick.csv", stringsAsFactors = FALSE)
McKissick <- McKissick %>%
  mutate(Session_length = 20,
         Procedure = "count")
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

#--------------------
# Wright & McCathren (2012)
#--------------------

Wright2012 <- read.csv("auxilliary/Wright & McCathren data (wide).csv", stringsAsFactors = FALSE)
Wright2012$Participant <- factor(Wright2012$Participant, levels = c("Nick","Logan","Trevor","Peter"))
str(Wright2012)
save(Wright2012, file = "data/Wright2012.RData", compress = TRUE)

#------------------
# Shogren 2004
#------------------

Session_data <- droplevels(subset(read.csv("auxilliary/Shogren Session data.csv"), Phase %in% c("Choice","No Choice") & 
                                    Measure %in% c("Disruptive behavior","Problem behavior","Engagement","Undesirable behavior") &
                                    (Case != "Danny" | Measure == "Problem behavior") &
                                    Study %in% c("Dunlap","Dyer","Jolivette","Kern","Moes","Powell","Romaniuk","Seybert") | 
                                    (Study == "Frea" & Setting == "Home living"), select = c(1,2,4:9)))

Case_data <- read.csv("auxilliary/Shogren Case data.csv")

Shogren <- left_join(Session_data, Case_data) %>%
  select(-Outcome_measure, -mu) %>%
  mutate(outcome = ifelse(is.na(Percentage),Observed, Percentage / 100),
         outcome = ifelse(Study == "Kern" & Case == "Danny", outcome/4, outcome),
         direction = ifelse(Measure == "Engagement", "increase", "decrease"),
         Possible = ifelse(is.na(Possible), 0, Possible),
         Phase = factor(Phase))

save(Shogren, file = "data/Shogren.RData", compress = TRUE)
