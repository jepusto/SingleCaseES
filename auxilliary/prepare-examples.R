#--------------------
# Schmidt
#--------------------

Schmidt <- read.csv("auxilliary/Schmidt.csv", stringsAsFactors = FALSE)
str(Schmidt)
save(Schmidt, file = "data/Schmidt.RData", compress = TRUE)

#--------------------
# Thorne
#--------------------

Thorne <- read.csv("auxilliary/Thorne.csv", stringsAsFactors = FALSE)
str(Thorne)
save(Thorne, file = "data/Thorne.RData", compress = TRUE)