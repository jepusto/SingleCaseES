library(dplyr)
data("iris")

group_data <- function(df, group_var) {
  group_class <- tryCatch(class(group_var), error = function(e) "variable")
  print(group_class)
  if (group_class == "variable") group_var <- enquo(group_var)
  vars_select(names(df), !!!group_var)
}

group_data(iris, Species)
group_data(iris, "Species")
group_data(iris, Petal.Width)
group_data(iris, vars(Petal.Width))
group_data(iris, vars(Species, Petal.Width))
group_data(iris, c(Species, Petal.Width))
group_data(iris, c("Species", "Petal.Width"))
