source("utils.R")
best <- function(state, outcome) {
    data <- filterdata(state, outcome)
    data[1, "Hospital.Name"]
}