source("utils.R")

rankhospital <- function(state, outcome, num = "best") {
    data <- filterdata(state, outcome)
    if (is.character(num)) {
        if (num == "worst") {
            num <- nrow(data)
        } else if (num == "best") {
            num <- 1
        } else {
            stop("invalid num")
        }
    }
    data[num, "Hospital.Name"]
}