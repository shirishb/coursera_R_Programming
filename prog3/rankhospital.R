source("utils.R")

rankhospital <- function(state, outcome, num = "best") {
    ## Returns a character vector of hospital names based on indices into a
    ## sorted data set of 30-day mortality rates for a given outcome.
    
    ## "state" is a 2 character abbreviation, e.g. "TX" for "Texas"
    ## "outcome" is one of "heart attack", "heart failure" or "pneumonia"
    ## "num" can be a vector of indices into the outcome data set or the 
    ##    mnemonics "best" or "worst" which are converted to index values
    
    ## Note: Any out of boundary indices result in NA values in the returned
    ## hospital name vector
    
    ## Get filtered and sorted outcome data set
    ## inputs for "state" and "outcome" are validated within "getdata()"
    data <- getsorteddata(state, outcome)

    ## Validate "num" input based on rules stated above
    if (is.character(num)) {
        if (num == "worst") {
            num <- nrow(data)
        } else if (num == "best") {
            num <- 1
        } else {
            stop("invalid num")
        }
    }
    if (!is.numeric(num)) {
        stop("invalid num")
    }
    data[num, "Hospital.Name"]
}