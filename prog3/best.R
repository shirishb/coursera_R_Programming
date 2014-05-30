source("utils.R")

best <- function(state, outcome) {
    ## Returns name of hospital with best outcome based on 30-day mortality rates
    ## "state" is a 2 character abbreviation, e.g. "TX" for "Texas"
    ## "outcome" is one of "heart attack", "heart failure" or "pneumonia"
    
    ## Get filtered and sorted outcome data set
    ## inputs for "state" and "outcome" are validated within "getdata()"
    data <- getsorteddata(state, outcome)
    data[1, "Hospital.Name"]
}