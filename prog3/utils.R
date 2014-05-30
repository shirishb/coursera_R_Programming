getoutcomes <- function() {
    ## Returns a list of data set column name by 'outcomes'
    outcomes <- list()
    outcomes["heart attack"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    outcomes["heart failure"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    outcomes["pneumonia"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"
    outcomes
}

getsorteddata <- function(state, outcome) {
    ## Function returns a data frame containing the "Outcome of Care Measures"
    ## csv data that is filtered by state and sorted by outcome
    ## Note: This function also validates "state" and "outcome" inputs and 
    ## stops processing on error
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Ensure that the "state" column is stored as a factor to simplify
    ## further operations on the data set
    data[,"State"] <- as.factor(data[,"State"])
    
    ## Check that state and outcome are valid
    if (!(state %in% levels(data[,"State"]))) {
        stop("invalid state")
    }
    
    data <- split(data, data[,"State"])[[state]]
    
    ## Get data set column names for each of the outcomes and validate 
    ## "outcome" input against allowed values
    outcomes <- getoutcomes()
    if (!(outcome %in% names(outcomes))) {
        stop("invalid outcome")
    }
    
    ## Convert desired outcome column to be a numeric value for comparisons
    ## Note: Warning due to NA values is intentionally being suppressed here
    suppressWarnings(data[,outcomes[[outcome]]] <- as.numeric(data[,outcomes[[outcome]]]))
    
    ## Filter out incomplete data
    data <- data[complete.cases(data[,outcomes[[outcome]]]),]
    
    ## Sort the data frame in increasing order of given outcome data
    ## and on alphabetical order of hospital name incase of ties
    data <- data[ order(data[,outcomes[[outcome]]], data[,"Hospital.Name"]),]

    data
}