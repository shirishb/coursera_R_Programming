best <- function(state, outcome) {
    
    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data[,"State"] <- as.factor(data[,"State"])
    
    ## Check that state and outcome are valid
    if (!(state %in% levels(data[,"State"]))) {
        stop("invalid state")
    }
    
    data <- split(data, data[,"State"])[[state]]
    
    ## outcome can be: "heart attack", "heart failure", or "pneumonia"
    outcomes <- list()
    outcomes["heart attack"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    outcomes["heart failure"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    outcomes["pneumonia"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"  
    if (!(outcome %in% names(outcomes))) {
        stop("invalid outcome")
    }
    
    ## Return hospital name in that state with lowest 30-day death
    ## rate
    suppressWarnings(data[,outcomes[[outcome]]] <- as.numeric(data[,outcomes[[outcome]]]))
    data <- data[ order(data[,outcomes[[outcome]]], data[,"Hospital.Name"]),]
    data[1, "Hospital.Name"]
}