rankall <- function(outcome, num = "best") {
    ## validate num
    if (is.character(num)) {
        if (num == "worst") {
            num <- -1
        } else if (num == "best") {
            num <- 1
        }
    }
    if ((!is.numeric(num)) || length(num) != 1 ) {
        stop("invalid num")
    }
    
    ## outcome can be: "heart attack", "heart failure", or "pneumonia"
    outcomes <- list()
    outcomes["heart attack"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack"
    outcomes["heart failure"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure"
    outcomes["pneumonia"] <- "Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia"  
    if (!(outcome %in% names(outcomes))) {
        stop("invalid outcome")
    }

    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    data[,"State"] <- as.factor(data[,"State"])
    
    data <- split(data, data[,"State"])
    
    hospital <- list()
    state <- list()
    for(s in names(data)) {
        statedata <- data[[s]]
        
        suppressWarnings(statedata[,outcomes[[outcome]]] <- as.numeric(statedata[,outcomes[[outcome]]]))
        statedata <- statedata[ order(statedata[,outcomes[[outcome]]], statedata[,"Hospital.Name"]),]
        statedata <- statedata[complete.cases(statedata[,outcomes[[outcome]]]),]
    
        if (num == -1) {
            num_temp <- nrow(statedata)
        } else {
            num_temp <- num
        }
        hospital[s] <- statedata[num_temp, "Hospital.Name"]
        state[s] <- s
    }
    
    as.data.frame(cbind(hospital, state))
}