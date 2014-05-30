source("utils.R")

rankall <- function(outcome, num = "best") {
    ## Returns a data frame containing of hospital names for each state
    ## based on an index into a sorted data set of 30-day mortality rates
    ## for a given outcome.

    ## "outcome" is one of "heart attack", "heart failure" or "pneumonia"
    ## "num" should be either "best", "worst" or a single numeric index value
    
    ## Validate num
    if (is.character(num)) {
        if (num == "best") {
            ## "best" corresponds to the first element of the sorted data set
            num <- 1
        } else if (num == "worst") {
            ## "worst" corresponds to the last element of the sorted data set
            ## this value is not known as yet so treat it as a special case
            ## to be resolved later
            num <- -1
        } 
    }
    if ((!is.numeric(num)) || length(num) != 1 ) {
        stop("invalid num")
    }
    
    ## Get data set column names for each of the outcomes and validate 
    ## "outcome" input against allowed values
    outcomes <- getoutcomes() 
    if (!(outcome %in% names(outcomes))) {
        stop("invalid outcome")
    }

    ## Read outcome data
    data <- read.csv("outcome-of-care-measures.csv", colClasses = "character")
    
    ## Ensure that the "state" column is stored as a factor to simplify
    ## further operations on the data set
    data[,"State"] <- as.factor(data[,"State"])
    
    ## Split the data frame by state names to allow iteration over data
    ## from each state
    data <- split(data, data[,"State"])
    
    ## Lists to store result values
    hospital <- list()
    state <- list()

    for(s in names(data)) {
        ## Get date for particular state
        statedata <- data[[s]]
        
        ## Convert desired outcome column to be a numeric value for comparisons
        ## Note: Warning due to NA values is intentionally being suppressed here
        suppressWarnings(statedata[,outcomes[[outcome]]] <- as.numeric(statedata[,outcomes[[outcome]]]))
        
        ## Filter out incomplete data
        statedata <- statedata[complete.cases(statedata[,outcomes[[outcome]]]),]
        
        ## Sort the data frame in increasing order of given outcome data
        ## and on alphabetical order of hospital name incase of ties
        statedata <- statedata[ order(statedata[,outcomes[[outcome]]], statedata[,"Hospital.Name"]),]
        
        ## Handle special case conversion for "worst" index
        ## Notes: Using a temporary variable to preserve the original 'num' 
        ## since 'worst' index can be different for each state
        if (num == -1) {
            num_temp <- nrow(statedata)
        } else {
            num_temp <- num
        }
        
        # Store results in a list indexed by state names
        hospital[s] <- statedata[num_temp, "Hospital.Name"]
        state[s] <- s
    }
    
    # Return "hospital" and "state" list as a data frame
    as.data.frame(cbind(hospital, state))
}