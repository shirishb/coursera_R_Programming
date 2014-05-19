source("utils.R")

complete <- function(directory, id = 1:332) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'id' is an integer vector indicating the monitor ID numbers
    ## to be used
    
    ## Return a data frame of the form:
    ## id nobs
    ## 1  117
    ## 2  1041
    ## ...
    ## where 'id' is the monitor ID number and 'nobs' is the
    ## number of complete cases
    
    nobs <- vector()
    for (i in seq_along(id)) {
        filename <- paste(directory, "/", getpadstring(id[i]) , id[i], ".csv", sep="")
        data <- read.csv(filename)
        nobs[i] <- nrow(data[complete.cases(data),])
    }
    data.frame(id, nobs)
}