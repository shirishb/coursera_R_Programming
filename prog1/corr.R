source("complete.R")
source("utils.R")

corr <- function(directory, threshold = 0) {
    ## 'directory' is a character vector of length 1 indicating
    ## the location of the CSV files
    
    ## 'threshold' is a numeric vector of length 1 indicating the
    ## number of completely observed observations (on all
    ## variables) required to compute the correlation between
    ## nitrate and sulfate; the default is 0
    
    ## Return a numeric vector of correlations
    data <- complete(directory)
    id <- data[data$nobs > threshold,"id"]
    
    nobs <- vector()
    for (i in seq_along(id)) {
        filename <- paste(directory, "/", getpadstring(id[i]) , id[i], ".csv", sep="")
        data <- read.csv(filename)
        nobs[i] <- cor(data$sulfate, data$nitrate, use="complete.obs")
    }
    nobs
}