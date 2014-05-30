## ----------------------------------------------------------------------------
## Assignment 2 for "R Programming" on Coursera
## Submitted by github.com:shirishb
## ----------------------------------------------------------------------------

## This assignment demonstrates R's lexical scoping rules when trying to
## design a function that can cache potentially time-consuming computations
## like the inverse of large matrices.

## The sample code provided for cached vector mean is used as a basis for
## this implementation. Primary additions are code comments and some basic
## unit tests to prove the code is functional.

## ----------------------------------------------------------------------------
## makeCacheMatrix
## ----------------------------------------------------------------------------
## Creates a wrapper around a matrix object to allow caching of its inverse.
## The matrix and inverse values can be accessed through the getter and setter
## functions which are returned in a list object.
##
## Warning:
##   No additional checks are performed to ensure that the inverse being set is 
##   actually correct. Trust the user and don't overthink the assignment... :)
## ----------------------------------------------------------------------------
makeCacheMatrix <- function(x = matrix()) {
    ## Takes a matrix 'x' as input and returns a list object with the following
    ## functions:
    ##    'set'         updates the matrix and clears any cached inverse value
    ##    'get'         returns the last set matrix object
    ##    'setinverse'  updates the matrix inverse object without any checks
    ##    'getinverse'  returns the last set matrix inverse object

    i <- NULL
    
    get <- function() x
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    getinverse <- function() i
    setinverse <- function(inverse) i <<- inverse

    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## ----------------------------------------------------------------------------
## cacheSolve
## ----------------------------------------------------------------------------
## Wrapper method around 'solve' for cached matrix objects created using
## 'makeCacheMatrix' which caches the result of the matrix inverse and uses
## it if available.
##
## Warning:
##   As per stated requirements for the assignment it is assumed that the
##   inverse of the matrix can be calculated and no additional checks are
##   performed.
## ----------------------------------------------------------------------------
cacheSolve <- function(x, ...) {
    ## Takes a cached matrix object 'x' created using 'makeCachedMatrix' as input
    ## and returns a matrix that is the inverse of it
    inverse <- x$getinverse()
    if (!is.null(inverse)) {
        message("getting cached data")
        return(inverse)
    }
    data <- x$get()
    inverse <- solve(data)
    x$setinverse(inverse)
    inverse
}

## ----------------------------------------------------------------------------
## testCacheMatrix
## ----------------------------------------------------------------------------
## _Very_ simple test cases. Only success cases are being tested.
## TODO: Try moving  this to 'RUnit'
## ----------------------------------------------------------------------------
testCacheMatrix <- function() {
    m1 <- matrix(1:4, 2, 2)
    m1_inverse <- solve(m1)

    m2 <- matrix(5:8, 2, 2)
    m2_inverse <- solve(m2)
    
    # Test 1: get after init
    M <- makeCacheMatrix(m1)
    if (identical(m1, M$get())) {
        message("pass: get after init works")
    } else {
        warning("fail: get after init does not work")
    }
    
    # Test 2: get after set
    M <- makeCacheMatrix(m1)
    M$set(m2)
    if (identical(m2, M$get())) {
        message("pass: get after set works")
    } else {
        warning("fail: get after set does not work")
    }

    # Test 3: getinverse after set
    M <- makeCacheMatrix(m1)
    M$set(m2)
    if (identical(NULL, M$getinverse())) {
        message("pass: set clears inverse")   
    } else {
        warning("fail: set does not clear inverse")
    }
    
    # Test 4: getinverse after init
    M <- makeCacheMatrix(m1)
    M$getinverse()
    if (identical(NULL, M$getinverse())) {
        message("pass: inverse clear on init")   
    } else {
        warning("fail: inverse not clear on init")
    }
    
    # Test 5: getinverse after setinverse
    # intentionally used m2_inverse for m1 matrix since it is not checked!
    M <- makeCacheMatrix(m1)
    M$setinverse(m2_inverse)
    if (identical(m2_inverse, M$getinverse())) {
        message("pass: getinverse after setinverse")   
    } else {
        warning("fail: inverse not clear on init")
    }
    
    # Test 5: cacheSolve with empty cache
    M <- makeCacheMatrix(m1)
    if (identical(m1_inverse, cacheSolve(M))) {
        message("pass: cacheSolve with empty cache")   
    } else {
        warning("fail: cacheSolve with empty cache")
    }
    
    # Test 6: cacheSolve with cached inverse
    M <- makeCacheMatrix(m1)
    M$setinverse(m1_inverse)
    if (identical(m1_inverse, cacheSolve(M))) {
        message("pass: cacheSolve with cached inverse")   
    } else {
        warning("fail: cacheSolve with cached inverse")
    }
}
