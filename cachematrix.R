## This file implements a "cache matrix" data type
## This data type comprises storage and retrieval functions
## for a matrix and its inverse, and is initialized
## using the function makeCacheMatrix
##  
## The file also contains one function acting on this data type,
## cacheSolve, which solves for the inverse if it is not already
## cached

## makeCacheMatrix takes as input a matrix
## and returns a list comprising four functions:
## set, which sets the value of the matrix
## get, which returns the value of the matrix
## setinv, which sets the value of the inverse
## getinv, which returns the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL    # initialize inverse to NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## cacheSolve takes as input a "cache matrix"
## and returns its inverse matrix.
## If the value of the inverse is cached, it is
## read and returned.
## If the value of the inverse is not cached, it is
## computed, cached, and returned.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getinv()    # retrieve cached inverse
    if(!is.null(inv)) {
        message("getting cached inverse")
        return(inv)    # return cached value, if available
    }
    data <- x$get()    # read in the matrix
    inv <- solve(data, ...)    # compute the inverse
    x$setinv(inv)    # cache the inverse
    inv
}
