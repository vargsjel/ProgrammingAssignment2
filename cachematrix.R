## These functions used to create a special object
## that stores a matrix and caches its inverse. 

## Creates a special list of functions to operate with input matrix (x):
## set and get matrix values, cache it and retrieve cache content.
makeCacheMatrix <- function(x = matrix()) {
    cache.m <- NULL
    set.M <- function(y) {
        x <<- y 
        cache.m <<- NULL
    }
    get.M <- function() x
    set.Inv <- function(M.Inv) cache.m <<- M.Inv
    get.Inv <- function() cache.m
    list(setMatrix=set.M, getMatrix=get.M, setInverse=set.Inv, getInverse=get.Inv)
}


## Checks if the inverse of a given matrix (x) has already been calculated.
## If so, gets the inverse from the cache and skips the computation.
## Otherwise, calculates the inverse of a matrix and puts it to the cache.
cacheSolve <- function(x, ...) {
    cache.m <- x$getInverse()
    if(!is.null(cache.m)) {
        message("getting cached data")
        return(cache.m)
    }
    data <- x$getMatrix()
    cache.m <- solve(data, ...)
    x$setInverse(cache.m)
    cache.m
}