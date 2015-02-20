## Matrix inversion is usually a costly computation, which may benefit from
## caching instead of computing it repeatedly. Below is a pair of functions that
## cache the inverse of a matrix, once computed.


## This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    
    ## Initialize inverse i and set to NULL until the inverse of matrix x has
    ## been computed
    i <- NULL

    ## Set the value of matrix x and initialize inverse i with a NULL value
    setMatrix <- function(y) {
        x <<- y
        i <<- NULL
    }
    
    ## Return the value of matrix x
    getMatrix <- function() {
        x
    }
    
    ## Set the value of inverse i
    setInverse <- function(z) {
        i <<- z
    }
    
    ## Return the value of inverse i
    getInverse <- function() {
        i
    }
    
    ## Return the list of function calls for makeCacheMatrix
    list(setMatrix = setMatrix,
         getMatrix = getMatrix,
         setInverse = setInverse,
         getInverse = getInverse)
    
}


## This function computes the inverse of the special "matrix" returned by
## makeCacheMatrix above. If the inverse has already been computed (and the
## matrix has not changed), then the cacheSolve should retrieve the inverse from
## the cache.
cacheSolve <- function(x, ...) {
    
    ## Get the value of inverse i
    i <- x$getInverse()
    
    ## If inverse i was previously computed, return inverse i
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    ## If inverse i was not previously computed, compute it, set the value of
    ## inverse i, and then return inverse i
    data <- x$getMatrix()
    i <- solve(data, ...)
    x$setInverse(i)
    i
    
}