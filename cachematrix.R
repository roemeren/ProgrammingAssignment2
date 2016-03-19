# This file contains a pair of functions that cache the inverse of a matrix.

# This function creates a special "matrix" object that can cache 
# its inverse.

makeCacheMatrix <- function(x = matrix()) {
    xInv <- NULL
    
    set <- function(y) {
        x <<- y
        xInv <<- NULL
    }
    
    get <- function() x
    
    setInverse <- function(inv) xInv <<- inv
    
    getInverse <- function() xInv
    
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


# This function computes the inverse of the special "matrix" returned
# by makeCacheMatrix. If the inverse has already been calculated
# (and the matrix has not changed), then the function should retrieve
# the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inv <- x$getInverse()
    
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    mtrx <- x$get()
    inv <- solve(mtrx, ...)
    x$setInverse(inv)
    inv
}
