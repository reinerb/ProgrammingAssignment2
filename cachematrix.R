## Two functions to generate a matrix representation that can cache its
## own inverse, and compute that inverse or retrieve it from the cache.
## Assumes that the given matrix is an invertible square matrix.

## Converts a matrix into a vector with four functions:
## - x$set(y) sets the value of the matrix,
## - x$get()  gets the value of the matrix,
## - x$setinv(inverse) sets the value of the inverse of x,
## - x$getinv() retrieves the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    get <- function() x
    setinv <- function(inverse) inv <<- inverse
    getinv <- function() inv
    
    list(set = set,
         get = get,
         setinv = setinv,
         getinv = getinv)
}

## Retrieves the cached inverse of a matrix if possible.
## Computes its inverse if not.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    
    # Return cached data if available
    if(!is.null(inv)){
        message("Retrieving from cache")
        return(inv)
    }
    
    # If not available, compute the inverse, then cache it
    m <- x$get()
    inv <- solve(m, ...)
    x$setinv(inv)
        
    inv
}
