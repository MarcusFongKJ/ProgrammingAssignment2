## A pair of functions that cache the inverse of a matrix.
## Assuming matrix supplied is always invertible.

## makeCacheMatrix is a function that creates a special "matrix" 
## object that can cache its inverse. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cachesolve should retrieve the inverse from the cache.

makeCacheMatrix <- function(x = matrix()) {
        # initialize inv as NULL
        inv <- NULL
        
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        
        setInverse <- function(solve) inv <<- solve
        getInverse <- function() inv
        
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## cacheSolve is a function computes the inverse of the special 
## "matrix" returned by makeCacheMatrix.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getInverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setInverse(inv)
        inv
}
