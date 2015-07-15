## This pair of functions: 
## - creates a special "matrix" object that can cache its inverse, and 
## - computes the inverse of the special "matrix" returned.

## This function creates a series of functions to support the inversion 
## of a matrix, or commit this inversion to the cache. 

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
        get <- function() x
        setinverse <- function(solve) m <<- solve
        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

## Computes the inverse of a matrix returned by the makeCacheMatrix
## function.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinverse(m)
        m
}