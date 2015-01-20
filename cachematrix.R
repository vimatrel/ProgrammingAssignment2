## makeCacheMatrix:This function creates a special "matrix" object that can
## cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.


## makeCacheMatrix provides a set of functions that assist in the management
## of the matrix and the cache as described next.
## set: assigns a new matrix to x
## get: provides the current matrix stored in x
## setinv: assigns the computed inverse matrix to inv to be used as cache
## getinv: provides the current value of inv. Will be NULL if inverse for current 
## matrix has not been computed before

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x
        setinv <- function(inverse) inv <<- inverse
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)               
}


## cacheSolve: Using the functions provided by makeCacheMatrix
## validates if inverse has been computed before by calling getinv() and
## checking it´s current value. If current matrix inverse has already been 
## computed, returns cached value with proper message. 
## if not previously computed, uses get() to retrieve current matrix and
## computes the inverse. 
## uses setinv() to store computation in cache
## prints the computation.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
