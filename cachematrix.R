## makeCacheMatrix:This function creates a special "matrix" object that can
## cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above.If the inverse has already been calculated
## (and the matrix has not changed), then the cachesolve should retrieve the
## inverse from the cache.


## makeCacheMatrix provides a set of functions that assist in the management
## of the matrix and the cache.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        ## set: assigns a new matrix to x
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        ## get: provides the current matrix stored in x
        get <- function() x
        ## setinv: assigns the computed inverse matrix to inv to be used as cache
        setinv <- function(inverse) inv <<- inverse
        ## getinv: provides the current value of inv. Will be NULL if inverse for current 
        ## matrix has not been computed before
        getinv <- function() inv
        list(set = set, get = get, 
             setinv = setinv, getinv = getinv)               
}


## cacheSolve: Uses the functions provided by makeCacheMatrix and
## computes the inverse of the special "matrix".
cacheSolve <- function(x, ...) {
        ## validate if inverse has been computed before by calling getinv() and
        ## check it´s current value. If current matrix inverse has already been 
        ## computed, return cached value with proper message.
        inv <- x$getinv()
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        ## if not previously computed, use get() to retrieve current matrix and
        ## compute the inverse. 
        data <- x$get()
        inv <- solve(data, ...)
        ## use setinv() to store computation in cache
        x$setinv(inv)
        ## display the computation.
        inv
}
