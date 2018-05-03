## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix creates a set of functions that will be used to create & test for cached values

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL                               ## assigns m in the Global Environment to NULL
        set <- function(y) {                    ## creates a "set" closure that sets the global value of x equal to y
                x <<- y
                m <<- NULL
                }
        get <- function() x                     ## creates a "get" closure that returns the local value of x
        setinverse <- function(inverse) m <<- inverse    ## creates a "setinverse" closure that assigns m in the Global Environment to the inverse
        getinverse <- function() m                       ## creates a "getinverse" closure that returns the local value of m
        list(set = set, get = get,                       ## returns a list of 4 functions, set, get, setinverse, getinverse
                setinverse = setinverse,
                getinverse = getinverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
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
