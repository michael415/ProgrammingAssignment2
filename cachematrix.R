## Functions for creating a cache matrix, calculating its inverse and saving
## the result to prevent unnecessary recalculation if the matrix doesn't change

## The makeCacheMatrix function is either passed an existing matrix or creates
## an empty one. It provides access (set/get) functions for the matrix itself
## as well as its inverse. 

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) m <<- inverse
    getInverse <- function() m
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


## The cacheSolve function takes a (cache) matrix as input and checks if its
## inverse has already been calculated. In that case, it returns the previously
## calculated result and returns it. Otherwise the result is first computed and
## saved in the cache matrix and then returned.

cacheSolve <- function(x, ...) {
    m <- x$getInverse()
    # If the inverse is cached, return it, otherwise
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    # calculate inverse, set it and return it
    data <- x$get()
    m <- solve(data, ...)
    x$setInverse(m)
    m
}
