## Demonstration of global variable assignment usage ( <<- )
## as well as its application for caching the data

## create function that returns a list of function involving
## finding inverse matrix of x as well as data caching mechanism

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setsolve <- function(inverse) inv <<- inverse
    getsolve <- function() inv
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## get an inverse matrix of x by looking in the cache first
## print "getting cached data" and return data in cache if 'cache hit'
## if not in cache, call solve() and store result in the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
    inv <- x$getsolve()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    inv <- solve(data, ...)
    x$setsolve(inv)
    inv
}
