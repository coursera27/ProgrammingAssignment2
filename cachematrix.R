## These functions allow you to create a matrix whose inverse can be cached.
## It is assumed that only square matrices will be defined/used.

## The matrix is created with the makeCacheMatrix function.
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
## The inverse is not computed until the cacheSolve function is called.
##That function returns the inverse.  From then on, you can obtain it from 
## getinverse or by cacheSolve.
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

