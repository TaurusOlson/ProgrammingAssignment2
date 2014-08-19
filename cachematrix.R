## cachematrix.R is a set of functions to cache the inverse of a matrix:
## 
## Usage:
## m.cached <- makeCacheMatrix(m)
## m.inversed <- cacheSolve(m.cached)
##
## where 'm' is an invertible matrix.


## makeCacheMatrix creates a special matrix that can be handled by cacheSolve
## It returns a list of 4 functions to 
## - set and get a new matrix
## - set and get the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
        x <<- y
        m <<- NULL
    }
    get <- function() x
    setinv <- function(inverse) m <<- inverse
    getinv <- function() m
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## cacheSolve returns the inverse of a matrix created by makeCachedMatrix.
## If the inverse is not stored in the cache, it calculates it and returns it.
## If the inverse is already stored in the cache it returns this value
## skipping the computation.  
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    m <- x$getinv()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinv(m)
    m
}