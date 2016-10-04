## As per the assignment description, the function is written to store the inverse of different matrix in order to avoid
## recalculation when necessary to re use the value.

## The first function sets a list, by scoping a list of functions to the assigned value (Matrix to be "inversed")

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setinv <- function(solve) i <<- solve
    getinv <- function() i
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This functions checks that the specified Matrix hasn't been inversed before, if so, looks up the cached value,
## otherwise calculated the inverse of the matrix

cacheSolve <- function(x, ...) {
    i <- x$getinv()
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    data <- x$get()
    i <- solve(data, ...)
    x$setinv(i)
    i
}
