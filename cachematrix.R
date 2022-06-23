# The purpose of makeCacheMatrix and cacheSolve is to cache the
# inverse of a matrix, avoiding recalculate the inverse if this has
# been calculated before.

# Creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        get <- function() x

        set.inverse <- function(inverse) inv <<- inverse

        get.inverse <- function() inv

        list(set = set, get = get,
             set.inverse = set.inverse,
             get.inverse = get.inverse)
}


# If the inverse has been calculated cacheSolve get it from the
# cache, if not calcualtes the inverse of the given matrix.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$get.inverse()

        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }

        inv <- solve(x$get())
        x$set.inverse(inv)
        inv
}
