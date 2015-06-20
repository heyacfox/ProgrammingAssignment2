# This pair of functions will store a matrix
# as an object along with its solved inverse.

# This function creates a matrix object that can
# be used to calculate the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    # This function is used to reset the newly created
    # matrix object's value after it has been created
    setobjvalue <- function(y) {
        x <<- y
        inv <- NULL
    }
    # Returns the value of the object
    get <- function() x
    # This function assumes we've already solved
    # for the inverse and now we are storing it
    setinv <- function(inverse) inv <<- inverse
    # Will return NULL until we actually solve this
    # object
    getinv <- function() inv
    # "Matrix" "object" that is really a list
    list(get = get, 
         set = setobjvalue, 
         getinv = getinv,
         setinv = setinv)
}


# This object solves the inverse of a matrix
# as contained in the matrix object returned
# by makeCacheMatrix, either through solving
# the matrix or returning a cached solution
# for the "object" "matrix". Note: the dot dot dot
# is for the

cacheSolve <- function(x, ...) {
    cachedinverse <- x$getinv()
    # If the stored inverse is not NULL, then we return
    # the cached value
    if(!is.null(cachedinverse)) {
        message("Cached Matrix Inverse:")
        return(cachedinverse)
    }
    # If the stored inverse is NULL, then we solve
    # for the matrix...
    cachedinverse <- solve(x$get(), ...)
    # ...use the object's setinv to set its cached value...
    x$setinv(cachedinverse)
    # ...and return the calculated inverse matrix
    cachedinverse
}
