## The functions are used for creating a matrix object which stores a) A matrix and b) Its inverse.
# We do not compute inverse on creation of the matrix object, but by calling cacheSolve which 
# computes the inverse and stores it in the matrix object so that it can be retrieved next time 
# cacheSolve is called.

## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
#  1. set the value of the matrix
#  2.  get the value of the matrix
#  3.  set the value of the inverse
#  4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inv_x <- NULL
        set <- function(y) {
                x <<- y
                inv_x <<- NULL
        }
        get <- function() x
        set_inv <- function(solve) inv_x <<- solve
        get_inv <- function() inv_x
        list(set = set, get = get,
             set_inv = set_inv,
             get_inv = get_inv)
}


## cacheSolve returns the inverse of matrix object x which has earlier been created using makeCacheMatrix.
# It checks whether the inverse has already been computed in which case, it just returns the cached inverse
# Else we compute the inverse of the matrix explicitly, set its inverse in the matrix object x and return it.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv_x <- x$get_inv()        
        if(!is.null(inv_x)) {
                message("getting cached data")
                return(inv_x)
        }
        data <- x$get()
        inv_x <- solve(data, ...)
        x$set_inv(inv_x)
        inv_x
}
