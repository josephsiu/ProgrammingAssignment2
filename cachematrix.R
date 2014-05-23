###############################################################################
## Joseph Siu
## Last Update: 2014-05-23
## Created for Coursera's R Programming Course
###############################################################################
## The following two functions creates a special matrix which can be initiated
## in R for any matrix and it will calculate the inverse; once the inverse is
## calculated, the functions will cache the results and will not re-calculate
## but instead retrieve the stored result.
###############################################################################

## This function creates the special matrix with the capability to store
## the original matrix as well as the inverse matrix; note that the function 
## does not calculate the inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {
          x <<- y
          inv <<- NULL
     }
     get <- function() x
     setinv <- function(z) inv <<- z
     getinv <- function() inv
     list(set = set, get = get, setinv = setinv, getinv = getinv)
}

## Used in combination with the function above, this function first
## retrieves the stored inverse if possible; if null, it then calculates
## the inverse and store it accordingly.

cacheSolve <- function(x, ...) {
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("getting cached data")
          return(inv)
     }
     data <- x$get()
     inv <- solve(data, ...)
     x$setinv(inv)
     inv
}