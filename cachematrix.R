## Programming Assignment 2:  A pair of functions makeCacheMatrix and cacheSolve
## that cache the inverse of a matrix.

## makeCacheMatrix: This function creates a special "matrix" 
## object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
     inv <- NULL
     set <- function(y) {                            # Set the value of the matrix
          x <<- y
          inv <<- NULL
     }
     get <- function() x                             # Get the value of the matrix
     setinv <- function(inverse) inv <<- inverse     # Set the value of the inverse matrix
     getinv <- function() inv                        # Get the value of the inverse matrix
     list(set = set, get = get,                      # Creates a list of functions
          setinv = setinv,
          getinv = getinv)
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the inverse 
## from the cache.

## Note: This functions assumes x is invertible

cacheSolve <- function(x, ...) {
     ## Return a matrix that is the inverse of 'x'
     inv <- x$getinv()
     if(!is.null(inv)) {
          message("Getting cached matrix data...")
          return(inv)
     }
     matrix_data <- x$get()
     inv <- solve(matrix_data)                        # Computes the inverst of the matrix
     x$setinv(inv)
     inv
     }
