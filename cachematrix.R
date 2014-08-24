## The two functions below, makeCacheMatrix and cacheSolve, calculate the inverse
## of a matrix ("x") and store the inverse matrix ("inv") for future calculations.

## makeCacheMatrix creates a list of functions related to the invertible matrix "x".
## These functions are set(), get(), setinverse() and getinverse(),
## where set() defines and can modify the matrix "x", get() calls the matrix "x", 
## setinverse() is used by the cacheSolve function to define the inverse of 
## matrix "x" ("inv"), and getinverse() calls the stored value for "inv" if one exists.

makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(y) {
            x <<- y
            inv <<- NULL
      }
      get <- function() x
      setinverse <- function(inverse) inv <<- inverse
      getinverse <- function() inv
      list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## cacheSolve returns and stores a matrix that is the inverse of "x" ("inv").

cacheSolve <- function(x, ...) {
      inv <- x$getinverse()
      if(!is.null(inv)) {
            message("getting cached data")
            return(inv)
      }
      data <- x$get()
      inv <- solve(data, ...)
      x$setinverse(inv)
      inv
}
