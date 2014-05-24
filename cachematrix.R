# These two functions are for creating and 
# operating on special matrices that cache the results
# of the solve operation so extra computation
# can be avoided. 

# makeCacheMatrix creates an object
# that encapsulates a matrix and its inverse. 
# Provides getters and setters for the matrix  
# and inverse. The argument is the optional 
# initial matrix. The function clears 
# the inverse variable when a new matrix
# is set.
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y){
    x <<- y
    inverse <<- NULL
  }
  get <- function(){ x }
  setInverse <- function(updatedInverse){
    inverse <<- updatedInverse
  }
  getInverse <- function(){ inverse }
  list(set = set, get = get, 
       setInverse = setInverse,
       getInverse = getInverse)

}

# cacheSolve computes the inverse of a cacheMatrix object.
# It checks the object to see if the the inverse has already
# been computed before performing the potentially expensive
# solve operation. 
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if (!is.null(inverse)){
    return(inverse)
  }
  updatedInverse <- solve(x$get(), ...)
  x$setInverse(updatedInverse)
  updatedInverse
}
