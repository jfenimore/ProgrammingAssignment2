## makeCacheMatrix and cacheSolve will check to see if a passed matrix has been updated, and 
## persist unchanged passed maxtrices to save on cycle time to solve the inverse of a matrix values
## It will calculate the inverse value of a matrix should the state of cached matrix change.

## makeCacheMatrix checks for state changes in a precached matrix, sets the cache, and updates it if necessary

makeCacheMatrix <- function(x = matrix()) {

  inv <- NULL  # Initialize default value of matrix inverse to NULL
  
  # Define utility functions to get and set matrix (x) 
  setMatrix <- function(y) {
    x <<- y  #Cache passed parameter
    inv <<- NULL  #Clear matrix inverse value when the state of the matrix has changed
  }  #End of set cache block
  
  # Collect cached value of inverse
  getMatrix <- function() {
    x
  } # End of get cache block
  
  # Define functions to get and set inverse of matrix
  setinverse <- function(inverse) {
    message("Setting matrix")
    inv <<- inverse
  } # End of set inverse block
  
  getinverse <- function() {
    message("Getting inverse of matrix")
    inv
  } # End of get inverse block
  
  # Passes results
  list(setMatrix=setMatrix, getMatrix=getMatrix,
       setinverse=setinverse
       ,getinverse=getinverse)
  
} #End of makeChacheMatrix block


## cacheSolve : Calculates the inverse of matrix

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  # Checking to see if the result has already been cached
  if(!is.null(inv)) {
    message("Result already processed. Getting cached data.")
    return(inv)
  }
  
  #Calculate inverse if it is not there
  data = x$getMatrix()
  inv <- solve(data,...)
  x$setinverse(inv)
  inv
} #End of CacheSolve block
