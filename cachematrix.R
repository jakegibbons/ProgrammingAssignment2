## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix: This function creates a special "matrix" object that can
## cache its inverse.

makeCacheMatrix <- function(inMatrix = matrix()) {

  # initialize variables
  # inverse
  invMatrix <- NULL
  
  # Matrix constructor
  set <- function(newData){
    inMatrix <<- newData
    # inverse is null for newly constructed matrix
    invMatrix <<- NULL
  }
  
  # Return matrix
  get <- function() {
    inMatrix
  }
  
  # Inverse constructor
  setInverse <- function(inverse) {
    invMatrix <<- inverse
  }

  # Return inverse
  getInverse <- function() {
    invMatrix
  }
  
  # List of function methods available
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## cacheSolve: This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve should 
## retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  # Get the inverted matrix 
  invMatrix <- x$getInverse()

  # If the inverse is present then return is
  if(!is.null(invMatrix)) {
    message("getting cached data.")
    return(invMatrix)
  }
  
  # If not cached then get the original matrix and call inverter
  data <- x$get()
  invMatrix <- solve(data) %*% data

  x$setInverse(invMatrix)
  
  # Return the inverted matrix
  invMatrix

}