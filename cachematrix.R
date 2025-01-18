## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  # Initialize a variable to store the cached inverse
  inverse <- NULL
  
  # Function to set the matrix
  set <- function(y) {
    x <<- y
    inverse <<- NULL  # Reset the inverse cache when the matrix is updated
  }
  
  # Function to get the matrix
  get <- function() x
  
  # Function to set the cached inverse
  setInverse <- function(inv) inverse <<- inv
  
  # Function to get the cached inverse
  getInverse <- function() inverse
  
  # Return a list of the above functions
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)

}

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  # Check if the inverse is already cached
  inverse <- x$getInverse()
  
  # If cached inverse exists, return it
  if (!is.null(inverse)) {
    message("Getting cached data")
    return(inverse)
  }
  
  # If not cached, compute the inverse
  matrix <- x$get()
  inverse <- solve(matrix, ...)  # Compute the inverse
  x$setInverse(inverse)          # Cache the inverse for future use
  inverse                        # Return the computed inverse
}

