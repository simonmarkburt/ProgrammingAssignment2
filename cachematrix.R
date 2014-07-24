# These functions are designed to cache the inverse of a matrix


# makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
# by creating a list containing a function to
# 1. set the value of the matrix
# 2. get the value of the matrix
# 3. set the value of inverse of the matrix
# 4. get the value of inverse of the matrix

makeCacheMatrix <- function( mtx = matrix() ) {
  
  # Initialize the inverse property
  inv <- NULL
  
  # Method to setup matrix
  set <- function( matrix ) {
         mtx <<- matrix
         inv <<- NULL
  }
  
  # Method the get the matrix
  get <- function() {
         mtx
  }
  
  # Method to set the inverse of the matrix
  setInverse <- function(inverse) {
                inv <<- inverse
  }
  
  # Method to get the inverse of the matrix
  getInverse <- function() {
                inv
  }
  
  ## Return a list of the methods
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}


# Compute the inverse of the special matrix returned by the function "makeCacheMatrix"
# First check if the inverse has already been computed. 
# If it has it returns the result.
# If it has not calculate the inverse and set the value in the cache via
# setinverse function.

cacheSolve <- function(mtx,...) {
  
  # Return a matrix that is the inverse of 'mtx'
  inv <- mtx$getInverse()
  
  # Return the inverse if it is already computed
  if( !is.null(inv) ) {
    message("getting cached data")
    return(inv)
  }
  
  # Get the matrix from the object
  data <- mtx$get()
  
  # Calculate the inverse using solve functio
  inv <- solve(data,...)
  
  # Set the inverse to the object
  mtx$setInverse(inv)
  
  # Return the matrix
  inv
}