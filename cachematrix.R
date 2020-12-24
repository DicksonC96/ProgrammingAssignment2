## Matrix inversion is usually a costly computation and there may be some benefit
## to cache the inverse of a matrix rather than compute it repeatedly.

## The makeCacheMatrix function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  # Initialize empty vector for inverse matrix
  i <- NULL
  # Setting up caching functions for the matrix
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inversed) i <<- inversed
  getInverse <- function() i
  # Returning a list of caching functions
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## The cacheSolve function computes the inverse of the special "matrix" 
  ## returned by makeCacheMatrix above. If the inverse has already been calculated
  ## (and the matrix has not changed), then the cacheSolve should retrieve the inverse
  ## from the cache.

cacheSolve <- function(x, ...) {
  # Getting cached inversion result
  i <- x$getInverse()
  
  # Returning cached result if any
  if(!is.null(i)) {
    message("Returning cached result...")
    return(i)
  }
  
  # Solving matrix inversion if x has not been calculated
  matrix <- x$get()
  inversed <- solve(matrix, ...)
  # Caching the inversion result
  x$setInverse(inversed)
  # Returning inversion result
  message("Calculating matrix inversion...")
  inversed
}
