## Below are the 2 functions that can be used to create an object that a matrix
## and can cache its inverse.

## This function creates a special "matrix" object that can cache its inverse.
## I used the same format of the example provided in the assignment instructions.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
      x <<- y
      inv <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) inv <<- inverse
    getInverse <- function() inv
    list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## This function computes the inverse of the matrix created by the function
## "makeCacheMatrix". In case the inverse has already been computed previously,
## this function will retrive the inverse from cache (and say 'getting cached data')


cacheSolve <- function(x, ...) {
  inv <- x[getInverse()]
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
