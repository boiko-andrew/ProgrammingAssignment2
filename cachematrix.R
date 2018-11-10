## The following two functions calculate and cache the inverse
## of a matrix


## This function creates a special "matrix"
## which is a list containing a function to:
## set the value of the matrix
## get the value of the matrix
## set the value of the inverse matrix
## get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverseMatrix) {
    inverse <<- inverseMatrix
  }
  getInverse <- function() inverse
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function calculates the inverse matrix. If it has
## already been calculated then this function returns the
## inverse matrix from the cache

cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached matrix")
    return(inverse)
  }
  inputMatrix <- x$get()
  inverse <- solve(inputMatrix, ...)
  x$setInverse(inverse)
  inverse
}
