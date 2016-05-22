## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
      invMatrix <- NULL
      setMatrix <- function(y) {
            x <<- y
            invMatrix <<- NULL
      }
      getMatrix <- function() x
      setInverseMatrix <- function(invM) invMatrix <<- invM
      getInverseMatrix <- function() invMatrix
      list( setMatrix = setMatrix, getMatrix = getMatrix,
            setInverseMatrix = setInverseMatrix,
            getInverseMatrix = getInverseMatrix)
}


## This function calculates the inverse of the matrix created by the function "makeCacheMatrix". It first checks
## if the inverse matrix was already calculated, if not, it will calculates the inverse and cache it via the 
## function "setInverseMatrix"

cacheSolve <- function(x, ...) {
      ## Return a matrix that is the inverse of 'x'
      invM <- x$getInverseMatrix()
      if(!is.null(invM)) {
            message("getting cached data")
            return(invM)
      }
      matrix <- x$getMatrix()
      invM <- solve(matrix, ...)
      x$setInverseMatrix(invM)
      invM
}
