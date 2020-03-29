## Put comments here that give an overall description of what your
## functions do

## This function creates a list of functions to set the matrix data, get it,
## set and save the inverse of this matrix and get the inverse matrix

makeCacheMatrix <- function(x  = matrix()) {
  invMat <- NULL
  set <- function(x_mat) {
    x <<- x_mat
    invMat <<- NULL
  }
  get <- function() {
    x
  }
  setInverse <- function(x_inv) {
    invMat <<- x_inv
  }
  getInverse <- function() {
    invMat
  }
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function checks the vector above for the cached value of the inverse
## of the matrix, and if it does not exist, it will compute the inverse and save it
## Returns the inverse matrix

cacheSolve <- function(x, ...) {
  ## Read out the inverse and see if it is already set
  invMat <- x$getInverse()
  
  if (!is.null(invMat)) {
    ## Inverse is already cached. Yay. Just return it
    message("Getting cached matrix inverse")
    return(invMat)
  }
  
  ## Inverse was not found. Compute it, and set it
  mat <- x$get()
  invMat <- solve(mat, ...)
  x$setInverse(invMat)
  invMat
}
