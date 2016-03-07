## the first function creates a a set of functions to create or set the inverted Matrix in Cache
## The second function calculates the inverse of the Matrix if it doesn't exist in cache

## Create functions used by cacheSolve to get or set the inverse matrix depending on current cache

makeCacheMatrix <- function(x = matrix()) {
  Matrix_Inv <- NULL
  set <- function (y) {
    x <<- y
    Matrix_Inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) Matrix_Inv <<- inverse 
  getInverse <- function() Matrix_Inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}





## Creates or returns an inverse matrix into cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  Matrix_Inv <- x$getInverse()
  if (!is.null(Matrix_Inv)) {
    message("Returning Cached Inverse of Matrix")
    return(Matrix_Inv)
  } else {
      Matrix_Inv <- solve(x$get())
      x$setInverse(Matrix_Inv)
      return(Matrix_Inv)
  }
}
