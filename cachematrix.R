# There is like interface for getting inverse of matrix and the origin matrix
makeCacheMatrix <- function(matrixForCache = matrix()) {
  inv <- NULL
  set <- function(newMatrix) {
    matrixForCache <<- newMatrix
    inv <<- NULL
  }
  get <- function() matrixForCache
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

# This is a function that uses for caching inverse, it works only with result of makeCacheMatrix(yourMatrix)
cacheSolve <- function(matrixObject, ...) {
  matrixInverse <- matrixObject$getInverse()
  if(!is.null(matrixInverse)) {
    message("getting cached data")
    return(matrixInverse)
  }
  matr <- matrixObject$get()
  matrixInverse <- solve(matr, ...)
  matrixObject$setInverse(matrixInverse)
  matrixInverse
}
