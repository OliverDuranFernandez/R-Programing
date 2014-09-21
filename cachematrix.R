## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  get <- function() x
  setInverse <- function(Inv) inverse <<- Inv
  getInverse <- function() inverse
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inMa <- x$getInverse()
  if(!is.null(inMa)) {
    message("getting cached data")
    return(inMa)
  }
  data <- x$get()
  inMa <- solve(data, ...)
  x$setInverse(inMa)
  inMa
}
