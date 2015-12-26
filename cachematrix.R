## makeCacheMatrix and cacheSolve functions provide a way of 
## calculating matrix inverse and caching its value. If the value of
## the matrix doesn't change, inverse is not calculated every time
## but it is instead returned from internal cache of the special matrix.

## makeCacheMatrix creates and returns a special type of a "matrix"
## which supports caching of its own inverse.
## Function accepts a single parameter, initial matrix.
## Returned "special" matrix provides 4 methods:
## - get/set for getting/setting matrix value
## - getinverse/setinverse for getting/setting matrix inverse value
makeCacheMatrix <- function(x = matrix()) {
  inverseValue <- NULL
  set <- function(y) {
    x <<- y
    inverseValue <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inverseValue <<- inverse
  getinverse <- function() inverseValue
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## cacheSolve returns the inverse of given matrix 'x'.
## Provided matrix needs to be created using makeCacheMatrix
## in order to support caching of inverse
cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setinverse(inv)
  inv
}
