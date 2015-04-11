## This file contains two functions which help with caching and retrieving
## the inverse of the given matrix.

## makeCacheMatrix() create an object which encapsulate a matrix and
## it's inverse. Provides $get(), $set(), $getInverse() and $setInverse().

makeCacheMatrix <- function(x = matrix()) {
  inverseX <- NULL

  set <- function(y) {
    x <<- y
    inverseX <<- NULL
  }

  get <- function() x

  setInverse <- function(inverse) inverseX <<- inverse

  getInverse <- function() inverseX

  list(set = set,
       get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## cacheSolve() takes an object created by makecacheMatrix() and uses
## the cached inverse if available, otherwise calculate the inverse 
## and save it into the given cacheMatrix object.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inverseX <- x$getInverse()
  if (!is.null(inverseX)) {
    message("getting cached data")
    return(inverseX)
  }
  inverseX <- solve(x$get(), ...)
  x$setInverse(inverseX)
  inverseX
}
