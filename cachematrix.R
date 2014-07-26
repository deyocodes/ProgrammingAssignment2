## These functions make a special "matrix" object to find and cache
## the inverse of matrix so that if the inverse of the special "matrix"
## has already been calculated, then the result is retrieved from
## cache.
## 

## "makeCacheMatrix" creates a special matrix object that can cache its inverse.


makeCacheMatrix <- function(x = matrix()) {
         m <- NULL
         set <- function(y) {
                  x <<- y
                  m <<- NULL
         }
         get <- function() x
         setInverse <- function(solve) m <<- solve
         getInverse <- function() m
         list(set = set, get = get,
              setInverse = setInverse,
              getInverse = getInverse)
}


## "cacheSolve" computes the inverse of the special "matrix" returned by
## "makeCacheMatrix." If the inverse has already been calculated, "cacheSolve"
## retrieves the result frome cache.

cacheSolve <- function(x, ...) {
         m <- x$getInverse()
         if(!is.null(m)) {
                  message("getting cached data")
                  return(m)
         }
         data <- x$get()
         m <- solve(data, ...)
         x$setInverse(m)
         m
}


