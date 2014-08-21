## Matrix inversion is usually a costly computation and their may be some
## benefit to caching the inverse of a matrix rather than compute it
## repeatedly. The following pair of functions cache the inverse of a matrix.

## The function makeCacheMatrix creates a special "vector", which is really a
## list containing a function to: (a) set the value of the matrix, (b) get the
## value of the matrix, (c) set the value of the matrix and d get the value of
## the matrix

makeCacheMatrix <- function(x = matrix()) {
  reverse <- NULL
  set <- function(y) {
    x <<- y
    reverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) reverse <<- inverse
  getinverse <- function() reverse
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

## The function cacheSolve calculates the inverse of the special "vector" created
## with the makeCacheMatrix function. However, it first checks to see if the
## inverse has already been calculated. If so, it gets the inverse from the cache
## and skips the computation. Otherwise, it calculates the inverse of the matrix
## and sets the value of the inverse in the cache via the setinverse function.

cacheSolve <- function(x, ...) {
  ## Returns a matrix that is the inverse of 'x'
  reverse <- x$getinverse()
  if(!is.null(reverse)) {
    message("Getting cached data.")
    return(reverse)
  }
  data <- x$get()
  reverse <- solve(data)
  x$setinverse(reverse)
  reverse 
}