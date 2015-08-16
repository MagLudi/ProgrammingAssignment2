## This program returns an inverse of a matrix. When a matrix cache is passed
## in through cacheSolve it first checks to see if its inverse has already been
## calculated. If so a message is printed and the inverse is retrieved from the
## cache and returned. If not, then it will calculate the inverse and save it to
## the cache before returning the inverse.

## Two assumptions are made:
## 1. makeCacheMatrix is called prior to the implementation of cacheSolve
## 2. all maticies do have an inverse

## makeCacheMatrix creates a list containing a function to set and
## get the values of the matrix that is passed in and its inverse

makeCacheMatrix <- function(x = matrix()) {
  y <- NULL
  set <- function(z) {
    x <<- z
    y <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) y <<- inverse
  getinverse <- function() y
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Modified version of cachemean function example. Instead of checking
## to see if the mean of a vector has been calculated aready, it checks
## to see if the inverse of matatrix had been found. If not it will
## calculate the inverse and use setinverse to set the result in the cahe

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  y <- x$getinverse()
  if(!is.null(y)) {
    message("getting cached data")
    return(y)
  }
  data <- x$get()
  y <- solve(data, ...)
  x$setinverse(y)
  y
}
