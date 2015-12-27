## This is Jeff Watson's Programming Assignment 2 submission for the Coursera R Programming course.
## Date: December 27, 2015 
## Put comments here that give an overall description of what your functions do

## Write a short comment describing this function:
## The "makeCache Matrix" function below creates a matrix object that is capable of aching it inverse.
  

makeCacheMatrix <- function(x = matrix()) {

## Setting the cached Inverse equal to NULL

  cachedInverse <- NULL
  set <- function(y) {
         x <<- y
         cachedInverse <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) cachedInverse <<- inverse
  getInverse <- function() cachedInverse
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## Write a short comment describing this function:
## The "cacheSolve" matrix below calculates the inverse of the matrix object produced by the aforementioned "makeCache" Matrix function.
##   The logic in this particular function is to determine:
##     If the inverse of this matrix has already been previously computed:
##       1.) If the answer is "Yes", then this function will simply extract the already calculated inverse of the matrix from the cache.
##       2.) If the answer is "No", then it will calculate the inverse of this matrix. 

cacheSolve <- function(x, ...) {

   ## Return a matrix that is the inverse of 'x'
inverseFunction <- x$getInverse()
  if (!is.null(inverseFunction)) {
      message("extracting the cached data in mere nanoseconds")
      return(inverseFunction)
  }
  data <- x$get()
  inverseFunction <- solve(data, ...)
  x$setInverse(inverseFunction)
  inverseFunction
}
