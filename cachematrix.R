## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  
  ## initialize "I" (Inverse) to NULL
  I <- NULL
 
  ## set: function to set the values of a matrix
  set <- function(y) {
    x <<- y
    I <<- NULL
  }
  
  ## get: function to return the values of a matrix
  get <- function() x
  
  
  setinverse<- function(inverse) I <<- inverse
  getinverse <- function() I
  
  list(set = set, get = get,  setinverse = setinverse, getinverse = getinverse)
  
}

## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then cacheSolve should retrieve the
## inverse from the cache.

cacheSolve <- function(x, ...) {
## Return a matrix that is the inverse of 'x'
  
  
## First get the inverse of a matrix returned by getinverse.
## If the inverse isn't NULL then return. 
  I <- x$getinverse()
  if(!is.null(I)) {
    message("getting cached matrix")
    return(I)
  }
  
## If the inverse is NULL then compute the inverse and 
## set the inverse using solve() and setinverse()
  
  m <- x$get()
  
  I <- solve(m, ...)
  x$setinverse(I)
  I
  
}
