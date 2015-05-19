## This R script functions to cache the inverse of a given matrix. 
## Assumption: matrix supplied is always invertible. 

## makeCacheMatrix creates a special "matrix" that can cache its 
## inverse.

makeCacheMatrix <- function(x = matrix()) {
  ## x = square invertible matrix
  ## return list of functions to 
  ## 1) set the matrix
  ## 2) get the matrix
  ## 3) set the inverse matrix
  ## 4) get the inverse matrix
  
  inv <- NULL
  
  ## setting a matrix: makeCacheMatrix.set(Matrix_A) 
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  ## getting a matrix
  get <- function()x
  
  ## setting the inverse of a matrix
  setinverse <- function(solve) inv <<- solve
  
  ## getting he inverse of a matrix through function inv 
  getinverse <- function() inv
  list(set = set, 
       get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}

## cacheSolve computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already 
## been calculated (and the matrix has not changed), then the 
## cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  
  if(!is.null(inv)) {
    message("getting cached data!")
    return(inv)
  }
  ## getting matrix at the time, stored as "data"
  data <- x$get()
  ## using "data" tp output inverse matrix, then set the inverse matrix
  ## into makeCacheMatrix
  inv <- solve(data)
  x$setinverse(inv)
  inv  
}