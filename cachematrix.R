##This program returns the inverse of a matrix using a cache, which 
##can shorten the processing time

##The makeCacheMatrix() function create a matrix that 
## caches its inverse. Only works for a square invertible matrix 
##(returns error if not square)
##The outputs (set, get, setsolve, getsolved) will be used as inputs
##into the next function cacheSolve)

makeCacheMatrix <- function(x = matrix()) {
## x is a square invertible matrix (returns error if not square)

 inv <- NULL
 ##inv is set originally to NULL for there is no inverse yet

 set <- function(y) {
  x <<- y
  inv <<- NULL
  ##with a new data set, inv is set back to NULL
 }

 get <- function()x
 ##get gets the matrix

 setsolve <- function(solve) inv <<- solve
 ##setsolve sets the inverse if inv is not already defined

 getsolve <- function()inv
 ##getsolve gets the inverse matrix

 list(set=set, get=get,
  setsolve = setsolve,
  getsolve = getsolve)
}

## This function returns the inverse of the matrix defined with 
## makeCacheMatrix()(running solve if inv is NOT already stored  
##in the cache, or else pulls the data from the cache)
cacheSolve <- function(x, ...) {
 inv <- x$getsolve()
 
 ##If inv is already defined (is not null), then pull the data 
 ##from the cache without running solve()
 if(!is.null(inv)){
  message("getting cached data")
  return(inv)
 }

 ##If inv is NOT defined (is null), then pull matrix data and run 
 ##solve()
 data <- x$get()

 inv <- solve(data,...)

 ##Puts the solve()output back into cache
 x$setsolve(inv)
 
 ##Return the inverse
 inv
 
}


