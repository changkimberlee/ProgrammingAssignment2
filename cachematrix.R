
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
 set <- function(y) {
  inv <<- y
  m <<- NULL
 }
 get <- function()x
 setsolve <- function(solve) inv <<- solve
 getsolve <- function()inv
 list(set=set, get=get,
  setsolve = setsolve,
  getsolve = getsolve)
}


cacheSolve <- function(x, ...) {
 inv <- x$getsolve()
 if(!is.null(inv)){
  message("getting cached data")
  return(inv)
 }
 data <- x$get()
 inv <- solve(data,...)
 x$setsolve(inv)
 inv
}


