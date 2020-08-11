## Creating a function that give the inverse of a matrix if it cached
## or calculate it if it's veritable and not cached

## Seting a matrix and cached it

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function (y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() {x}
  setInverse <- function(inverse) {inv <<- inverse}
  getInverse <- function() {inv}
  list(set=set, get=get,
       setInverse=setInverse,
       getInverse=getInverse)
  
}


## Calculating the inverse if it is not cached

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("Getting cached data")
    return(inv)
  }
  matr <- x$get()
  inv <- solve(matr, ...)
  x$setInverse(inv)
  inv
}