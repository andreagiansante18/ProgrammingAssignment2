## makeCacheMatrix retrieves a list of all inner functions so that they 
##can be accessed from outside and 
##cacheSolve will get the matrix, compute the inverse and store it

## Create a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  get <- function() x
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Computes the inverse of the matrix retrieving from cache if all are calculated

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()             
  inv <- solve(data, ...)             
  x$setinverse(inv)             
  inv
}