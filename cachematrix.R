## makeCacheMatrix creates a special "matrix" object that can cache a matrix
## cacheSolve: This function computes the inverse of the special "matrix" 
## If the Cache already contains the matrix, the matrix is printed vs. calculated

## makeCacheMatrix: Creates a special "matrix" object
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL                                       ##initializing as NULL
  set <- function(y)  {                           
    x <<- y
    m <<- NULL
  }
  get <- function() x                             ##returns x
  setinverse <- function(solve) m <<- solve       ##solve used to calculate inverse
  getinverse <- function() m                      ##returns m
  list(set = set, get = get,                      
       setinverse = setinverse,
       getinverse = getinverse)
}
##This function computes the inverse of the special "matrix" returned by makeCacheMatrix function 
##If the inverse of the original matrix is already stored in cache, the cachesolve will 
##retreive the matrix.  If the matrix is not stored in cache the value is calculated.

cacheSolve <- function(x, ...) {
  
  m <- x$getinverse()
  if(!is.null(m)) {             ##Checking if inverse already calculated
    message("getting cached data")  ##If cached, print message
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinverse(m)
  m                             ## Returns the inverse matrix
}