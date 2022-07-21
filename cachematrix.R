## Functions that cache the inverse of the matrix

## A matrix object is created that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  i <- NULL
  
  set <- function( matrix ) {
    m <<- matrix
    i <<- NULL
  }
  
  get <- function () {
     m
  }
  
  setInverse <- function(inverse) {
    i <<- inverse
  }
  
  getInverse <- function() {
     i
  }
  
  list(set = set, get = get,
       setInverse = setInverse, getInverse = getInverse)
}


## Compute the inverse of the matrix (formed by the makeCacheMatrix function)
## if inverse is already calculated then the following function will retrieve the inverse

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  
  if (!is.null(m)){
    message("retrieving cached data")
    return(m)
  }
   data <- x$get()
   
   m <- solve(data) %*% data
   
   x$setInverse(m)
   
   m
}
