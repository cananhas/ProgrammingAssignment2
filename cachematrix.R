## Matrix inversion is a computationally expensive process.
## This class performs matrix inversion on a given matrix and
## stores the inverted matrix value in cache for further processes. 

##makeCacheMatrix creates a list containing a function to
#  set value of the matrix
#  get value of the matrix
#  set value of inverse of the matrix
#  get value of inverse of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) inv <<- inverse
  getInverse <- function() inv
  list(set=set, get=get, setInverse=setInverse, getInverse=getInverse)
}


## Returns the inverse of given matrix. Input matrix is first checked whether
## it is inverse already. In case it is inverse, the matrix is returned. If it is
## not inverse, then the function inverse it and sets the value to the cache by using
## setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setInverse(inv)
  inv
}
