## This two functions make use of the scoping rules in R. 
## A matrix made of functions is created, a second function makes use of the first one in order to:
## Check if the inverse of a matrix is already calculated, if not it calculates it

## This function creates a fake matrix from a list of functions which do the following: 
#Set the value of a matrix
#Get the value of a matrix
#Set the value of the inverse of a matrix
#Get the value of the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setmatrix <- function(solve) inv <<- solve
  getmatrix <- function() inv
  list(set = set, get = get,
       setmatrix = setmatrix,
       getmatrix = getmatrix)
}


## This function check if the inverse of a matrix is already computed and if not computes it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getmatrix()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setmatrix(inv)
  inv
}
