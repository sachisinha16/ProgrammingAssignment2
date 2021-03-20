setwd('C:\Users\ss14674\Downloads\R_coursera\Ass2')
## Set the input x as a matrix
## Set the solved value "s" as a null
## Change every reference to "mean" to "solve"

##makeCacheMatrix is a function which creates a  "matrix" object that can cache its inverse for the input
makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}

#cacheSolve is a function which computes the inverse of the "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
  i <- x$getInverse()
  if(!is.null(i)) {
    message("getting inversed matrix")
    return(i)
  }
  
  mat <- x$get()
  i <- solve(mat, ...)
  x$setInverse(i)
  i
}

#for testing
m<-matrix(rnorm(16),4,4)
m1<-makeCacheMatrix(m)
cacheSolve(m1)