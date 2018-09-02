## Put comments here that give an overall description of what your
## functions do
## makeCacheMatrix takes in an input matrix and returns a list of functions (input to the function cacheSolve)
## --by handle=sshia1 09/01/2018

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
matrix_inverse <- NULL
  set <- function(y) {
    x <<- y
    matrix_inverse <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) matrix_inverse <<- inverse
  getinverse <- function() matrix_inverse
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## cacheSolve() takes the return value of makeCacheMatrix() and returns the inverse matrix (if it exists)

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  matrix_inverse <- x$getinverse()
  if(!is.null(matrix_inverse)) {
    message("getting cached matrix data")
    return(matrix_inverse)
  }
  data <- x$get()
  matrix_inverse <- solve(data)
  x$setinverse(matrix_inverse)
  matrix_inverse
}
