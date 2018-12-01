## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function will solve the inverse form of the given matrix


makeCacheMatrix <- function(x = matrix()) {
 mat_inv <- NULL
  set <- function(numb) {
    x <<- numb
    mat_inv <<- NULL
  }
  
  get <- function() x
  setInverse <- function(inverse) mat_inv <<- inverse
  getInverse <- function() mat_inv
  list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)  
}


## Write a short comment describing this function
## This function will retrieve the inverse value from the cache if it is already calculated

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 mat_inv <- x$getInverse()
  if (!is.null(mat_inv)) {
    message("getting cached data")
    return(mat_inv)
  }
  
  numb_mat <- x$get()
  mat_inv <- solve(numb_mat, ...)
  x$setInverse(mat_inv)
  mat_inv       
}


