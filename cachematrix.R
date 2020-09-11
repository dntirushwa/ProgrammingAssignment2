## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix is a function that creates a  "matrix" object that can
#cache its inverse for the input that is an invertible square matrix.

makeCacheMatrix <- function(A = matrix()) {
  i <- NULL
  set <- function(x){
    A <<- x
    i <<- NULL
  }
  get <- function()A
  setInverse <- function(inverse) i <<- inverse
  getInverse <- function() i 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)
  
}

## Write a short comment describing this function
# The cacheSolve is a function that computes the inverse of the special "matrix"
#created by the makeCacheMatrix function created previously. If the inverse has already been calculated
#and the matrix has not changed, then the cachesolve function should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'     
  i <- x$getInverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  mat <- x$get()
  i <- solve(mat,...)
  x$setInverse(i)
  i
}
