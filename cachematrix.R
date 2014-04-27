## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## This function creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(inputMatrix = matrix()) {
  inverseMatrix <- NULL
  set <- function(y) {
    inputMatrix <<- y
    inverseMatrix <<- NULL
  }
  get <- function() inputMatrix
  setinverse <- function(solve) inverseMatrix <<- solve
  getinverse <- function() inverseMatrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}


## Write a short comment describing this function
## This function computes the inverse of the special "matrix" 
## returned by makeCacheMatrix above. If the inverse has already been 
## calculated (and the matrix has not changed), then the cachesolve 
## should retrieve the inverse from the cache
cacheSolve <- function(inputMatrix, ...) {
    ## Return a matrix that is the inverse of 'inputMatrix'
    inverseMatrix <- inputMatrix$getinverse()
    if(!is.null(inverseMatrix)) {
      message("getting cached data")
      return(inverseMatrix)
    }
    data <- inputMatrix$get()
    inverseMatrix <- solve(data, ...)
    inputMatrix$setinverse(inverseMatrix)
    inverseMatrix
}


## Example:  How to run this do following

A = matrix( 
   c(2, 4, 3, 1), # the data elements 
   nrow=2,              # number of rows 
   ncol=2,              # number of columns 
   byrow = TRUE)        # fill matrix by rows 

c <- makeCacheMatrix(A)

#first time calling cacheSolve will cache the inverse and return the inverse
cacheSolve(c)

#subsequent time call to cacheSolve will return the cached data
cacheSolve(c)
