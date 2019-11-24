## Matrix inversion is usually a costly computation and there may be some benefit to caching the inverse of a matrix 
## rather than compute it repeatedly. 
## Following pair of functions cache the inverse of a matrix (and the matrix too).

## This function creates a user defined object with a list a return type - to store cache of a matrix and its inverse 

makeCacheMatrix <- function(x = matrix()) {
  matrix_inversed <- NULL
  
  #Setting and getting matrix and its inverse
  set <- function(matrix_input) {
    x <<- matrix_input
    matrix_inversed <<- NULL
  }
  get <- function() x
  setInverse <- function(solve) matrix_inversed <<- solve
  getInverse <- function() matrix_inversed
  
  #Returning list with mutators and accessors
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function finds inverse of a matrix if unknown and stores it in an object of makeCacheMatrix. If inverse is already cached, the value is retrieved

cacheSolve <- function(x, ...) {
  matrix_inversed <- x$getInverse()
  
  if(!is.null(matrix_inversed)) {
    message ("Getting cached data")
    return(matrix_inversed)
  }
  
  message ("Calculating inverse and storing cache")
  matrix_inversed<-solve(x$get())
  x$setInverse(matrix_inversed)
  matrix_inversed
}