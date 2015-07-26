## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    
    #cached Result
    inverse_mat <- NULL
    
    #Definition of SET function
    set <- function(y) {
      x <<- y
      inverse_mat <<- NULL
    }
    
    #Definition of GET function
    get <- function() x
    
    #Definition of SETINVERSE function
    setinverse <- function(inv) inverse_mat <<- inv
    
    #Definition of GETINVERSE function
    getinverse <- function() inverse_mat
    
    #Return: List of functions
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
  
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
    inver_mat <- x$getinverse()
    
    if(!is.null(inver_mat)) {
      message("getting cached data")
      return(inver_mat)
    }
    
    data <- x$get()
    inver_mat <- solve(data, ...)
    x$setinverse(inver_mat)
    inver_mat
  
}