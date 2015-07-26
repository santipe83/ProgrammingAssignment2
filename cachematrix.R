

makeCacheMatrix <- function(x = matrix()) {
## Creates a special "matrix" object that can cache 
##
## Args: 
##    x:    Matrix to solve
##    ... : Arguments available for Solve() function
## 
## Returns: 
##    A list of functions
      
    #cached Result
    inverseMat <- NULL
    
    #Definition of SET function
    set <- function(y) {
      x <<- y
      inverseMat <<- NULL
    }
    
    #Definition of GET function
    get <- function() x
    
    #Definition of SETINVERSE function
    setInverse <- function(inv) inverseMat <<- inv
    
    #Definition of GETINVERSE function
    getInverse <- function() inverseMat
    
    #Return: List of functions
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
  
}



cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  ## This function computes the inverse of the special "matrix"
  ## returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.
  
    inverMat <- x$getInverse()
    
    #Checks if Inverse of X has been already calculated
    if(!is.null(inverMat)) {
      message("getting cached data")
      return(inverMat)
    }
    
    #Calculation of inverse of X (caching the result too)
    data <- x$get()
    inverMat <- solve(data, ...)
    x$setInverse(inverMat)
    inverMat
  
}