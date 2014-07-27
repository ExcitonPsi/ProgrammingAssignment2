
## Makes a list of functions on matrix and mean

makeCacheMatrix <- function(x = matrix()) {

  inverse <- NULL
  
  ## Sets value of matrix and clears inverse matrix
  set <- function(y)
  {
    x <<- y
    inverse <<- NULL
  }
  
  ## Gets value of matrix
  get <- function() x
  
  ## Sets value of inverse matrix
  setInverse <- function(y) inverse <<- y
  
  ## Gets value of inverse matrix
  getInverse <- function() inverse
  
  ## Outputs list containing above functions
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)

}


## Calculates inverse of data using functions
## from cacheMatrix (made by makeCacheMatrix)

cacheSolve <- function(cacheMatrix, ...) {
  
  ## If inverse is already cached, gets and returns
  ## it if not, continues
  inverse <- cacheMatrix$getInverse()
  
  if(!is.null(inverse)){
    message("getting cached data")
    return(inverse)
  }
  
  ## Gets matrix using cacheMatrix and calculates its inverse
  data <- cacheMatrix$get()
  inverse <- solve(data, ...)
  cacheMatrix$setInverse(inverse)
  inverse
  
}
