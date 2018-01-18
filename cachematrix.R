## Matrix computations:
## Creates and stores the matrix and inverse

## Creates a matrix object (y), 
## returns the matrix value,
## stores the inverse of the matrix,
## and returns a list of all the above functions

makeCacheMatrix <- function(x = matrix()) {
  y <- NULL
  invY <- NULL
  
  get <- function(){
    y <<- x
    y
    invY <<- NULL
  }
  
  inverse <- function(){
    invY <- solve(x)
    invY
  }
  list(get = get, inverse = inverse)
}


## cacheSolve function calculates the inverse of the function.
## The calculated value is stored in and fetched from
##the cache for the same input matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        matr <- x$get()
        if(!is.null(inverse)){
          message("Getting the cached data")
          return(inverse)
        }
        inverse <- solve(matr, ...)
        x$setInverse(inverse)
        inverse
        
}
