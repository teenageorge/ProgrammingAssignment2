
## function to store the value of a matrix and cache it's inverse
makeCacheMatrix <- function(x = matrix()){
  y <- NULL
  invY <- NULL
  
  get <- function(){
    y <<- x
    y
  }
  
  getInverse <- function(i){
    invY
  }
  setInverse <- function(inv){
    invY <<- inv
  }
  list(get = get, setInverse = setInverse, getInverse = getInverse)
}

## Function to create the inverse if it is not already existing.
## if it exists, it is served from the cache
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  matr <- x$get()
  if(!is.null(inverse)){
    message("Getting the cached data")
    return(inverse)
  }else{
    inverse <- solve(matr, ...)
    x$setInverse(inverse)
    inverse
  }
  
}