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