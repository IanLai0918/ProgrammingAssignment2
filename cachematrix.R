makeCacheMatrix <- function(x = matrix())
{
  get <- function() 
    x
  inverse <- NULL
  setinverse <- function(x)
    inverse <<- solve(x)
  getinverse <- function() 
    inverse
    
}


cacheSolve <- function(x, ...) {
  inverse <- x$getinverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()
  inverse <- solve(data)
  x$setinverse(x)
  inverse
}
