makeCacheMatrix <- function(x = matrix())
{
  get <- function() 
    x
  inverse <- NULL       #make sure the inverse is originally empty
  setinverse <- function(x)       
    {inverse <<- solve(x)}      #let the variable 'inverse' contains the inverse matrix of the input matrix
  getinverse <- function() 
    {inverse}                #get the inverse matrix
    
  inverse   #return the inverse
}


cacheSolve <- function(x, ...) 
{
  inverse <- x$getinverse()
  if(!is.null(inverse))             #check whether the variable 'inverse' contains any value already
  {
    message("getting cached data")
    return(inverse)
  }
  data <- x$get()          #set 'data' contains the value of the input matrix
  inverse <- solve(data)       #inverse now contains the inverse matrix of 'data'
  x$setinverse(x)
  {inverse}                 #return the value of the inverse matrix
}
