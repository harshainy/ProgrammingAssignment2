## Functions that cache the inverse of a matrix

## First sets the matrix via the set function
##Then gets the matrix via the get function
##After that sets the inverse of the matrix  via the setinverse function 
##and gets the inverse of the matrix via the getinverse function

makeCacheMatrix <- function(x = matrix()) 
{
  i<-NULL
  
  set<-function(y)
  {
    x<<-y
    i<<-NULL
  }

  get<-function()x
  
  setinverse<-function(solve) i<<-solve
  
  getinverse<-function() i 
  
  list(set=set,get=get,
       setinverse=setinverse,
       getinverse=getinverse)
}


## First checks to see if the inverse has already been calculated.
## If so,it gets the inverse matrix from the cache and skips the computation.
##Otherwise it get the inverse of the matrix and sets the inverse in the cache via the setinverse function

cacheSolve <- function(x, ...) 
{
  ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  
  if(!is.null(i))
  {
    message("getting cached inverse matrix")
    return(i)
  }
  data <- x$get()
  i <- solve(data,...)
  x$setinverse(i)
  i
}
