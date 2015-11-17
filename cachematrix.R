## Put comments here that give an overall description of what your
## functions do

## function to create matrix that can cache its reverse

makeCacheMatrix <- function(x = matrix()) {
           invmx<- NULL
           set<- function(y){
             x <<- y
             invmx<<- NULL
           } 
          get <- function() x
          setinv<- function(inverse)invmx <<- inverse
          getinv<- function ()invmx
          list(set=set, get=get, setinv= setinv, getinv=getinv)
}


## function computes the inverse of matrix returned by makecachematrix above.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  invmx<- x$getinv
  #if inverse is already been calculated
  if(!is.null(inv))
    #get it from cached data
  {message("getting cached data")
    return(invmx)

  }
  # otherwise, calculate the inverse
  new<- x$get()
  invmx<- solve(new,...)
  x$setinv(invmx)
  return(invmx)
}
