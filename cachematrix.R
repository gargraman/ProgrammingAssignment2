## functions do
#########################
##
##get()
##set(y)
##getinverse()
##setinverse(invers)
##
#########################

makeCacheMatrix <- function(x = matrix()) {
  mat <- NULL
  set <- function(y){
    x <<- y
    mat <<- NULL
  }
  get <- function(){
    x
  }
  setinverse <- function(invers){
    mat <<- invers
  }
  getinverse <- function() mat
  list(set = set, get=get,setinverse=setinverse,getinverse=getinverse)
  
}


## Write a short comment describing this function
##########################################################
##
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  mat <- x$getinverse()
  if(!is.null(mat)){
    message("getting cached inverse")
    return (mat)
  }
  data <- x$get()
  mat <- solve(data,...)
  x$setinverse(mat)
  mat
}
