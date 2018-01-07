##These functions cache and return the inverse of a matrix.  

##If the inverse of a given matrix was previously calculated, 
##the value will be returned from the cache.  

##If not previously calculated, the functions will calculate the inverse and
##return it. 


## This function creates a list of functions to be used my cacheSolve 

makeCacheMatrix <- function(x=matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y
    i <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) i <<- solve
  getinverse <- function() i
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
  
}



## This function refers to the list of functions created in makeCacheMatrix to 
## calculate the inverse of a matrix.  If the inverse of the matrix was 
## already calculated, it will retrieve the value from the saved variable.  Otherwise 
## it will calculate the inverse and return it.    

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  i <- x$getinverse()
  if(!is.null(i)){
    message("getting cached data")
    return(i)
  }
  data <- x$get()
  i <- solve(data, ...)
  x$setinverse(i)
  i
}


