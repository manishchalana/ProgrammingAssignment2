## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
      invx<-NULL
      # Following function sets the value of argument passed to the matrix
      set <- function(y){
            x<<- y
            invx <<- NULL
      }
      # Following function retrieves the value of matrix from memory
      get <- function() x
      
      # Following function set the value of inverse to be equal to argument passed
      setinv <- function(inv) invx <<- inv
      
      # Following function retrieves the value of inverse of the matrix from memory
      getinv <- function() invx
      
      # Returning list of get, set functions for matrix and its inverse
      list(set=set, get=get, setinv=setinv, getinv=getinv)

}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x
      
      # Return the value of inverse that is stored in memory
      invx = x$getinv()
      
      # If inverse is already calculated earlier, then program will enter the 'if' and return the inverse
      if(!is.null(invx)){
            message("getting cached data")
            return(x$getinv())
      }
      
      # If inverse is not set, then inverse is first calcuated by using the solve function, stored
      # in memory and then returned
      data <- x$get()
      invx <- solve(data)
      x$setinv(invx)
      invx
}
