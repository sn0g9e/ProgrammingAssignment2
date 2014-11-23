
makeCacheMatrix <- function(x = matrix()) {  # input x will be a matrix
  inv <- NULL #set to NULL each time makeCacheMatrix() is called
  set <- function(y) {
    #superassignment_
    x <<- y 
    inv <<- NULL #if matrix is re-assigned it invalidates the previously computed inverse.
  }
  get <- function() x #returns the value of the original vector
  
  #this is called by cacheSolve() during the first cacheSolve()
  #access and it will store the value using superassignment
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  
  #invoke the list function which returns a list. 
  #accessed each time makeCacheMatrix() is called, 
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}

# Computes, caches, and returns a matrix inverse
cacheSolve <- function(x, ...) { # the input x is an object created by makeCacheMatrix()
  # Return a matrix that is the inverse of 'x'
  # can be called with amatrix$getinverse()
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data.")
    return(inv)
  }
  # get() can be called from 'outside' and returns 'original' matrix
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
     

