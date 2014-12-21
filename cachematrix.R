#This code allow to create an object containing a matrix and that can store
#in cache also the inverse matrix, without necessarily recalculate it everytime its needed

#this is a function that can create a special object that contains
#a matrix, and save into cache its inverse 
makeCacheMatrix <- function(x = matrix()) {
  s <- NULL
  #set the matrix
  set <- function(y) {
    x <<- y    
    s <<- NULL
  }
  get <- function() x
  
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  
  list(set = set, get = get, setinverse=setinverse, getinverse=getinverse)
}


#this function takes in input an object, defined with makeCacheMatrix()
#and return the inverse of the matrix set in the object. if the inverse was
#already be stored in memory this function does not need to recalculate it.
cacheSolve <- function(x, ...) {
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  x$setinverse(s)
  s
  
}
