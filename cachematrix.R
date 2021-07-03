## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
  i <- NULL
  set <- function(y) {
    x <<- y   #set the input as x
    i <<- NULL   #set the solved value "i" as a null
  }
  #Here, I changed every "mean" in the reference to "solve".
  get <- function() x
  setinverse <- function(inverse) i <<- inverse
  getinverse <- function() i
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

## Write a short comment describing this function

#Here, I did the same thing. I changed every "mean" in the reference to "solve".
cacheSolve <- function(x, ...) {
  i <- x$getinverse()                        
  if(!is.null(i)) {                                 
    message("getting cached data")    
    return(i)                                     
  }
  data <- x$get()                            
  i <- solve(data, ...)                         
  x$setinverse(i)                               
  i                                                 
}



