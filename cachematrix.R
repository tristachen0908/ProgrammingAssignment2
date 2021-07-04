# This function creates a special "matrix" object that can cache its inverse.
# set the input x as matrix
makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y   #set the input as x
    inv <<- NULL   #set the solved value "inv" as a null
  }
  get <- function() x #function to get matrix x
  setinverse <- function(inverse) inv <<- inverse  #calculate the inverse
  getinverse <- function() inv #function to get the inverse of the matrix
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# This function computes the inverse of the special "matrix" created by 
# makeCacheMatrix above. If the inverse has already been calculated (and the 
# matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  #Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()                        
  if(!is.null(inv)) {                 #checking whether inverse is NULL                
    message("getting cached data")    
    return(inv)                         # returns inverse value          
  }
  data <- x$get()                            
  inv <- solve(data, ...)    #calculates inverse value                     
  x$setinverse(inv)                               
  inv         #return a matrix that is the inverse of 'x'                                        
}



