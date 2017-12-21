# This function creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y){
    
    # cache y in x and reset m 
    x <<- y
    
    m <<- NULL
    
  }
  
  get <- function(){
    
    return(x)
    
  } 
  
  setinverse <- function(solve){
    
    m <<- solve
    
  } 
  
  getinverse <- function(){
    
    return(m)
    
  } 
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

#This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x) {
  m <- x$getinverse()
  
  if(!is.null(m)){
    
    # pop up a message if cached data is called
    message("getting cached data")
    return(m)
    
  }
  
  data <- x$get()
  
  m <- solve(data)
  
  x$setinverse(m)
  
  return(m)
}