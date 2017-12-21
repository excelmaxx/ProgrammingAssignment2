makeCacheMatrix <- function(x = matrix()) {
  
  m <- NULL
  
  set <- function(y){
    
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
    
    m
    
  } 
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


cacheSolve <- function(x) {
  m <- x$getinverse()
  
  if(!is.null(m)){
    
    return(m)
    
  }
  
  data <- x$get()
  
  m <- solve(data)
  
  x$setinverse(m)
  
  return(m)
}