#Matrix inversion costs a lot of computation. 
#Two functions are used to cache the inverse of a matrix
#rather than compute it repeatedly. 

#makeCacheMatrix creates a matrix that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set <-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function()x
  setsolve<-function(solve) m<<-solve
  getsolve<-function() m
  list(set=set,get=get,setsolve=setsolve,getsolve=getsolve)
}


# chacheSOlve returns the inverse of the matrix 'x'
# If inverse is already calculated, cacheSolve retrieves the inverse from cache

cacheSolve <- function(x, ...) {
  #get inverse
  m<-x$getinverse
  
  #if inervse already exists, check if it is already inversed
  #if yes, returne inverse cached
  
  if(!is.null(m)){
    message("getting cached data")
    return (m)
  }
  
  #if no, get data, compute and cache inverse
  data<-x$get()
  m<-solve(data,...)
  x$setsolve(m)
  m
}
