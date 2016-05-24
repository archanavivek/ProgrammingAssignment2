## This program is very similar to the Cache mean program. It first has a function
## to create a matrix which caches the inverse. 
## cacheSolve is the function that computes the inverse of the matrix. If the Inverse
## of the matrix is present it simply retrieves from cache. If not it will compute.

makeCacheMatrix <- function(x=matrix()) {
  m<-NULL
  set<-function(y) {
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinv<-function(inverse) m <<-inverse
  getinv<-function() m
  list(set=set,get=get,setinv=setinv,getinv=getinv)
}

cacheSolve<-function(x,...)
{
  m<-x$getinv()
  if (!is.null(m))
  {
    message("getting cahced data")
    return(m)
  }
  data<-x$get()
  m<-solve(data,...)
  x$setinv(m)
  m
}
