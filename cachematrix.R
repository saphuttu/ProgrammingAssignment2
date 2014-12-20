## Programming Assignment 2: Lexical Scoping

## makeCacheMatrix creates a special matrix.
## In fact its a list containing 4 functions.
## set matrix
## get matrix
## set inverse matrix
## get inverse matrix
makeCacheMatrix <- function(x = matrix()) {
  m<-NULL
  set<-function(y){
    x<<-y
    m<<-NULL
  }
  get<-function() x
  setinvmatrix<-function(solve) m<<- solve
  getinvmatrix<-function() m
  list(set=set, get=get,
       setinvmatrix=setinvmatrix,
       getinvmatrix=getinvmatrix)
}


## cacheSolve is a function that calculates the inverse of special matrix
## created earlier. If the same matrix is calculated earlier it get's the
## value from cache.
## Otherwise the function calculates the inverse and put it in the cache.
cacheSolve <- function(x=matrix(), ...) {
  m<-x$getinvmatrix()
  if(!is.null(m)){
    message("There is something in cache")
    return(m)
  }
  matrix<-x$get()
  m<-solve(matrix, ...)
  x$setinvmatrix(m)
  m
}
