## makeCacheMatrix: creates an object that can store a matrix and the matrix's inverse
## cacheSolve: accepts one of these objects, and either calculates the inverse, or fetches it from the cache



## This function creates an object that can store a matrix "x", as well as its inverse, "Inv"
## Accessor functions for getting and setting the matrix and its cached inverse (respectively):
##      get(), set(), getInv(), setInv()

makeCacheMatrix <- function(x = matrix()) {

  Inv <- NULL
  set <- function(y) {
    x <<- y
    Inv <<- NULL
  }
  get <- function() x
  setInv <- function(Inverse) Inv <<- Inverse
  getInv <- function() Inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
  
}





## This function expects to be passed an object "x" created by the makeCacheMatrix function
## The ... extra arguments will be passed to solve()

## If the inverse of the matrix has not been cached, it's calculated using solve(), and cached
## if the inverse has already been cached, then the cached inverse is returned without recalculating

cacheSolve <- function(x, ...) {
  i<-x$getInv()
  if(!is.null(i))
  {
    message("getting inverse from cache")
    return(i)
  }
  data<-x$get()
  i<-solve(data,...)
  x$setInv(i)
  i
}
