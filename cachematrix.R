## Two functions below calculate inverce matrix and chaches the result 


## The function below receies as matrix as parameter. Returs the list with available
## methods. 
## setinv caches inverted matrix.  
## getinv, returns cached matrix.
## set, sets the value of the matrix
## get, gets the value of the matrix
makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)

}


## cacheSolve receives as a parametr an instance of makeCacheMatrix function.
## It uses function solve to return inverted matrix if getinv function returns value NULL.
## Otherwise it returns cached result of solve function. Once result is calculated 
## makeCacheMatrix caches the return value.

cacheSolve <- function(x, ...) {
  m <- x$getinv()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data, ...)
  x$setinv(m)
  m
}
