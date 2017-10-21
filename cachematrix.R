## This script has functioins for creating a matrix object, calculating inverse matrix and storing
## results in the parent enviroment. When the cacheSolve function is called
## it first checks out the cache before running expensive calculations,
## making the process a lot more efficient.
## example:
##   myData = matrix(c(2, 4, 3, 1), nrow=2, ncol=2)
##   myCache <- makeCacheMatrix()
##   myCache$set(myData)
##   cacheSolve(myCache)


## makeCacheMatrix | creates an empty matrix object in the current enviroment 
## that exposes set, get, and inverse functions.
## On initialization it clears current and parent x and s variables


makeCacheMatrix <- function(x = matrix()) {
  message("creating makeCacheMatrix")
  s <- NULL
  set <- function(y) {
    x <<- y
    s <<- NULL
  }
  get <- function() x
  setinverse <- function(solve) s <<- solve
  getinverse <- function() s
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)

}


## cacheSolve | stores the value of solve(x) function in the cached matrix created
## in the makeCacheMatrix
## if the cache vale exists it returned immidiatly, otherwise solve is run
## on x and the value stored in cache for future use


cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  s <- x$getinverse()
  if(!is.null(s)) {
    message("getting cached data from makeCacheMatrix")
    return(s)
  }
  data <- x$get()
  s <- solve(data, ...)
  message("setting cached data in makeCacheMatrix")
  x$setinverse(s)
  s
  }
