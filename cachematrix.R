## Caching the Inverse of a Matrix

##  This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinv <- function(inv = matrix()) m <<- inv
  getinv <- function() m
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above
##If the inverse has already been calculated it retrieves the inverse from the Cache

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
  
  ## Return a matrix that is the inverse of 'x'
}

a<- matrix(1:4, nrow = 2, ncol = 2)
y<-makeCacheMatrix(a)
cachesolve(y)

##Solution 

##      [,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5
