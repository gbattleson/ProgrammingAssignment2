
## This function will create a matrix object so that the 
## following function cacheSolve can calculate the inverse
## or if the invcerse has already been stored it retrives 
## the cached information.

## The first step is to create a Matrix
## x <- matrix(rnorm(25), 5, 5)

makeCacheMatrix <- function(x = matrix()) {
  z <- NULL
  set <- function(y) {
    x <<- y
    z <<- NULL
  }
  get <- function() x
  setinverseM <- function(inverse) z <<- inverse
  getinverseM <- function() z
  
  list(set = set, get = get,
       setinverseM = setinverseM,
       getinverseM = getinverseM)
}

## Then run makeCacheMatrix(x)$get()
## Yay it worked but we need a variable for the cacheSolve
## function so lets run M <- makeCacheMatrix(x)

## cacheSolve will calculte the inverse or if that has already been done
## it will retreive the inverse from the cache hense the name

## Now the first time you run cacheSolve it will create
## the cache and do the computation so for a big matrix this could take a while
## but the second time it will display the message
## "getting cached matrix data" as it does not have to do the computation twice
## so simply run cacheSolve(M) twice to see both results
## This will only work on a matrix that is always invertible

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  z <- x$getinverseM()
  if(!is.null(z)) {
    message("getting cached matrix data")
    return(z)
  }
  data <- x$get()
  z <- solve(data, ...)
  x$setinverseM(z)
  z
  }
