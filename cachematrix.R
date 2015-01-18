## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(Y){
    x <<- Y
    invX <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invX <<- inverse
  getinverse <- function() invX
  list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  invX <- x$getinverse()
  if(!is.null(invX)) {
    message("getting cached data")
    return(invX)
  }
  matrix <- x$get()
  invX <- solve(matrix)
  x$setinverse(invX)
  invX
}
