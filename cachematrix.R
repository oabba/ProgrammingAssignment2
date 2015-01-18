## the functions stored in this file collectively stores (caches) the input matrix and its inverse
## in the parent environment. An example use of this is 
# M <- matrix(sample.int(15, size = 25, replace = TRUE), nrow = 5, ncol = 5) # Example matrix
# source("cachematrix.R") # Loading R script into R
# MList <- makeCacheMatrix(M) # caches matrix into parent environment
# cacheSolve(MList) # computes inverse and caches it into parent environemnt
# cacheSolve(MList) # note the inverse is already computed, it only get the inverse from parent environemnt


## Details on makeCacheMatrix()
# makeCacheMatrix(x=matrix()) stores the matrix x in a parent environment through subfunction set(x) 
# enables us to retrieve the matrix through subfunction get() and initializes a dummy inverse and stores
# the inverse in parent environment. Finally, makeCacheMatrix() returns a list of the following
# objects
# 1) setting the matrix to parent environment
# 2) getting the matrix
# 3) setting the matrix inverse to partnet enviroment
# 4) gettting the inverse
makeCacheMatrix <- function(x = matrix()) {
  invX <- NULL
  set <- function(Y){
    x <<- Y
    invX <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) invX <<- inverse
  getinverse <- function() invX
  xList <- list(set = set, get = get,
       setinverse = setinverse, 
       getinverse = getinverse)
}


## Write a short comment describing this function
# input to cacheSolve() is a list with four objects as described earlier. It checks 
# wheter the inverse of the matrix exists. If it does, it returns the inverse else, it 
# obtains the matrix via the get() subfunction of makeCacheMatrix and computes the matrix inverse
# and caches the inverse to the parent environment via setinverse() subfunction of makeCacheMatrix
# and returns the inverse

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
