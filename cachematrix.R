

## There are two functions in this program. The goal is to cache the inverse of a matrix
## The first function creates a matrix object that can cache the inverse of a matrix
## The second function calculates the inverse of the matrix from the first function
## If there is no change in the matrix, the matrix should be retrieved from the first function


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.




makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set,get=get,setinverse=setinverse,getinverse=getinverse)

}


##cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if (!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  m <- x$get()
  inv <- solve(m, ...)
  x$setinverse(inv)
  inv
}
