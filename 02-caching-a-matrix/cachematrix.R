## This script uses caching to solve for the inverse of an invertible matrix

# The first function makeCacheMatrix creates a list of 4 elements that : 
# 1) set the value of the matrix
# 2) get the value of the matrix
# 3) set the value of the inverse
# 4) get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
m <- NULL
set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

# The second function cacheSolve takes the matrix as its argument and returns the inverse
# If the inverse has previously been computed, it uses its cached value
# Otherwise it computes it from scratch using R's solve function
# There is no checking of whether the matrix is invertible!

cacheSolve <- function(x) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
}

# Here are some examples to run
# Example 1
mymat1 <-matrix(1:4,nrow=2,ncol=2)

# Example 2
mymat2 <- c(3,2,0,0,0,1,2,-2,1)
dim(mymat2) <- c(3,3)

# Example 3
mymat3 <-c(1,0,5,2,1,6,3,4,0)
dim(mymat3) <- c(3,3)

# Run examples here
mymatx <- makeCacheMatrix(mymat1)
z <- cacheSolve(mymatx)
z <- cacheSolve(mymatx) # second call uses cached data

