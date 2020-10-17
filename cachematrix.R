## makeCacheMatrix and cacheSolve together provide functionality to calculate 
## and cache the result of a inverse operation of a Matrix

## takes a matrix as input, provides getter/setter methods for contents and
## inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set = set, get = get, setinverse = setinverse,
       getinverse = getinverse)
}

## The function, cacheSolve(x), calculates and caches the inverse of
## a Matrix, x,  created via the function, makeCacheMatrix, shown above. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached inverse")
    return(inv)
  }
  
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv
}

# # Creates a matrix
# myMatrix <- matrix(1:4, 2, 2)
# # Creates a cacheable representation of myMatrix
# cacheableMatrix <- makeCacheMatrix(myMatrix)
# # Calculates and returns inverse of myMatrix and cache result
# inverse1 <- cacheSolve(cacheableMatrix)
# inverse1
# # Returns inverse of myMatrix from cache
# inverse2 <- cacheSolve(cacheableMatrix)
# inverse2
