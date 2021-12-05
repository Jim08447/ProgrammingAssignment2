# The two functions below are used to create a special object
# that stores a matrix and cache's it's inverse.

# The first function, makeCacheMatrix creates a special Matrix, which is really a
# list containing a function to

# 1. set the value of the matrix

# 2. get the value of the matrix

# 3. set the value of the inverse

# 4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(n){
    x <<- n
    m <<- NULL
  }
  get <- function()x
  setInverse <- function(inverse) m <<- inverse
  getInverse <- function() m 
  list(set = set, get = get, 
       setInverse = setInverse, 
       getInverse = getInverse)

}


# The following function calculates the inverse of the special "matrix" created
# with the above function. However, it first checks to see if the inverse has
# already been calculated. If so, it gets the inverse from the cache and skips the
# computation. Otherwise, it calculates the inverse of the data and sets the value
# of the inverse in the cache via the SetInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  m <- x$getInverse()
  if(!is.null(m)){
    message("getting cached data")
    return(m)
  }
  mat <- x$get()
  m <- solve(mat,...)
  x$setInverse(m)
  m
}
