##The twin functions together will ease the need for repetitive computation
## of inverse of a matrix , which is a tedious exercise. This is achieved by caching the 
## the inverse and retrieving it from cache memory.
## The first function is a special matrix which is a list comprising functions to ;
## 1. set the matrix 
## 2. Get the matrix
## 3. Set the matrix inverse 
## 4. Get the matrix inverse
makeCachematrix <- function(x = matrix()) {
    m <- NULL
    set <- function(y) {
    x <<- y
    m <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) m <<- inverse
    getinverse <- function() m
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}
  
## The following function computes the inverse of the matrix created by the 
## special function above. This is done by first checking if the inverse has 
## already been computed and stores and if so retrieving the inverse from the 
## cache skipping the tedious ccomputation process. If not available it computes 
## the inverse(using solve(x) function) and stores it in cache via the setinverse 
##function.

 cachesolve <- function(x, ...) {
   ## return a mtrix that is inverse of x
    m <- x$getinverse()
    if(!is.null(m)) {
      message("getting cached data")
      return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
    
  }
  



