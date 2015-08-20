
## makeCacheMatrix: Saves the matrix and its cached inverse
## cacheSolve: checks for the cache, if available returns the 
##             inverse else computes the inverse and saves it in cache and returns the inverse

## Stores a matrix and cached value of its inverse

makeCacheMatrix <- function(x = matrix()) {
  
  m <-NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  } 
          
  get <- function() x
  
  setInv <- function(solve)  m <<- solve 
  getInv <- function() m
  
  list(set = set, get = get, setInv = setInv, getInv = getInv)
}


## Returns the inverse of matrix. Uses Cache. Computes only if the matrix values changes

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
   m <- x$getInv() 
   if(!is.null(m)) { 
           message("getting cached data") 
           return(m) 
   } 
  
   data <- x$get() 
   m <- solve(data) 
   x$setInv(m) 
   m 
  
}
