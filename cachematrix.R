
## makeCacheMatrix: creates a special "matrix", which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the invers of the matrix
## 4. get the invers of the matrix

makeCacheMatrix <- function(x = matrix()) {
  inv<-NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  
  get <-function() x
  
  setinverse <- function(inverse) inv <<- inverse
  
  getinverse <- function() inv
  
  list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
    
}


## cacheSolve: computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), it retrieves 
## the inverse from the cache and skips the computation, otherwise, it calculates the inverse of
## the matrix and sets the value of inv in the cache for later use

## it assumes that the matrix x is always invertible and also a squre matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
  inv <-x$getinverse()
  if((!is.null(inv)) && (identical((x$get() %*% inv), diag(nrow(x$get()))))){
    message("getting cached restults")
    return(inv)
  }
  
  data <- x$get()
  
  inv <- solve(data)
  x$setinverse(inv)
  inv
}
 

