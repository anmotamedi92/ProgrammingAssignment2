
## makeCacheMatrix function creates and stores a special matrix, 
## and the cacheSolve function finds the inverse of the matrix
## created by the makeCacheMatrix function. It will also determine 
## whether the inverse has been calculated or not (by the if command
## in the cacheSolve function)

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y 
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, 
       setinv=setinv, 
       getinv=getinv)
  
}


cacheSolve <- function(x, ...) {
        inv <- x$getinv()
        
        if(!is.null(inv)){
         message("getting cached data")
         return(inv)
        }
        
        mat.data = x$get()
        inv <- solve(mat.data, ...)
        x$setinv(inv)
        inv
}
