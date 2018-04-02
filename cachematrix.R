## The functions cache the inverse of a matrix.

## The makeCacheMatrix function creates a special matrix that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
       m <- NULL
       set <- function(y) {
              x <<- y
              m <<- NULL
       }
       get <- function() x
       setinv <- function(inv) m <<- inv
       getinv <- function() m
       list(set = set, get = get,
            setinv = setinv,
            getinv = getinv)
}

## The cacheSolve function computes the inverse of the special matrix returned by
## makeCacheMatrix above. 

cacheSolve <- function(u, ...) {
       v <- u$getinv()
       if(!is.null(v)) {
              message("getting cached data")
              return(v)
       }
       data <- u$get()
       v <- solve(data, ...)
       u$setinv(v)
       v
}
