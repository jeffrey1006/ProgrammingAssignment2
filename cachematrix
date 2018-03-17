## makeCacheMatrix provides a list containing function to
## set a matrix, get a matrix, set inverse of a matrix and get the inverse of a matrix.

makeCacheMatrix <- function (x =matrix()){
      m <- NULL
      set <- function(y){
            x <<- y
            m <<- NULL
      }
      get <- function() x
      setinv <- function(inv) m<<- inv
      getinv <- function() m
      list(set = set, get = get, setinv = setinv, getinv= getinv)
}


## cacheSolve provides the inverse of a martix. Firstly it checks
## if the inverse is already calculated then it gets the inverse from cache.
## Otherwise, it provides the inverse after calulating it.

cacheSolve <- function(x, ...){
      m <- x$getinv()
      if(!is.null(m)){
            message("Getting cache data")
            return(m)
      }
      data <- x$get()
      m <- solve(data, ...)
      x$setinv(m)
      m
}
