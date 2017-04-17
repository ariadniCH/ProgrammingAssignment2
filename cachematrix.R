## The first function makeCacheMatrix essentially caches both the original and the inverted matrix 
# wheareas the second function cacheSolve checks if the matrix's inverse has been cached and if not it calculates its inverse

## makeCacheMatrix caches both and the original and the inverted matrix 

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) m <<- inverse
  getinverse <- function() m
  list(set = set,
       get = get,
       setinverse = setinverse ,
       getinverse = getinverse)
  
}


## cacheSolve checks if the inverted matrix has been cached and if yes it returns it but if not it calculates it 

cacheSolve <- function(x, ...) {
  m <- x$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- x$get()
  m <- solve(data)
  x$setinverse(m)
  m
        ## Return a matrix that is the inverse of 'x'
}
