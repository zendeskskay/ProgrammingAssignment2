makeCacheMatrix <- function(x = matrix()) {  # this function defined the four functions to be used in cacheSolve 
  m <- NULL                                  # initialize m to Null
  set <- function(y) {                       # define set function
    x <<- y
    m <<- NULL
  }
  get <- function() x                        # define get function
  setinverse <- function(mean) m <<- mean    # define setinverse function
  getinverse <- function() m                 # define getinverse function
  list(set = set, get = get,
       setinverse = setinverse,
       getinverse = getinverse)
}

cacheSolve <- function(x, ...) {
  m <- x$getinverse()                 # get inverse of x from parent env. & set m
  if(!is.null(m)) {                   # if m is not null (i.e. already calculated)
    message("getting cached data")
    return(m)                         # return m from cache
  }
                                      # if m was null, skip to here and run function for inverse
  data <- x$get()                     # pass x into the variable data
  m <- solve(data, ...)               # solve data to get the inverse of x
  x$setinverse(m)                     # pass the result into m
  m                                   # print m
}
