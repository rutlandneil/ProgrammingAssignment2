## This pair of functions take a matrix as an argument, calculate its inverse and store it
##so that it can be retrieved later without the need to be recalculated.


##makeCacheMatrix takes as its arguement a matrix and creates a list holding the
##functions to retrieve or change the original matrix and calculate or store its inverse
makeCacheMatrix <- function(x = matrix()) {
      m <- NULL
      #Allows you to change the matrix and reset its cache value to NULL
      set <- function(y) {
            x <<- y
            m <<- NULL
      }
      #gets the base matrix
      get <- function() x
      #updates cache value to inverse matrix calculated in cacheMean
      setInverse <- function(solve) m <<- solve
      #returns cache value or NULL if not yet cached
      getInverse <- function() m
      list(get = get,
           setInverse = setInverse,
           getInverse = getInverse
      )
}

##cacheSolve checks if a matrix's inverse has already been cached and either returns 
##the cached value or calculates it and stores the value.
cacheSolve <- function(x, ...) {
      #checks if the inverse matrix is in the cache and returns it if it is.
      m <- x$getInverse()
      if(!is.null(m)) {
            message("getting cached data")
            return(m)
      }
      #If the above does not run then create, save and return the inverse matrix
      data <- x$get()
      m <- solve(data)
      x$setInverse(m)
      m
}