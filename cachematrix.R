## A pair of functions that compute the inverse of 
## a matrix and cache it to avoid the cost of
## computing it each time it is required.

## makeCacheMatrix creates a special 'matrix' object
## that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    # The cached inverse
    i <- NULL

    # Set the value of the matrix
    set <- function(y) {
               x <<- y
               i <<- NULL
    }

    # Get the value of the matrix
    get <- function() { x }

    # Set the value of the inverse
    setinverse <- function(inverse) {
                      i <<- inverse
                  }

    # Get the value of the inverse
    getinverse <- function() { i }

    # Make get and set functions available
    list ( set = set,
           get = get,
           setinverse = setinverse,
           getinverse = getinverse )

}


## cacheSolve computes and returns the inverse of the special
## 'matrix' returned by makeCacheMatrix. If the 
## inverse has already been calculated and cached 
## it will return the cached inverse.

cacheSolve <- function(x, ...) {
    
    # Get the cached inverse of x
    i <- x$getinverse()
    
    # If there there is a cached value then return it
    if(!is.null(i)) {
        message("getting cached data")
        return(i)
    }
    
    # No cached inverse so we calculate it
    data <- x$get()
    
    i <- solve(data, ...)
    
    # Cache the calculated inverse
    x$setinverse(i)
    
    # Return the inverse
    i
}
