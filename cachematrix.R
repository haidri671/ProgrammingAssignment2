## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL  # Initialize the inverse as NULL
        
        # Set the matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL  # Reset cached inverse when matrix changes
        }
        
        # Get the matrix
        get <- function() x
        
        # Set the cached inverse
        setinverse <- function(inverse) inv <<- inverse
        
        # Get the cached inverse
        getinverse <- function() inv
        
        # Return a list of 4 functions
        list(set = set,
             get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}



## This function computes the inverse of the special "matrix" returned by makeCacheMatrix.


cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
        
        # If inverse is already cached, return it
        if(!is.null(inv)) {
                message("getting cached inverse")
                return(inv)
        }
        
        # Otherwise compute the inverse
        mat <- x$get()
        inv <- solve(mat, ...)  # Compute inverse
        x$setinverse(inv)       # Cache the inverse
        inv                     # Return the inverse
}
