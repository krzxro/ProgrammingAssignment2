

## This function creates a matrix that can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL 

        ## Function to set the matrix
        set <- function(y) { 
                x <<- y
                inv <<- NULL ## Establish inverse as null again since function has changed
        }

        ## Function to get the matrix
        get <- function() x 
        
        ## Function to set the inverse of the matrix
        setInverse <- function(inverse) inv <<- inverse

        ## Function to get the inverse of the matrix
        getInverse <- function()inv

        ## Returns a list of all functions 
        list(set = set, get = get, setInverse = setInverse, getInverse = getInverse)
}

## This function completes the inverse of the matrix returned by the function above.

cacheSolve <- function(x, ...) {
        inv <- x$getInverse() # Get the cached inverse

        # If the inverse is already cached, return it
        if (!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        # Otherwise, compute the inverse 
        data <- x$get() # Get the matrix
        inv <- solve(data, ...) # Compute the inverse
        x$setInverse(inv) # Cache the inverse

        inv # Return the inverse

}
