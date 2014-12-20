# The makeCacheMatrix functions creates a special matrix, which saves a normal matrix and the inverse of that matrix.
# The function return a list with the getters and setters for the matrix and inverse.
makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        
        get <- function() x
        
        setInverse <- function(inverse) i <<- inverse
        
        getInverse <- function() i
        
        list(set        = set,
             get        = get,
             setInverse = setInverse,
             getInverse = getInverse)
}

# The cacheSolve function creates the inverse of the matrix saved in the makeCacheMatrix function.
# The function returns the saved inverse, if there is already one, or returns the newly created one.
cacheSolve <- function(x, ...) {
        i <- x$getInverse()
        
        if (!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        
        data <- x$get()
        
        # solve() creates the inverse of a given matrix
        i <- solve(data)
        
        x$setInverse(i)
        
        i
}
