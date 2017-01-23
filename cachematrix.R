## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
        # initiates a null inverse matrix
        inv <- NULL
        
        # set an inverse matrix
        set <- function(y) {
                x <<- y
                inv <<- NULL
        }
        
        # returned the inverse matrix
        get <- function() x

        # sets the inverse matrix
        setinverse <- function(inverse) inv <<- inverse
                
        # returns the inverse matrix
        getinverse <- function() inv
                
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}


## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinverse()
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv)
        }
        data <- inv$get()
        inv <- solve(data, ...)
        x$setinverse(inv)
        inv
}
