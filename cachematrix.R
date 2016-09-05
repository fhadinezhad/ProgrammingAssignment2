
# this function creates a special matrix object that can cache its inverse.
#
# function set : # set function to set the value of matrix
# function get : # get to return the value of matrix
# function setinverse : # set the inverse of the matrix
# function getinverse : # get the inverse of the matrix which is stored in parameter m

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL

        set <- function(y) {
                x <<- y
                m <<- matrix()
        }
	
        get <- function() x

        setinverse <- function(inverse) m <<- inverse

        getinverse <- function() m
        list(set = set, get = get,
             setinverse = setinverse,
             getinverse = getinverse)
}

 # This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
        m <- x$getinverse()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)# inverse the matrix
        x$setinverse(m)
        m # return the last statement which is the inverse matrix
}
