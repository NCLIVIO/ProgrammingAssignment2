
##-------Assignment3: Caching the Inverse of a Matrix-----------

# Create a matrix object than can cache its inverse

makeCacheMatrix <- function(x=matrix()) {
## Creates a list of functions that
## can cache the inverse of a matrix.
    i <- NULL
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    setInverse <- function(inverse) i <<-inverse
    getInverse <- function() i
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
 
}


## Solve a inverse matrix and check the cache

cacheSolve <- function(x, ...) {
    	## Computes the inverse of the matrix returned
	## by makeCacheMatrix(), unless the inverse has
	## already been calculated, in which case
	## it retrieves it from the cache.
    m <- x$getInverse()
    if ( ! is.null(m)) {
        print("getting cached data")
        return(m)
    }
    m <- solve(x$get())
    x$setInverse(m)
    m
}
