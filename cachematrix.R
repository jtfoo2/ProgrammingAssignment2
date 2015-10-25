## makeCacheMatrix and cacheSolve allow for the previously calculated inverse of a matrix to be cached and 
## returned without reevaluating if immediately requested again.

## makeCacheMatrix creates a list of functions that would set and return the inverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setcache <- function(inverse) inv <<- inverse
	getcache <- function() i
	list(set = set, get = get,
		setcache = setcache,
		getcache = getcache)
}


## cacheSolve checks for a cached inverse of matrix, and calculates and saves it if not previously set 

cacheSolve <- function(x, ...) {
	i <- x$getcache()
	if(!is.null(i)) {
		message("getting cached data")
			return (i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setcache(i)
	i ## Return a matrix that is the inverse of 'x'
}
