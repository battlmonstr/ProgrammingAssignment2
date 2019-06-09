## A pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	x.inverse <- NULL
	set <- function(y) {
		x <<- y
		x.inverse <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) x.inverse <<- inverse
	getInverse <- function() x.inverse
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" returned by `makeCacheMatrix` above.
## If the inverse has already been calculated (and the matrix has not changed), then `cacheSolve` retrieves the inverse from the cache.

cacheSolve <- function(cx, ...) {
	if (is.null(cx$getInverse())) {
		message("calculating the inverse and memoizing")
		x <- cx$get()
		inverse <- solve(x, ...)
		cx$setInverse(inverse)
	}
	cx$getInverse()
}
