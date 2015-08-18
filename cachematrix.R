## Put comments here that give an overall description of what your
## functions do


## Create a special 'matrix' object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) m <<- inverse
	getinverse <- function() m
	list (set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## Get the inverse of the matrix. First, check to see if the
## inverse is cached. If it is, return that. Otherwise, calculate
## the inverse, cache it, and return it.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	m <- x$getinverse()
    if(!is.null(m)) {
        message("getting cached data")
        return(m)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
