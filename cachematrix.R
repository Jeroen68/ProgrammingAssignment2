## Put comments here that give an overall description of what your
## functions do

## makeCacheMatrix
## create a list of functions that set and get the value 
## of a the matrix, and set and get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	set <- function(y) {
		x <<- y
		m <<- NULL
	}
	get <- function() x
	setinverse <- function(solve) m <<- solve
	getinverse <- function() m
	list(set = set,
	     get = get,
	     setinverse = setinverse,
	     getinverse = getinverse) 
}

## cacheSolve
## calculates the inverse of the matrix created
## in makeCachematrix; it first checks to see if
## the inverse has already been calculated
## if so, it gets the mean from the cache and
## skips the computation. Otherwise, it calculates
## the inverse of the matrix and sets the value
## of the inverse in the cache via the setinverse
## function


cacheSolve <- function(x, ...) {
	m < x$getinverse()
	if(!is.null(m)) {
		message("getting cached data")
		return(m)
	}
	data <- x$get()
	m <- solve(data,...)
	x$setinverse(m)
	m
}
