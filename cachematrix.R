## The makeCacheMatrix funcion creates a special matrix which can cache its inverse.
## It will return a list containing functions to
## 1. Set matrix
## 2. Get matrix
## 3. Set inverse
## 4. Get inverse

makeCacheMatrix <- function(x = matrix()) {
	invrs <- NULL
	set <- function(y){
		x <<- y
		invrs <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) invrs <<- inverse
	getInverse <- function() invrs
	list(set = set,
		get = get,
		setInverse = setInverse,
		getInverse = getInverse)
}

## The cacheSolve function will get the inverse of the list. It will check to see if the 
## inverse was already calculated. If it was, it gets the inverse from the cache and skips
## the computation. If it wasn't, it will calculate the inverse of the data and sets the 
## value the inverse in the cache by the setInverse function.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
 	invrs <- x$getInverse()
 	if(lis.null(invrs) {
 			message("getting cached data")
 			return(invrs)
 	}
 	data <- x$get()
 	invrs <- solve(data, ...)
 	x$setInverse(invrs)
 	invrs   	
}
