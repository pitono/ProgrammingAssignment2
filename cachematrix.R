## Put comments here that give an overall description of what your
## functions do

## This function creates a special "matrix", which is really a list containing 
## a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInverse <- function(solve) i <<- solve
	getInverse <- function() i
	list(set = set, get = get, 
		setInverse = setInverse, 
		getInverse = getInverse)
}


## This function calculate the inverse of the special "matrix" created with 
## makeCacheMatrix function. However, it first check to see if the inverse
## of the matrix has already been calculated.
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- x$getInverse()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInverse(i)
	i
}
