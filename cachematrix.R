## These two functions use a cacheing system to compute the inverse of a matrix

## This function creates a special "matrix" object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {

	# for the first call, initializes the inverse matrix to NULL
	inverse <- NULL

	# sets a matrix given the function's input, gets rid of cached inverse
	set <- function(y) {
		x <<- y
		inverse <<- NULL
	}

	# returns matrix from part 1
	get <- function() x

	# sets value of inverse of matrix from part 1
	setinverse <- function(solve) inverse <<- solve

	# returns inverse of matrix from part 1
	getinverse <- function() inverse

	# returns list of functions
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)

}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated (and 
## the matrix has not changed), then the cachesolve should retrieve the inverse
## from the cache. If the matrix has been changed, it will compute the inverse
## of the new matrix

cacheSolve <- function(x=matrix(), ...) {

	# gives a matrix that is the inverse of 'x'
	inverse <- x$getinverse()
	if(!is.null(inverse)) {
		message("getting cached data")
		return(inverse)
	}

	# get the initial matrix
	data <- x$get()

	# compute the inverse matrix
	inverse <- solve(data, ...)

	# cache the inverse matrix in the list from the initial function
	x$setinverse(inverse)

	# return the inverse
	inverse

}


#### test script
size <- 10
mymatrix <- matrix(rnorm(size^2), nrow=size, ncol=size)
mymatrix.inverse <- solve(mymatrix)

special.matrix   <- makeCacheMatrix(mymatrix)
special.solved.1 <- cacheSolve(special.matrix)
special.solved.2 <- cacheSolve(special.matrix)
identical(mymatrix.inverse, special.solved.1) & identical(mymatrix.inverse, special.solved.2)