########################################################################################
## The makeCacheMatrix function creates a special object that stores a numeric matrix, it's
## inverse and get/set functions. The inverse is calculated by the cacheSolve function. The
## cacheSolve funtion first tries to read the cache, if the inverse is not available, only then
## it calculates it.
## BENEFIT of this code is that you will avoid repeated calculations of the resource-intensive
## inverse calculation.
########################################################################################

########################################################################################
## The makeCacheMatrix function creates a special object that stores a numeric 
## matrix and cache's its inverse
##  Input: matrix
##  Output: None
##  Methods: 	get() to get the matrix
##			set(y) to set the matrix (matrix is parameter)
##			getinverse() to get the inverse of the matrix
##			setinverse(inv)to set the inverse of the matrix (inverse is parameter)
##  Note: the cacheSolve function can be used to calculate the inverse of the matrix
########################################################################################
makeCacheMatrix <- function(x = matrix()) {
	inverse <- NULL			# If the inverse is not filled in, set it to NULL
	set <- function(y){ 		# If the matrix is updated (but not inverse) set it to NULL
		x <<- y
		inverse <<- NULL
	}
	get <- function() x 		# give back the matrix when requested
	getinverse <- function () inverse 			# give back the inverse when requested
	setinverse <- function(inv) inverse <<- inv	# set the inverse
	list(set = set, get = get,
		setinverse = setinverse,
		getinverse = getinverse)
}

########################################################################################
## The cacheSolve function calculates the inverse of a special numeric matrix object
## that was created by the makeCacheMatrix function. If the inverse was already cached it is
## re-used, otherwise it is calculated and cached
##  Input: matrix object created by makeCacheMatrix
##  Output: returns the inverse of the matrix
########################################################################################
cacheSolve <- function(x, ...) {
	inverse <- x$getinverse()		# Try to get the inverse from cache	
	if (!is.null(inverse)) {		# If inverse is retrieved from cache, return it
		message("getting cached data")
		return(inverse)
	}
	matrix <- x$get()				# If inverse was not in cache, calculate it. First get matrix
 	inverse <- solve(matrix)		# calculate inverse using solve function
    	x$setinverse(inverse)			# update the cache
	inverse					# return inverse
}
