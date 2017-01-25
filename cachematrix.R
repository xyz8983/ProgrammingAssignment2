## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## makeCacheMatrix defined the 4 functions of the matrix which has set, get, setmatrix, getmatrix function
## to enable the cache inversed matrix behavior
makeCacheMatrix <- function(x = matrix()) {
	inversed_matrix <- NULL
	set <- function(y){
		x <<- y
		inversed_matrix <- NULL
	}
	get <- function() x
	
	setmatrix <- function(i) inversed_matrix <<- i
	getmatrix <- function() inversed_matrix
	
	list(set = set, get=get, setmatrix=setmatrix, getmatrix=getmatrix)
}


## Write a short comment describing this function
##get the cached inversed matrix for x, if no cached one, calculate a new one then cache it for future usage
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	inversed_matrix <- x$getmatrix()
	
	##check if the cache value is empty or not
	if(!is.null(inversed_matrix)){
		message("getting cached data")
		return(inversed_matrix)
	}
	
	##calculate the inversed matrix
	data <- x$get()
	inversed_matrix <- solve(data)
	x$setmatrix(inversed_matrix)
	inversed_matrix
	
	
}