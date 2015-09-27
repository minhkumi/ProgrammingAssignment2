## The function makeCacheMatrix creates a special "vector", 
## which is really a list containing a function to
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inversed matrix
## 4. get the value of the inversed matrix

makeCacheMatrix <- function(x = matrix()) {
	m <- NULL
	setmatrix <- function(y) {
		x <<- y
		m <<- NULL
	}
	getmatrix <- function() x
	setinverse <- function(inversematrix) m <<- inversematrix
	getinverse <- function() m
	list(setmatrix = setmatrix, 
		getmatrix = getmatrix,
		setinverse = setinverse,
		getinverse = getinverse)

}


## The function calculates the inversed matrix of the special "vector" created with the above function. 
## However, it first checks 2 things :
	## 1. if the inversed matrix has already been calculated
	## 2. if the original matrix has been changed
## If so, it gets the inversed matrix from the cache and skips the computation. 
## Otherwise, it calculates the inversed matrix of the data 
## and sets the value of the inversed matrix (via the setmean function) 
## and sets the value of the original matrix (using the << operator ) in the cache.  

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        if(!is.null(m) && identical(savematrix, x$getmatrix())) {
        ##if(!is.null(m)) {
        	message("getting cache data")
        	return(m)
        }
        data <- x$getmatrix()
        savematrix <<- data
        m <- solve(data, ...)
        x$setinverse(m)
        m 
}





