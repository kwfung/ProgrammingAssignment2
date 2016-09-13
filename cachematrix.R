## The first function "makeCacheMatrix" creates a special matrix object that is really a list containing a function to 
##1) set the value of the vector, 
##2)get the value of the vector, 
##3)set the value of the inverse, 4)get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {

		s <- NULL
		set <- function(y) {
			x <<- y 
			s <<- NULL 
		}

		get <- function() x
		setinverse <- function(solve) s <<- solve
		getinverse <- function() s
		list(set = set, get = get, 
		setinverse = setinverse, 
		getinverse = getinverse) 

}


## The function "CacheSolve" returns the inverse of x, the vector created from the above function. 
##However, it first looks to see if the inverse has already been calculated. 
##If so, it will get the inverse value from the cache and skip the calculation. 
##If not, it will calculate the inverse of the data and set the value of the inverse in the cache using the setinverse. 

cacheSolve <- function(x, ...) {
	s <- x$getinverse() 
	if(!is.null(m)) {
		message("getting cached data") 
		return(s) 
	}
	data <- x$get()
	s <- solve(data, ...) 
	x$setinverse(s)
	s
        
}
