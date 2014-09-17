makeCacheMatrix store a matrix and its inverse, 
returning a list with 4 functions. $get() returns the matrix, $set()
sets a new matrix, $getinverse() returns the inverse of the matrix and
$setinverse sets the inverse of the matrix. If the inverse had already 
been calculated, $getinverse() returns its value and returns NULL when
it hasn't been calculated. Take note that seting a new matrix also sets 
the inverse to NULL.

makeCacheMatrix <- function(x = numeric()){
	i <- NULL
	set <- function(y = numeric()){
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setinverse <- function(inverse) i <<- inverse
	getinverse <- function() i
	list(set = set, get = get, 
		setinverse = setinverse,
		getinverse = getinverse) 
}

cacheSolve returns the inverse of a matrix stored in the list return by
the previous function. If the inverse had already been calculated, it 
just gets the value. Otherwise, it calculates the inverse and sets its value
in the list for future use.

cacheSolve <- function(x, ...){
	i <- x$getinverse()
	if(is.null(i)){
		message("Calculating inverse...")
		i <- solve(x$get())
		x$setinverse(i)
	}
	else message("Retrieving data from cache...")
	i
}
