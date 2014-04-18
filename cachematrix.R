## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the inverstion of the matrix
## 4. get the inversion of the matrix

makeCacheMatrix <- function(x = matrix()) {	
		im <- NULL					## im: data comm. between makeCacheMatrix and cacheSolve
							
		set <- function(y) {
			x <<- y					# store y matrix into x
			im <<- NULL 			# clean inverse matrix
		}
		
		get <- function() x			# return x
		
		setinv <- function(i) im <<- i	
		
		getinv <- function() im
		
		list(set = set, get = get, setinv = setinv, getinv = getinv)
}


## Write a short comment describing this function
##
## if inv. matrix stored in im then return it, 
## else calculate inv. matrix and store it in im
## 
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        
        im <- x$getinv()			# try to get cached inv. matrix
        
        if(!is.null(im)) {
        	message("getting cached data")
        	return(im)
        }
        
        data <- x$get()				# if no cached data, then compute inv. matrix
        im <- solve(data, ...)
        x$setinv(im)
        im
}
