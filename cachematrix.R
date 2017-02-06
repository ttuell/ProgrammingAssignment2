## These 2 functions work in tandem to accept a matrix then calculate its inverse


## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
	mINV <- NULL
	set <- function(y){
		x<<-y
		mINV<<-NULL
	}
	get <- function()x
	setINV<- function(inverse) {mINV<<-inverse}
	getINV<- function() mINV
	list(set=set,
		 get=get,
		 setINV=setINV,
		 getINV=getINV)
}


## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
##If the inverse has already been calculated (and the matrix has not changed), 
##then the cachesolve should retrieve the inverse from the cache

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        mINV <- x$getINV()
        if(!is.null(mINV)){
        	message("getting cached data"
        	return(mINV))
        }
        data<- x$get()
        mINV<-inverse(data, ...)
        x$setINV(mINV)
        mINV
}
