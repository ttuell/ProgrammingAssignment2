## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

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


## Write a short comment describing this function

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
