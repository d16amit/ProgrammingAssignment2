## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {

	invx<- NULL
	set<-function(invy){
		x<<-invy
		invx<<-NULL
	}
	get<-function() x
	
	setinv<- function(solve) invx<<- solve
	getinv<- function() invx

	list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	invx<-x$getinv()
	if(!is.null(invx)){
		message("getting cached data")
		return(invx)
	}
	data<-x$get()
	invx<-solve(data,...)
	x$setinv(invx)
	
	invx


}
