## Below functions exhibit how a instead of calling Solve function 
## always we can cache the function results

## Write a short comment describing this function
## this function wraps the matrix to a "Special Matrix" 
##(which is actally a list of functions)along with that it 
## creates a list of special functions to Set-get the matrix
## and also set and get functions for inverse of Matrix

makeCacheMatrix <- function(x = matrix()) {

	## Initialize the invese of Matrix
	invx<- NULL
	## define function to set the value of input matrix
	set<-function(invy){
	## speciall operator <<- to set the value in paremt function scope
		x<<-invy
		invx<<-NULL
	}
	## define function to return the input matrix	
	get<-function() x
	
	## define function to call solve function to calculate the inverse of matrix
	setinv<- function(solve) invx<<- solve
	## define function to resurn the Inverse cached
	getinv<- function() invx

	## returns the list of special functions created to set-get the input matrix
	## and set-get the cached inverse 
	list(set=set,get=get,setinv=setinv,getinv=getinv)

}


## This function calculates the inverse of matrix is not already cached.
## this takes input as "special matrix" created in above function

cacheSolve <- function(x, ...) {
      ## Get the inverse cached and check if it is cached already
	invx<-x$getinv()
	## if cached
	if(!is.null(invx)){
		message("getting cached data")
		## return cached inverse and exit funtion
		return(invx)
	}
	## if not cached
	## Get input Matrix 
	data<-x$get()
	## Calculate the inverse
	invx<-solve(data,...)
	## Cache Inverse for use for next time function is called
	x$setinv(invx)
	

	## return inverese
	invx


}
