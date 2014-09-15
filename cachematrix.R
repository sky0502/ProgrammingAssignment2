##cache the inverse of a matrix

##This function creats a special matrix that can cache its inverse

makeCacheMatrix<-function(x=matrix()){
	inv<-NULL	
#set the value of the matrix	
	set<-function(y){
		x<<-y
		inv<<-NULL
	}
#get the value of matrix
	get<-function() x
#set the value of the inverse matrix
	setinverse<-function(solve) inv<<-solve
#get the value of the inverse matrix
	getinverse<-function() inv
	list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)	
}

##This function computes the inverse of the special matrix returned by makeCacheMatrix. 

cacheSolve<-function(x, ...){
	inv<-x$getinverse()
#If the inverse matrix is already calculated
	if(!is.null(inv)){
		message("getting cached data")
		return(inv)
	}
	
#calculate the inverse matrix and sets the value of the mean in the cache
	matrix<-x$get()
	inv<-solve(matrix)
	x$setinverse(inv)
	inv
}

#> m<-makeVector(1:10)
#> m$get()
# [1]  1  2  3  4  5  6  7  8  9 10
#> cachemean(m)
#[1] 5.5
#> cachemean(m)
#getting cached data
#[1] 5.5

#x<-matrix(1:4,nrow=2)
#n<-makeCacheMatrix(x)
#n$get()
#cacheSolve(n)
#cacheSolve(n)