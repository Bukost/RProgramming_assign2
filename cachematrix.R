## functions used to create an object that stores a matrix 
## and caches its inverse
##------------------------------------------------------------------------


## function creates a special "matrix" object with auxiliary functions to:
##		set the values of the matrix			set
##		get the values of the matrix			get
##		set the value of the inverse matrix		setInv
##		get the value of the inverse matrix		getInv

makeCacheMatrix <- function(x = matrix()) {
		Inv<-NULL
		set<-function(y){
				x<<-y
				Inv<<-NULL
		}
		get<-function() x
		setInv<-function(IM) Inv<<-IM
		getInv<-function() Inv
		list(set=set,get=get,setInv=setInv,getInv=getInv)
}

##------------------------------------------------------------------------


## function for calculation of inverse matrix Inv
## function checkes, if the inverse matrix has been calculated	
## gets the inverse matrix from cache					(getInv)
## or calculates the inverse matrix and sets it in the cache	(setInv)

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
		Inv<-x$getInv()
		if (!is.null(Inv)){
			message("getting cached data")
			return(Inv)
		}
		data<-x$get()
		Inv<-solve(data)
		x$setInv(Inv)
		Inv
}
