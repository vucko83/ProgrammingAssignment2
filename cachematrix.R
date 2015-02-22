## This functions makes a special matrix that can be cache its inverse
## Computing the inverse of a square matrix can be done with the solve function

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
    mInv <- NULL
    set<-function(y){
        x<<-y
        mInv<<-NULL
    }
    get<-function()x
    setInvMatrix<-function(solve) mInv<<-solve
    getInvMatrix<-function()mInv
    list(set=set, get=get, setInvMatrix=setInvMatrix, getInvMatrix=getInvMatrix)
    
    
}


## This is function for computing the inverse of special matrix that is created
## in previous function makeCacheMatrix
cacheSolve <- function(x, ...) {
    mInv<-x$getInvMatrix()
    if(!is.null(mInv)){
        message("getting cached data")
        return(mInv)
    }
    data<-x$get()
    mInv<-solve(data,...)
    x$setInvMatrix(mInv)
    mInv
    ## Return a matrix that is the inverse of 'x'
}
