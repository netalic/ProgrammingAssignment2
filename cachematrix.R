## We are creating a matrix and then getting its inverse. This is helpful beacuse inversing a matrix is a costly computation and once if we calculated then we don't have to compute it again. 

## This function will create a special matrix that can cache its inverse

makeCacheMatrix <- function(X = matix()){
    M <-NULL
    set <- function(Y){
        X <<- Y
        M <<- NULL
    }
    get <- function()X
    setinverse <- function(Z) {
      M <<- solve(Z)
      M
    } 
    getinverse <- function() M
    list(set=set, get=get, setinverse = setinverse, getinverse = getinverse)
}

## This function will return the inverse of a matric is it already computed otherwise it will compute in this function.

cacheSolve <- function(X, ...) {
    M <- X$getinverse
    if(!is.null(M)) {
        message("getting cached data")
        return(M)
    }
    data <- X$get()
    M <- solve(X)
    X$setinverse(M)
    M
}