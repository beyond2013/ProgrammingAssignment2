## This R script contains two functions makeCacheMatrix to create
## a special cached matrix, and cacheSolve to compute inverse of 
## the matrix if needed.

##makeCacheMatrix contains definition of 2 pair
## of functions; set and get are used to assign
## and retrieve a matrix, whereas setInverse, and
## getInverse assign value to xinv which is
## inverse of the matrix x

makeCacheMatrix <- function(x = matrix()) {
        xinv <- NULL
        set <- function(y){
                x <<- y
                xinv <<- NULL
        }
        get <- function() x
        setInverse <- function(inv) xinv <<- inv
        getInverse <- function () xinv
        
        list(set=set, get=get, setInverse = setInverse, getInverse = getInverse)
}


## cacheSolve computes inverse of matrix x (assumes x to be invertible)
##if it has not already been computed  otherwise it retrieves the
##inverse by calling getInverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        xinv <- x$getInverse()
        if(!is.null(xinv)){
                message("getting cached inverse")
                return(xinv)
        }
        data <- x$get()
        xinv <- solve(data)
        x$setInverse(xinv)
        xinv
}
