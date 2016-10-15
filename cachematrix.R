## These function creates a special "matrix" object 
## that computes and  cache its inverse

## this function returns a list to functions to 
## set/get a matirx and its inverse

makeCacheMatrix <- function(x = matrix()) {

    inverse <- NULL
    set <- function(y){
        x <<- y
        inverse <<- NULL
    }
    get <- function(){
        x
    }
    setInverse <- function(inverseMatrix){
        inverse <<- inverseMatrix
    }
    getInverse <- function(){
        inverse
    }
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
}


## This function returns inverse of a matix from
## cache
## if cache doesn't exist, then, computes inverse and 
## returns the inverse

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inverse <- x$getInverse()
        if(!is.null(inverse)){
            message("getting cached inverse matrix")
            return(inverse)
        }
        
        dmat <- x$get()
        inverse <- solve(dmat)
        x$setInverse(inverse)
        inverse
}
