## The following functions are used to create a special object, that stores a
## matrix and caches its inverse.

## The first function makeCacheMatrix creates a special matrix, which is really 
## list containing the function to: 
##1.  set the value of the matrix
##2.  get the value of the matrix
##3.  set the value of the inverse
##4.  get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function (y){
                x <<- y
                i <<- NULL
        }
        get <- function () x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list (set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## The second function calculates the inverse of the "special matrix" created by 
## the function above. It first checks, if the inverse has allready been 
##calculated, If so, it gets the inverse of the cache and skips the computation.
## Otherwise it calculates the inverse of the data and sets the value of the 
## inverse in the cache via the 'setinverse' function. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
           message("getting cached data")
                return(i)     
        }
         data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i 
}
        
                
