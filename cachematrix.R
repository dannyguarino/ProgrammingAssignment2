## following two functions are used to calculate and cache the inverse of a matrix 
## the 1st time cacheSolve is called.  Any subseqent call will not recalculate
## the inverse, instead it will return the cached value

## makeCacheMatrix creates a list containing functions to
## set and get a MATRIX
## set and get the INVERSE of a matrix

makeCacheMatrix <- function(x = matrix()) {
        inversematrix <- NULL
        set <- function(y) {
                x <<- y
                inversematrix <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) inversematrix <<- inverse
        getinverse <- function() inversematrix
        list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## cacheSolve returns the INVERSE of the given MATRIX
## the 1st time a matrix is passed it computes and stores the inverse
## subsequent passes of the same matrix, returns the cached value

cacheSolve <- function(x, ...) {
        inversematrix <- x$getinverse()
        if(!is.null(inversematrix)) {
                message("getting cached data.")
        }
        else {
                data <- x$get()
                inversematrix <- solve(data)
                x$setinverse(inversematrix)
        }
        inversematrix
}
