## This function creates a special matrix with a function that sets 
## the value of the matrix, gets the value of the vector, sets the value 
## of the inverse, and gets the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
        inverse <- NULL
        set <- function(y){
                x <<- y
                inverse <<- NULL
        }
        get <- function() x
        setinverse <- function(solvematrix) inverse <<- solvematrix
        getinverse <- function() inverse
        list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}

## This function calculates the inverse of the special matrix created 
## by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
        inverse <- x$getinverse()
        if(!is.null(inverse)){
                message("getting cached data")
                return(inverse)
        }
        data <- x$get()
        inverse <- solve(data)
        x$setinverse(inverse)
        inverse  
}