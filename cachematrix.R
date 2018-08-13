## This function is to calculate the inverse
## of a square matrix

## This function create a matrix with 4 functions

makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL #Initiate the inverse of the matrix
        
        set <- function(y){
                x <<- y
                inv <<- NULL
        }
        get <- function(){x}
        setinv <- function(inverse){inv <<- inverse}
        getinv <- function(){inv}
        
        list(set = set,
             get = get,
             setinv = setinv,
             getinv = getinv)
}


## This function calculates the inverse of the matrix

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}
