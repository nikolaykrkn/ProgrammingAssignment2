## makeCacheMatrix function creates a list with functons
## that set the value of the matrix, get its value,
## set the value of inverse and get the value of inverse
## of the matrix.

## cacheSolve function computes the inverse of the
## special "matrix" returned by makeCacheMatrix.
## If the inverse has already been calculated (and the
## matrix has not changed), inverse from the cache
## is retrieved.


makeCacheMatrix <- function(x = matrix()) {
    ## creating i variable
    i <- NULL
        ## assigning y to x non-locally 
        set <- function(y) {
                x <<- y
                i <<- NULL
                
        }
        get <- function() x
        ## assigned matrix to "get" variable
        setinv <- function(solve) i <<- solve
        ## assigned solve variable value to i
        getinv <- function() i
        ## retrieving i value
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
             ## list with functions created
}


cacheSolve <- function(x, ...) {
    ## assigning "getinv" value to i in this function
    i <- x$getinv()
    ## checking if any value already exists in i.
    ## if yes, printing message, returning i value 
    ## and ending executin of the function
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        ## otherwise, subsetting x to run the next steps
        data <- x$get()
        i <- solve(data, ...)
        x$setinv(i)
        i
}
}
