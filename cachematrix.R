


## this function makeCacheMatrix is the constructor for a cached matrix. 
## it equips the matrix with the getter and setter for the inverse and the matrix itsself.

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                  ## in the beginning no inverse is calculated
        set <- function(y) {
                x <<- y            ## a new matrix is stored in x
                i <<- NULL         ## a new matrix has a new inverse, which is not yet calculated 
        }
        get <- function() x        
        setInverse <- function(inverse) i <<- inverse 
        getInverse <- function() i
        list(set = set, get = get,
             setInverse = setInverse,
             getInverse = getInverse)
}


## This function returns the inverse. If the inverse is already calculated, it uses the cached inverse, if not the inverse is 
## calculated again.

cacheSolve <- function(x) {
        i <- x$getInverse()     ## x has to be an object created by the above function makeCacheMatrix, thus getInverse() exists
        if(!is.null(i)) {       ## if the inverse has already been calculated
                message("getting cached data")
                return(i)       ## the inverse is returned, return also terminates the function
        }
        data <- x$get()         ##get the matrix ittself 
        i <- solve(data)        ##calculate the inverse
        x$setInverse(i)         ##set the inverse 
        i                       ## return the inverse
}
