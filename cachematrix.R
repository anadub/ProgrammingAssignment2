## This function creates a special "matrix" object that can cache its inverse. 
## It is really a list containing a function to set the matrix, get the matrix, 
## set the inverse
## get the inverse

makeCacheMatrix <- function(x = matrix()) {
                 i <- NULL
                 set <- function(y) {
                                 x <<- y
                                 i <<- NULL
                         }
                 get <- function() x
                 setInv <- function(inv) i <<- inv
                 getInv <- function() i
                 list(set = set, get = get,
                        setInv = setInv,
                        getInv = getInv)
         }

## This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), 
## then the cachesolve retrieves the inverse from the cache.

cacheSolve <- function(x, ...) {
                i <- x$getInv()
                 if(!is.null(i)) {
                                 message("getting cached data")
                                 return(i)
                         }
                 data <- x$get()
                 i <- solve(data, ...)
                 x$setInv(i)
                 i
}

