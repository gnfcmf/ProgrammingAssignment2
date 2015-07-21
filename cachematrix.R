## This function stores a list of four functions: 'set' which creates a matrix x;
## 'get' which returns the matrix x sored; 'setinverse' which creates the inverse
## of the matrix x; 'getinverse' which returns the inverse of matrix x

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setInv <- function(Inv) i <<- Inv
        getInv <- function() i
        list(set = set, get = get,
             setInv = setInv,
             getInv = getInv)
}


## This function verifies if the inverse of matrix x is stored; return its value
## if stored, calculate the inverse of matrix x if value not stored

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