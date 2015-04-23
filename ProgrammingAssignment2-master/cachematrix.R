## Cache the inverse of a matrix to improve execution time if it is repeated


## First part - function makeCacheMatrix stores a Matrix as m for future function call

makeCacheMatrix <- function(x = matrix()) {
        m <- NULL
        set <- function(y) {
                x <<- y
                m <<- NULL
        }
                
        get <- function() x
        setinv <- function(solve) m <<- solve
        getinv <- function() m
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)

}

## Second part - recall the matrix and compute the inverse and store as m
## if the inverse matrix is called again - value is returned from cache

cacheSolve <- function(x, ...) {
         m <- x$getinv()
        if(!is.null(m)) {
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...)
        x$setinv(m)
        m

        ## Return a matrix that is the inverse of 'x'
}

#Just adding script to check that the function is working

n <- 1024
    mat <- matrix(rnorm(1:(n*n)), nrow=n, ncol=n)
    matCached <- makeCacheMatrix(mat)
    time1 <- system.time(matSolved1 <- cacheSolve(matCached))
    time2 <- system.time(matSolved2 <- cacheSolve(matCached))
        #Compare time difference
    print(time1["user.self"])
    print(time2["user.self"])
        #Check value
if (!identical(matSolved1, matSolved2))
        message("Cached version does not match solved version")
