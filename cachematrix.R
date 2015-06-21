##  cacheMatrix.R

##  Create a pair of functions that create the inverse of matrix
    ##  caches the inverse of the matrix
    ##  get the inverse from cache if the matrix is already cached
    ##  calculate the inverse of matrix if its not available in the cache 

##  Usage
    ##  m <- matrix(c(1:4),2,2)
    ##  cacheMatrix <- makeCacheMatrix(m)
    ##  cacheSolve(cacheMatrix)

##  Creates a special matrix object that 
    ##  verifies if the inverse is cached
    ##  get the cached inverse
    ##  caches inverse
makeCacheMatrix <- function(x = matrix()) {
        c_Inverse <- NULL
        set <- function(y) {
            x <<- y
            c_Inverse <<- NULL
        }
        get <- function() x
        setInverse <- function(inverse) c_Inverse <<- inverse
        getInverse <- function() c_Inverse
        list(set = set, get = get,
            setInverse = setInverse,
            getInverse = getInverse)
    }

## Computes inverse of the created matrix
cacheSolve <- function(x) {
        ## Return a matrix that is the inverse of 'x'
        m_inv <- x$getInverse()
        if(!is.null(m_inv)) {
            message("getting cached data")
            return(m_inv)
        }
        data <- x$get()
        m_inv <- solve(data)
        x$setInverse(m_inv)
        m_inv
    }
