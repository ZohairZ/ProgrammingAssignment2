## makeCacheMatrix: 
## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function( m = matrix()) {
        n <- NULL
        
        ## Set Matrix
        
        set <- function(matrix) {
                m <<- matrix
                n <<- NULL
        }
        
        ## Get Matrix
        get <- function(){
                m
        }
        
        ## Set Inverse
        setinverse <- function(inverse) {
                n <<- inverse
        } 
        ## Get Inverse
        getinverse <- function(){
                n
        }
        ## List of Methods
        list(set=set, get=get, setinverse = setinverse, getinverse=getinverse)
}

## cacheSolve
## This function computes the inverse of the special "matrix" returned 
## by makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve should retrieve 
## the inverse from the cache.

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getinverse()
        
        if(!is.null(m)){
                message("getting cached data")
                return(m)
        }
        data <- x$get()
        
        ## Calculate Inverse
        m <- solve(data) %*% data
        
        x$setinverse(m)
        
        m
}
