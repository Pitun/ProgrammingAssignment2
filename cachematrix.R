## This program  creates a special matrix
## then makes cache matrix inverse 
## it will check wether the cached matrix inverse done or not
## if it done, then retrieve the inverse from the cache
## else do the computation

## makeCacheMatrix: This function creates a special "matrix" object
## that can cache its inverse.
makeCacheMatrix <- function(matrx.param = matrix()){
        matrx.inv <- NULL
        set <- function(matrx.elem){
                matrx.param <<- (matrx.elem)
                matrx.inv <<- NULL
        }
        get <- function()matrx.param
        setinverse <- function(inverse) matrx.inv <<- inverse
        getinverse <- function() matrx.inv
        list( set = set, 
              get = get, 
              setinverse = setinverse,
              getinverse = getinverse)
}


## cacheSolve: This function computes the inverse of the
## special "matrix" returned by makeCacheMatrix above.
## If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(matrx.special, ...){
        matrx.inv <- matrx.special$getinverse()
        if(!is.null(matrx.inv)){
                message("Getting cached inverse of matrix")
                return(matrx.inv)
        }
        data <- matrx.special$get()
        matrx.inv <- solve(data, ...)
        matrx.special$setinverse(matrx.inv)
        matrx.inv
        
}

##Testing stage
matrx.tst <- matrix( rnorm(100),10,10 )
matrx.elem <- makeCacheMatrix(matrx.tst)
matrx.elem$get()
matrx.elem$getinverse()
cacheSolve(matrx.elem)
cacheSolve(matrx.elem)
matrx.elem$getinverse()

