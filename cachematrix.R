## Creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(a = matrix()) {
        inv  <- NULL
        set  <- function(b){
                a <<- b
                inv <<- NULL
                }
        get  <- function() a
        setinverse  <- function(inverse) i  <<- inverse
        getinverse  <- function() inv
        list(set= set, get = get, 
             setinverse = setinverse, 
             getinverse = getinverse)
}

## Computes the inverse. If the inverse has already been calculated (and the matrix has not changed),
## then the cachesolve should retrieve the inverse from the cach

cacheSolve <- function(a, ...) {
        inv  <- a$getinverse()
        if (!is.null(inv)){
                message("getting cached data")
                return(i)
        }
        datos  <- a$get()
        inv  <- solve(datos, ...)
        a$setinverse(i)
        inv
       
}
