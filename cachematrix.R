## makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.
## cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix 
##             If the inverse has already been calculated (and the matrix has not changed), 
##             then the cachesolve retrieves the inverse from the cache.


## This function creates a special matrix which holds a matrix and its inverse in cache, 
## provides access to matrix and inverse through the get and set methods of the special matrix object
makeCacheMatrix <- function(x = matrix()) {
    
    if(!is.matrix(x))
    {
        message("input argument is not a valid matrix")
        return()
    }
    
    ## intially setting the inverse to null
    inverse <- NULL        
    
    
    set <- function(y){
        ## Checking if new matrix is same as the existing one, 
        ## if not resetting the matrix and inverse
        if(!(is.matrix(x) && is.matrix(y) && dim(x)==dim(y) && all(x ==y)))
        {            
            x <<- y
            inverse <<- NULL
        }        
    }
    
    get <- function() x
    
    setInverse <- function(i)  inverse <<- i      
    
    getInverse <- function() 
    {
        inverse 
    }
    
    list(set = set, get = get, setInverse = setInverse, getInverse = getInverse )
    
}


## Computes the inverse of matrix within x if doesn't exist, 
## if it already exist returns the inverse from the cache through getInverse function of x
cacheSolve <- function(x, ...) {
    inverse <- x$getInverse()
    
    if(!is.null(inverse)){            
        message("getting cached inverse")
        return(inverse)        
    }    
    computeAndSetInverse(x, ...)    
}


computeAndSetInverse <- function(x, ...) {
    mat <- x$get()
    inverse <- solve(mat, ...)
    x$setInverse(inverse)
    inverse
}
