
## makeCacheMatrix creates a special "matrix" object that can cache its inverse.
## It returns a list of functions that set the value of the matrix, get the value of the matrix,
## set the value of the inverse and get the value of the inverse.

makeCacheMatrix <- function(x = matrix()) {
    
    inv <- NULL                     ## set variable inv to NULL                  
    set <- function(y) {            ## set function assigns y to x and NULL to inv
        x <<- y
        inv <<- NULL
    }
    
    get <- function() {             ## get function returns the value of x
        x
    }
    
    setinverse <- function(solve){
        inv <<- solve               ## assigns solve to inv
    }
    getinverse <- function() {
        inv                         ## returns inv
    }
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse) 
}


## cacheSolve calculates the inverse of the special "matrix" created by makeCacheMatrix.
## Before doing so it checks if the inverse matrix was calculated previously and already held in cache
## If it is held in cache, it retrieves the inverse matrix from cache and print a message to screen
## stating that the inverse matrix is being obtained from the cache.
## If not, it calculates the inverse of the matrix and sets the value of the matrix in the cache via the
## setCache function

cacheSolve <- function(x, ...) {                ## Return a matrix that is the inverse of 'x'
        
    inv <- x$getinverse()
    if(!is.null(inv)) {                         # if inv is not null, it was cached
        message("getting cached data")          # print message to screen
        return(inv)                             # return inv
    }
    data <- x$get()
    inv <- solve(x)
    x$setinverse(inv)
    inv
}
