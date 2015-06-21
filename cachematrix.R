## Put comments here that give an overall description of what your
## functions do

## The following function creates a "matrix" object that can cache its inverse 
##  by constructing the get and set accessor functions of matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
  
        ## initialize inverse matrix variable to NULL
        inverse <- NULL
        
        ## construct getMtx() function that returns actual matrix
        getMtx <- function() return(x)
        
        ## construct setMtx() function that resets cached inverse matrix with
        ## actual matrix initialized to matrix 'm' being passed
        setMtx <- function(m) {
            x <<- m
            inverse <<- NULL
        }
        
        ## construct getInvMtx() function that returns the cached inverse matrix
        getInvMtx <- function() return(inverse)
        
        ## construct setInvMtx() function that cache the inverse matrix ('y') of 'x'
        setInvMtx <- function(y) inverse <<- y
        
        ## returns the get and set accessor functions of matrix and its inverse
        list(getMtx = getMtx,setMtx = setMtx,getInvMtx = getInvMtx,
             setInvMtx = setInvMtx)
}


## This function returns the inverse of the "matrix" returned by makeCacheMatrix
## by looking into the cache through getInvMtx()
## or compute it and puts in cache if not exists through setInvMtx() 
 
cacheSolve <- function(x, ...) {       
  
        ## gets the cached inverse matrix if any
        inverse <- x$getInvMtx()
        
        ## obtains the cached data and return the inverse if exists in cache
        if(!is.null(inverse)){
          message("cache data available... getting it")
          return(inverse)
        }
        
        ## if not in cache, compute the inverse through solve()
        ## and puts it in cache
        m <- x$getMtx()        
        inverse <- solve(m,...)        
        x$setInvMtx(inverse)
        
        ## return the inverse of the matrix
        return(inverse)      
          
}