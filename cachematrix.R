## ****************************************************************
## The following two methods attempt to:
## 1. Take a given matrix
## 2. Return the cached matrix if it already exists for the given matrix.
## 3. If the cache is empty, it will calculate the inverse of it
## 4. Save the inverse to cache
## ****************************************************************

## *********************************************************************
## makeCacheMatrix
##
## Created On: 06/18/2015
## Desc: The method is a library of methods that extend the functionality
## that can be performed on a given matrix. 
##
## The librar will allow you to cache a matrix, return the matrix from 
## cache, get the cached inverse, and set the cached inverse.
## *********************************************************************
makeCacheMatrix <- function(x = matrix()) {
    
    # Initialize the variable used to save the inverse
    inverseMatrix <- NULL
    
    # Allows you set the matrix to a local variable and reinitialize the inverse variable.
    set <- function(y) {
        x <<- y
        inverseMatrix <<- NULL
    }
    
    # Returns the cached matrix
    get <- function() x
    
    # Returns the cached inverse of the matrix
    getInverse <- function() inverseMatrix
    
    # Saves the inverse to cache
    setInverse <- function(m) inverseMatrix <<- m
    
    list (
          set = set,
          get = get,
          getInverse = getInverse,
          setInverse = setInverse
         )
}


## *********************************************************************
## cacheSolve
##
## Created On: 06/18/2015
## Desc: This method will attempt to return in the inverse of a matrix.
## It will first attempt to return the cached inverse, if it exists. If
## it doesn't, it will calculate the inverse and save it to cache, before
## returning it to the calling program
## *********************************************************************
cacheSolve <- function(x, ...) {
    ## Get the inverse
    inverse <- x$getInverse() 
    
    ## Get the matrix
    matrix <- x$get()
    
    ## If the inverse existed in cache, return that.
    if (!is.null(inverse)){
        message('cached')
        return(inverse)
    }
    
    ## If the inverse didn't exist, we will calculate it now and save it to cache
    message('Not cached')
    inverse <- solve(matrix, ...)
    x$setInverse(inverse)
    
    ## Return the inverse
    inverse
}
