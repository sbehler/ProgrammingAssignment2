
##These 2 functions together will take a square invertible matrix 
##    and inverse it.
##The first function is used to initialize an object which will be
##    used to find an inverse of the matrix passed to it.
##The second function actually does the inverse, and updates the cached object
##    which is the inverse matrix object


## This function will take in a matrix and make a Cache of its inverse
## it will call the function cacheSolve to do the inverse computation

makeCacheMatrix <- function(x = matrix()) {
    
    ##INITIALIZE THE INVERSE MATRIX OBJECT
    inverseMatrix <- NULL
    
    ##Create an internal funciton which will replace the current Matrix
    set <- function(newMatrix) {
        x <<- newMatrix
        inverseMatrix <<- NULL
    }
    
    
    ## FUNCTION FOR DISPLAYING CURRENT MATRIX
    getCurrent <- function() x
    
    #FUNCTION FOR SETTING THE INVERSE MATRIX OBJECT (inverseMatrix)
    setInverse <- function(inverse) inverseMatrix <<- inverse
    
    ## FUNCTION FOR DISPLAYING THE INVERSE MATRIX OBJERCT (inverseMatrix)
    getInverse <- function() inverseMatrix
    
    ##LIST FOR LISTING THE PARTS OF X
    list(set = set, getCurrent = getCurrent,
         setInverse = setInverse,
         getInverse = getInverse)
    
    
}



## This function will take in a matrix and return its inverse

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    
    ##USE THE GET INVERSE FUNCTION FROM THE CURRENT MATRIX OBJECT
    ##  TO INITIALIZE THE INVERSE OBJECT
    inverseMatrix <- x$getInverse()
    
    ##CHECK TO SEE IF THE INVERSE OBJECT IS NULL
    ##  IF NOT, THE INVERSE HAS ALREADY BEEN COMPUTED
    ##  RETURN THE VALUE OF THE INVERSE OBJECT
    if(!is.null(inverseMatrix)) {
        
        message("getting cached data")
        
        return(inverseMatrix)
    }
    
    ##INITIALIZE AN OBJECT TO HOLD THE CURRENT MATRIX VALUE
    data <- x$getCurrent()
    
    ##USE THE SOLVE FUNCTION TO INVERSE THE CURRENT MATRIX VALUE
    inverseMatrix <- solve(data)
    
    ##USE THE SET INVERSE FUNCTION ON THE CURRENT MATRIX OBJECT
    ##  TO SET THE VALUE OF THE INVERSE OBJECT WITHIN TO THE INVERSE
    x$setInverse(inverseMatrix)
    
    ##RETURN THE INVERSE OBJECT'S VALUE TO THE DISPLAY
    inverseMatrix
    
}