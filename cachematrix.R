## This program deals with creation of a matrix and inversingf it and caching its value for reuse 

## This Function is used to create a new matrix and that is inversed 

makeCacheMatrix <- function(mt = matrix()) 
{
    inv <- NULL
    set <- function(x) 
	{
        mt <<- x;
        inv <<- NULL;
    }
    get <- function() return(mt);
    setinv <- function(inver) inv <<- inver;
    getinv <- function() return(inv);
    return(list(set = set, get = get, setinv = setinv, getinv = getinv))
}

## This function uses the matrix generated to produce an inverse and check if inverse was already generated and stored in cache

cacheSolve <- function(mt, ...) 
{
    inverse <- mt$getinv()
    if(!is.null(inv)) 
	{
        message("Reteriving Cache data")
        return(inv)
    }
    data <- mt$get()
    invserse <- solve(data, ...)
    mt$setinv(inv)
    return(inv)
}
