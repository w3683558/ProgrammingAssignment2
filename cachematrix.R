#This set of function (makeCacheMatrix and cacheSolve) will compute
#or cache the inverse of a user-defined matrix 

# This function returns a list that contains the user-defined matrix
# as well as the functions that set and get matrix and its inverse 
makeCacheMatrix <- function(x = matrix()) {
        inv <- NULL
        setM <- function(y){
                x <<- y
                inv <<- NULL
        }
        getM <- function()x
        setInv <- function(i) inv <<- i
        getInv <- function() inv
        list(setM = setM, getM = getM, setInv = setInv, getInv = getInv)
}


# This function will return the inverse of the matrix 
# defined in makeCacheMatrix function

cacheSolve <- function(x, ...) {
        inv <- x$getInv() 
        if(!is.null(inv)){
                message("getting cached data: inverse matrix =")
                return(inv)
        }
        data <- x$getM()
        inv <- solve(data,...)
        x$setInv(inv)
        inv
}
