## The overall objective of the two functions together is to cache potentially           ## time consuming computations such as matrix inversion, so that if same input was ## provided again, it could produce the result from the cache rather than performin## g the computation( matrix inversion) again. However, for a new matrix input the ## function performs matrix inversion and cache the result.  


## The following function takes a matrix as input. It stores four functions; set, ## get, setinv & getinv. 
## set function- sets the values to that of the parent environment i.e. makecachema## trix function
## get functions gets the value 
## setinv sets the value of the inverse
## getinv gets the inverse of the matrix once computed by cachesolve function


makecachematrix <- function(x = matrix()) {
        inv <- NULL
        set <- function(y) { 
                x <<- y
                inv <<- NULL
        }
        get <- function()x
        setinv <- function(solve) inv <<- solve
        getinv <- function() inv
        list( get = get, set = set,
              getinv = getinv, 
              setinv = setinv)
}


## cachesolve function has two parts. In the first part, it looks for the inverse ## of the matrix in the previous function, and returns the value if found with the ## message. In the second part, it calculates the inverse of matrix, if it does not ## find in the stored value in previous function. 

cachesolve <- function(x, ...){
        inv <- x$getinv()
        if(!is.null(inv)){
                message("getting cached data")
                return(inv)
        }
        data <- x$get()
        inv <- solve(data, ...)
        x$setinv(inv)
        inv
}