makeCacheMatrix <- function(x = matrix()) {
        #1. set value of matrix
        #2. get value of matrix
        #3. set value of of inverse
        #4. get value of inverse
        
        inv <- NULL #sets inverse as NULL
        set <- function(y) {
                x <<- y #gives value y as value x
                inv <<- NULL  #inverse is still NULL     
        }
        get <- function() x #gets value of matrix
        setinv <- function(inverse) inv <<- inverse #sets value of inverse of matrix
        getinv <- function() inv #returns value of inverse of matrix
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}

cacheSolve <- function(x, ...) {
        inv <- x$getinv() #retrieves value of getinv from makeCacheMatrix
        if(!is.null(inv)) {
                message("getting cached data")
                return(inv) #checks to see if inverse is null from previous function
                #if it is not NULL, retrieves inverse from cached data if it is NULL do following:
        }
        data <- x$get() #retrieves get value from makeCacheMatrix
        inv <- solve(data, ...) #solves for inverse of square matrix
        x$setinv(inv) #sets inverse value into this function
        inv #returns value of inverse of matrix
}
