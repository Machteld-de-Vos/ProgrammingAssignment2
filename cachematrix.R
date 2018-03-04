## As matrix inversion can be a computationally intensive operation, 
## these following functions combined help you cache the inverse of a matrix, 
## so it only has to be computed once and can keep on using that value, 
## rather than having to compute it repeatedly. This saves valuable time an 
## computation power! 

## N.B.: Both functions assume that the matrix supplied is invertible. 

## The first function, makeCacheMatrix, creates a special "matrix" 
## object, which is a list containing a function to:
## 1. set the value of the matrix
## 2. get the value of the matrix
## 3. set the value of the inverse of the matrix
## 4. get the value of the inverse of the matrix
## In doing so, this special "matrix" object can cache its inverse. 

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL
        set <- function(y) {
                x <<- y
                i <<- NULL
        }
        get <- function() x
        setinverse <- function(inverse) i <<- inverse
        getinverse <- function() i
        list(set = set, 
             get = get, 
             setinverse = setinverse,
             getinverse = getinverse)
        
}


## Don't forget to store your list in a variable and use this variable in the 
## following function. 

## The second function, cacheSolve, computes the inverse of the special "matrix" 
## returned by the above function, makeCacheMatrix. It first checks to see if 
## the inverse has already been calculated. If so, it gets the inverse from the
## cache and skips the computation. Otherwise, it computes the inverse of the 
## matrix via the solve function, and sets it in the cache via the setinverse 
## function. 

## This function returns a matrix that is the inverse of 'x'. 

cacheSolve <- function(x, ...) {
        i <- x$getinverse()
        if(!is.null(i)) {
                message("Getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}