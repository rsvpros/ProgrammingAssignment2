## makeCacheMatrix creates and caches a Cachematrix object with associated methods
##cacheSolve takes a cacheMatrix object and returns its

## makeCacheMatrix 
##takes a matrix as an argument and creates an object which can use functions:

#$get           to return its matrix value
#$set           to change it
#$getInverse             to return the inverse
#$setInverse            to set the inverse
#Note: $setInverse and $set use caching to store the data globally

makeCacheMatrix <- function(x = matrix()) {
                m <- NULL
                set <- function(y) {
                        x <<- y
                        m <<- NULL
                }
                get <- function() x
                setInverse <- function(inverse) m <<- inverse
                getInverse <- function() m
                list(set = set, get = get,
                     setInverse = setInverse,
                     getInverse = getInverse)
        }


# Computing the inverse of a square matrix can be done with the solve function in R.
#For example, if X is a square invertible matrix, then solve(X) returns its inverse.
# 
# For this assignment, assume that the matrix supplied is always invertible.

## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        m <- x$getInverse()
        if(!is.null(m)) {
                message("getting cached inverse...")
                return(m)
        }
        data <- x$get()
        m <- solve(data, ...) #takes the inverse
        x$setInverse(m)
        m
}

##testing notes:

matrix <- cbind(c(4,2),c(7,6)) 
#makes: [4, 7]
#       [2, 6]


cacheMatrix <-makeCacheMatrix(matrix)
#turns matrix into a CacheMatrix

cacheSolve(cacheMatrix)
#Solves cache Matrix for the first time. Expected output:
#       [0.6, -0.7]
#       [-0.2, 0.4]

cacheSolve(cacheMatrix)
#Solves cache Matrix for the second time. Expected output:
#getting cached inverse...
#       [0.6, -0.7]
#       [-0.2, 0.4]
#

