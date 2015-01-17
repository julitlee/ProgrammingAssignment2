##The two functions in this program are used to cache the inverse of a matrix for faster calculations in the future.

##This first function is called "makeCacheMatrix". makeCacheMatrix is a function where the user defines a "special matrix" object that is cached for another function cacheSolve.


makeCacheMatrix <- function (x = matrix()) {
        m <- NULL ##sets the value of m to NULL as a placeholder for future value
        set <- function (y) { ##defines the function
                x <<- y ##caches the inputted matrix so that cacheSolve checks for changes
                m <<- NULL ##sets the value of m (the matrix inverse if used cacheSolve) to NULL
        }
        get <- function() x ##returns the matrix x
        setmatrix <- function(solve) m <<- solve ##sets the matix m to solve
        getmatrix <- function(solve) m ##returns the matrix m
        list(set = set, get = get,     ##returns the 'special matrix' containing all functions just defined
             setmatrix = setmatrix,
             getmatrix = getmatrix)
}

## This second function is called "cacheSolve". cacheSolve is a function to caculate the inverse matrix object 'x' that was defined in makeCacheMatrix

cacheSolve <- function (x=matrix(), ...) {
        m <- x$getmatrix()
        if (!is.null(m)) {
                message("Retrieved from cached data.")
                return(m)
        }
        matrix <- x$get ()
        m <-solve (matrix, ...)
        x$setmatrix (m)
        m
}

##To Test

##> x <- matrix (1:4, 2,2)
##> m = makeCacheMatrix(x)
##> m$get()
##      [,1] [,2]
##[1,]    1    3
##[2,]    2    4

##> cacheSolve(m)
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5

##> cacheSolve(m)
##Retrieved from cached data.
##[,1] [,2]
##[1,]   -2  1.5
##[2,]    1 -0.5