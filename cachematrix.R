## Put comments here that give an overall description of what your functions do

##1 makeCacheMatrix: This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {   ## define the mode of "matrix"        
        Inver <- NULL                 ## set Inver as NULL; It will hold value of matrix inverse  
        setMatr <- function(y) {   ## define the set function, assign new value of matrix in parent environment
                x <<- y        
                Inver <<- NULL       ##  reset inver to NULL
        }
        getMatr <- function() x                                       ## define the get fucntion -get the value of from "matrix"
        setmeanInv <- function(mean) Inver <<- mean ## assigns value of inver to in parent environment
        getmeanInv <- function() Inver                          ## gets the value of inver where called
        list(setMatr = setMatr, getMatr = getMatr,
             setmeanInv = setmeanInv, getmeanInv = getmeanInv)  }  ## list set value of the vector, get value of the vector,
## set value of the mean and getvalue of the mean.

##2 cacheSolve: This function computes the inverse of the special "matrix" returned by makeCacheMatrix above.
##If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should
##retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {    ##define a functon                                                      
        Inver <- x$getmeanInv()         ##get the value of the invertible matrix from the makeCacheMatrix function
        if(!is.null(Inver)) {                    ##if Inver matrix is not NULL
                message("getting cached Matrix data")   ##Type message: getting cached Matrix data
                return(Inver)                    ##return the invertible matrix
        }
        data <- x$getMatr()       ##get the original Matrix Data  
        Inver <- solve(data, ...)  ##use solve function to inverse the matrix
        x$setmeanInv(Inver)       ##set the invertible matrix  
        Inver                     ## return the invertible matrix
}