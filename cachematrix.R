## makeCacheMatrix takes a matrix as an argument and returns a list of functions (set, get, setinv, getinv)
## cacheSolve function takes as an argument the result of the makeCacheMatrix function for a given matrix and returns its' 
## inverse - if the inverse matrix is already calculated it retrieves the cached value


## makeCacheMatrix function includes the formulas of 4 subfunctions: 
## set : sets argument matrix x equal to a free variable y and inverse matrix xinv as NULL
## get : returns matrix x
## setinv : takes the inverse of x as an argument and assigns it to xinv in the parent environment
## getinv : returns the inverse xinv of matrix x    

makeCacheMatrix <- function(x = matrix()) {
    xinv <- NULL
    set <- function(y) {
        x <<- y
        xinv <<- NULL
    }
    get <- function() x
    setinv <- function(inv) xinv <<- inv
    getinv <- function() xinv
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}

## cacheSolve function takes as an argument the result x of the makeCacheMatrix function for a given matrix and
## 1) assigns the result of getinv() subfunction for the matrix to xinv
## 2) checks if xinv is not NULL
## 3) if xinv is not NULL, it prints the message "getting cached data" and returns xinv
## 4) if xinv is NULL:
##	- it assigns the given matrix to 'data' (matrix is read from the get() subfunction)
##	- it calculates the inverse xinv of the matrix via the solve() function
##	- it calls the setinv() subfunction, which assigns the calculated xinv to xinv in the parent environment 
##	- it returns the inverse xinv of the matrix 

cacheSolve <- function(x, ...) {
    xinv <- x$getinv()
    if(!is.null(xinv)) {
        message("getting cached data")
        return(xinv)
    }
    data <- x$get()
    xinv <- solve(data, ...)
    x$setinv(xinv)
    xinv
}





