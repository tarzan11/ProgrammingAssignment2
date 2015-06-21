## The following functions calculate the inverse of a matrix and cache it.  If 
## it has already been calculated, they return the cached inverse. 

## The makeCacheMatrix receives a matrix as its input (x), caclulates its 
## inverse and stores the inverse in a matrix (inv). It returns: a function 
## to pass the matrix initialize the inverse matrix (set), a function to 
## calculate the inverse (setinv), and a function to grab the inverse back 
## (getinv).  These four are returned in a list.

makeCacheMatrix <- function(x = matrix()) {
        ## Initialize inv (matrix container for the inverse) with NA
        inv <- matrix(NA,1,1)           
        
        set <- function(y) {
                ## Assign the matrix to x and make it available across 
                ## environments (<<)
                x <<- y
                ## Initialize the inverse matrix (inv) with the sanem 
                ## dimensions as the input
                inv <<- matrix(rep(0,nrow(x)*ncol(x)), nrow(x),ncol(x))
        }
        ## the get function simply receives the input matrix
        get <- function() x
        ## the setinv calculates the inverse matrix
        setinv <- function(solve) inv <<- solve
        ## the getinv function simply returns the inverse matrix
        getinv <- function() inv
        ## Finally, pass the 4 functions in a list
        list(set = set, get = get,
             setinv = setinv,
             getinv = getinv)
}        
  
## The cacheSolve function, uses th list returned by makeCacheMatrix to check 
## the existence of the answer in cache and if not calculates the cache and 
## returns it

        cacheSolve <- function(x, ...) {
                ## 
                inv <- x$getinv()
                if(!is.na(inv[1,1])) {
                        message("getting cached data")
                        return(inv)
                }
                data <- x$get()
                inv <- solve(data)
                x$setinv(inv)
                inv
        } 