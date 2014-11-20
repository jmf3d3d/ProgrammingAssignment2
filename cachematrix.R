## These functions will create cached matrix and a function that will create or access the cached matrix
## 

## This function takes as input a matrix creates a cached matrix object 
## assign the output of this function to a new variable to hold the special object
## e.g.   foo <- makeCacheMatrix(Matrix_x)
## the methods defined below allow access to the matrices in the object using the form
## foo$setinv() for example to set the inverted matrix
## 


makeCacheMatrix <- function(x = matrix()) {
  
  ## assigns a new special object to x and sets the inverted matrix to null

  
        inv <- NULL   # this is the inverted matrix, initialized to NULL
      
                set <- function(y) {
                x <<- y
                inv <<- NULL
                }
      
  ## these are the methods for setting and getting the components of the cached matrix
  
        get <- function() x   ##simply returns the original vector
      
        setinv <- function(solve) inv <<- solve    ##sets the inv matrix to the inverse of the original vector
  
        getinv <- function() inv  ##simply returns the inverted matirx - NULL if not already cached
  
  
      ## this allow access to the function methods defined above
    
        list(set = set, get = get, setinv = setinv, getinv = getinv)
      
      
}


## This function uses the methods the cached object created by makeCachedMatrix
## to access the cached inverted matrix if it exists or to invert the matrix
## it returns the inverted matrix


cacheSolve <- function(x, ...) {
  
  ## first get the existing inverted matrix variable (will be NULL if no inverted matrix has been cached)
        inverted <- x$getinv()
        
        
  ## check to see if this has been inverted and cached already and simply return the cached matrix if it exists
        if(!is.null(inverted)) {
                message("getting cached data")  #so we know it was cached
                return(inverted)
                }
      ##otherwise invert the matrix and used methods set it in x
        data <- x$get()           ## first retrieve the original matrix
        inverted <- solve(data, ...)  ## invert it
        x$setinv(inverted)            ## then cache this inverted matrix 
        inverted    ##return the inverted matrix       
     
}


## for testing purposes - uncomment
##matrixx <- rbind(c(2, -1/4), c(-1/4, 2)) 