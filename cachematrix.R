## makeCacheMatrix will create and return a list of functions.
## 4 functinos will be created
## 

makeCacheMatrix <- function(x = matrix()) {
  
  matrixInverse <- NULL
  
  # create the set function store the original matrix
  set <- function(y){
    x <<- y
    matrixInverse <<- NULL
  }
  
  # get will return the matrix
  get <- function() x    
  
  # cache the inverse of the inverse
  setmatrix <- function(iMatrix) matrixInverse <<- iMatrix
  
  #return the inverse of the matrix
  getmatrix<-function() matrixInverse
  
  # create the list containing the functions
  list(set=set, get=get,setmatrix=setmatrix,getmatrix=getmatrix)
}


## cacheSolve will return the inverse of a matrix
## it will use the functions created in makeCacheMatrix
## first it will check to see if the inverse of the matrix is already in cache.
## If the inverse is in cache, it will return the cached version
## if tht inverse is not in cache, it will call makeCacheMatrix to store it in cache.

cacheSolve <- function(x, ...) {
      
  invx <- x$getmatrix()   # get the cached inverse if it exists
  if(!is.null(invx)){     # check if cached is retruned from makeCacheMatrix
    return(invx)          # then return it
  }
  
  oMatrix <- x$get()       # get the original matrix stored from makeCacheMatrix
  
  invx <- solve(oMatrix, ...)
  
  x$setmatrix(invx)        # store the inverse in cache
  
  invx                    # return the inverse  
  
}
