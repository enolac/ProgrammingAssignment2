## Put comments here that give an overall description of what your
## functions do

## there are two functions makeCacheMatrix, makeCacheMatrix
##makeCacheMatrix consist of set, get, setinv, getinv
##library(MASS) is used to calculate inverse for non suqared as well as square matracies
library(MASS)
makeCacheMatrix <- function(x = metrix()) {
    inv <- NULL     #initializing inverse as null
    set <- function(y){
      x <<- y
      inv <<- NULL
       }
    get <- function()x    #function to get matrix x
    setInverse <- function(inverse) inv <<-inverse
    getInverse <- function() {
      inver<-ginv(x)
      inver%*%x                   #function to obtain inverse of the matrix
      }
    list(set = set, get = get, 
         setInverse = setInverse, 
         getInverse = getInverse)
  }
  
  
 ## Write a short comment describing this function
 ## this is used to get the cache data
  
cacheSolve <-function(x, ...) ##gets cache data
    {
    inv <- x$getInverse()
    if(!is.null(inv)){             #checking whether inverse is null
            message("getting cached data")
            return(inv)  #returns inverse value
    }
    data<- x$get()
    inv <- solve(data,...)   # calculates inverse value
    x$setInverse(inv)
    inv   ## Return a matrix that is the inverse of 'x'
  
}
