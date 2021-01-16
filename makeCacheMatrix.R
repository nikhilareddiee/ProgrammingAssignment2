## put comments here that give an overall description of what your 
## functins do

## there are two functions mkeCacheMatrix, makeCacheMatrix
##makeCacheMatrix consists of set,get,setinv,getinv
makeCacheMatrix <- function(x = matrix()){
   inv <- NULL       #initializing inverse as NULL
  set <- function(y){
    x <<- y
  sinv <<- NULL
       }
  get <- function(){x}             #function to get matrix x 
   setInverse <- function(inverse){inv <<- inverse}
   getInverse <- function() {inv
                             inver <- ginv(x)
                             inver%*%x                 # function to obtain inverse of the matrix 
                             }
  list(set= set , get=get, setInverse=setInverse, getInverse=getInverse)
   }
 
  


## write a short comment describing this function 
## This is used to get cache data
   
   cacheSolve <-  function(x,...){           ## gets cache data 
      inv <- x$getInverse()
      if(!is.null(inv)){                        # checking whether inverse is NULL
         message("getting cached data")           
          return(inv)                           #returns inverse value 
        }
      mat <- x$get()
       inv <- solve(mat, ...)                       # calculates inverse value
       x$setInverse(inv)
      inv           ## Return a matrix that is the inverse of 'x'
     }
  
   
