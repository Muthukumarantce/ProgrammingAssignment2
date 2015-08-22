## makeCacheMatrix creates a list containing a function to
## 1.set the value of matrix
## 2.get the value of matrix
## 3.set the value of inverse of matrix
## 4.get the value of inverse of matrix


makeCacheMatrix <- function(x = matrix()) 
{
  inverse <- NULL
  set <- function(y)
  {
    x<<-y
    inverse<<-NULL
  }
  get<-function() x
  setinvmatrix<-function(solve) inverse <<-solve
  getinvmatrix<-function() inverse
  list(set=set, get=get,
       setinvmatrix=setinvmatrix,
       getinvmatrix=getinvmatrix)
}


## Returns the matrix which is the inverse of x
## This function suits for only invertible matrix

cacheSolve <- function(x, ...) 
{
 inverse<-x$getinvmatrix()
  if(!is.null(inverse))
  {
    message("getting cached data...")
    return(inverse)
  }
  matrix<-x$get()
  inverse<-solve(matrix,...)
  x$setinvmatrix(inverse)
  inverse  
}
