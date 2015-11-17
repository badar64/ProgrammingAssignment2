## Programming Assignment 2: Lexical Scoping

## makeCacheMatrix - This function creates a special "matrix" object that can cache
##                   the inverse of a matrix.
makeCacheMatrix <- function(x = matrix()) {
      inv <- NULL
      set <- function(val) { 
            x <<- val
            inv <<- NULL
      }
      get <- function() x
      setInv <- function(inverse) inv <<- inverse
      getInv <- function() inv
      
      list(set=set, get=get, setInv=setInv, getInv=getInv)
}

## cacheSolve - This function computes the inverse of the special "matrix" created by 
##              makeCacheMatrix. If the inverse has already been calculated (and the 
##              matrix has not changed), then it should retrieve the inverse
##              from the cache.
cacheSolve <- function(x, ...) {
      inv <- x$getInv()
      if(!is.null(inv)) {
            print("Loading data from cache")
            return (inv)
      }
      
      mat_x <- x$get()
      inv <- solve(mat_x, ...)
      x$setInv(inv)
      inv
}
