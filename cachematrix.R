##create function to make matrix
##x <<- y will assign the matrix input to x in the parent environment
##then store 'inv' in the parent function as 'NULL'
##then assign 'get' to return 'x' from the parent environment
##
makeCacheMatrix <- function(x = matrix()) { 
     inv <- NULL
     set <- function(y) {
     x <<- y
     inv <<- NULL
  }
  get <- function() x
  setinv <- function(inv) inv <<- inverse 
  getinv <- function() inv
  list(set = set, get = get,
       setinv = setinv,
       getinv = getinv)
}


#make cachesolve
#calculates the inverse of the matrix created by the above function
#it first checks to see if the inverse has already been calculated
#if so, it gets the inverse from the cache and skips the computation
#otherwise, it calculates the inverse of the matrix and sets the inverse
#in the cache via the setsolve function

##'m' is assigned the results of the inverse matrix
##since 'm' here and in the parent environment are both 'm', 
##the !is.null(m) statement will be FASLE and the next function 
##will be ignored.   
##next, 'x$get' calls on the get function to assign the original
##matrix to x.
##then assign the function to get the inverse of the orignial matrix(x)
##'solve' to m.
##then x$setinv(m) caches the inverse of the matrix. b/c of the
##nature of m, no inverse calculations will be made in the 
##makeCacheMatrix function
cacheSolve <- function(x, ...) {
        m <- x$getinv() ##get inversed matrix from x
        if(!is.null(m)){
          message("getting cached data")
          return(m)
        }
        data <- x$get()
        m <- solve(data)
        x$setinv(m)
        m ##return the solved result
}

##assigning 'solve.methods' to make a square/nonsingular matrix  
##of values whencommand 'solve.methods(XXX)' is called. XXX being  
#what you want in your matrix
test <- matrix(runif(10000,45,599939),5,5)
testCached <- makeCacheMatrix(test)

test


