#' MakeCacheMatrix 
#'
#' @param X, matrix
#'
#' @return matrix object, with basic functions get, set, getinv, setmat
#'
#' @examples
#' set.seed(265386)
#' r = rnorm(100)
#' matx = matrix (r, nrow=10, ncol=10)
#' matobj = makeCacheMatrix(matx)
makeCacheMatrix <- function(x = matrix()) {
  
  minv <- NULL
  
  #Retrieve the matrix from the WD
  get = function() x
  
  #Create the matrix in the WD
  set = function(y) {
    x <<- y
    minv <<- NULL
  }
  
  #Retrieve Inverse Matrix from the Cache
  getinv = function() minv
  #Store Inverse Matrix to the cache
  setmat = function(z) minv <<- z 
  
  
  #Set the functions list
  list(set=set, get=get, setmat=setmat, getinv=getinv)
}

#' cacheSolve 
#'
#' @param X, makeCacheMatrix Object
#'
#' @return matrix inverse
#'
#' @examples
#' set.seed(265386)
#' r = rnorm(100)
#' matx = matrix (r, nrow=10, ncol=10)
#' matobj = makeCacheMatrix(matx)
#' cacheSolve(matobj)

cacheSolve <- function(x, ...) {
  minv <- NULL
  
  #try to get inv from cache
  minv = x$getinv()
  
  #inv is not found, so we need to calculate it, and store it on cache
  if (is.null(minv)){
    #get the matrix
    c.matrix = x$get()
    #calculate inverse
    minv = solve(c.matrix, ...)
    #store inv on cache
    x$setmat(minv) 
  }
  
  #return inverse, else removed because of is not necesary
  minv
}