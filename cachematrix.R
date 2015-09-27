


makeCacheMatrix <- function(myMat = numeric()) {
  m <- NULL
  set <- function(y) {
    myMat <<- y
    m <<- NULL
  }
  get <- function() myMat
  solveMat <- function(solve) m <<- solve
  getinverse <- function() m
  list(set = set, get = get,
       solveMat = solveMat,
       getinverse = getinverse)
}

cacheSolve <- function(myMat, ...) {
  m <- myMat$getinverse()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  data <- myMat$get()
  m <- solve(data, ...)
  myMat$solveMat(m)
  m
}
