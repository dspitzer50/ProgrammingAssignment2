# `makeCacheMatrix` :: This function creates a special "matrix" object, 
#   that can cache its inverse.  It has a list containing functions to:
#      set: set the value of the matrix
#      get: get the value of the matrix
#      setmtrx: set the value of the inverse matrix
#      getmtrx: get the value of the inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  m <- NULL
  
  set <- function(y) {
    x <<- y
    m <<- NULL
  }
  
  get <- function() x
  
  setmtrx <- function(mtrx) m <<- mtrx
  
  getmtrx <- function() m
  
  list(set = set, get = get,
       setmtrx = setmtrx,
       getmtrx = getmtrx)
}


# `cacheSolve` :: This function computes the inverse of the special
# "matrix" returned by `makeCacheMatrix` above.  If the inverse has
# already been calculated (and the matrix has not changed), then
# `cacheSolve` should retrieve the inverse from the cache.  Otherwise,
# it uses solve() to find the inverse.

cacheSolve <- function(x, ...) {
  m <- x$getmtrx()
  if(!is.null(m)) {
    message("getting cached data")
    return(m)
  }
  
  data <- x$get()
  message("doing calc...")
  m <- solve(data)
  x$setmtrx(m)
  m
}


## Debugging...
print("=======================================")
mat1<-matrix(c(10,20,30,40,50,60,71,81,92,10,
            20,30,40,50,60,71,81,92,30,40,
            50,60,71,81,92),  5, 5)

print("mat1")
print(mat1)

print("cMat1 <- makeCacheMatrix(mat1)")
cMat1 <- makeCacheMatrix(mat1)

print("1st call to cacheSolve(cMat1)")
cacheSolve(cMat1)
matInv <- (cMat1$getmtrx())
print("matInv")
print(matInv)

#cMat1 <- makeCacheMatrix(mat1)
print("2nd call to cacheSolve(cMat1)")
cacheSolve(cMat1)
matInv <- cMat1$getmtrx()
print("matInv")
print(matInv)
  
print("mat1 X matInv -> ")
print(mat1 %*% matInv)

