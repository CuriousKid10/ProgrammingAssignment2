## KMMCruz
## Assignment: Caching the Inverse Matrix
## To design and formulate functions that can help in the computation of Matrix Inversion.

#Assignment: Caching the Inverse of a Matrix

makeCacheMatrix <- function(m = matrix()) {
      x <- NULL
      put <- function(s) {
           m <<- s
           x <<- NULL
      }
      turn <- function() m
      putinverse <- function(inverse) x<<- inverse
      turninverse <- function () x
      list(put = put,
           turn = turn,
           putinverse = putinverse,
           turninverse = turninverse)
}

## The function computes the inverse of the matrix, which is returned by makeCahceMatrix.

cacheSolve <- function(m, ...) {
     x <- m$turninverse()
     if (!is.null(x)) {
          message("Turning Cached Data.")
          return(x)
     }
     data <- m$turn()
     x <- solve(data, ...)
     m$putinverse(x)
     x
}

## The function assigned to cacheSolving should retrieve the inverse from the cache once calculated.
