

## makeCacheMatrix creates a special matrix object that can store

## a matrix and its inverse to avoid repetition of costly computations.

## The matrix object has methods 'set' for putting a new matrix into the 

## matrix object, 'get' for pulling out the stored matrix,

## 'setInverse' for setting the inverse of the matrix 

## and 'getInverse' for accessing the stored inverse of the matrix.


makeCacheMatrix <- function(x = matrix()) {

		inverse <- NULL

		set <- function(mtx) {

				x <<- mtx

				inverse <<- NULL

		}

		get <- function() x

		setInverse <- function(inv) inverse <<- inv

		getInverse <- function() inverse

		list(set = set, get = get,

			setInverse = setInverse,

			getInverse = getInverse)

}


## The cacheSolve function takes as an argument a matrix object created

## by the makeCacheMatrix function, checks if an inverse of the matrix 

## inside the object is already computed. If computed then returns it, 

## otherwise it performs computations and returns a result.

cacheSolve <- function(x, ...) {

        ## Return a matrix that is the inverse of 'x'

		inverse <- x$getInverse()

		if(!is.null(inverse)) {

				message("getting cached data")

				return(inverse)

		}

		mtx <- x$get()

		inverse <- solve(mtx, ...)

		x$setInverse(inverse)

		inverse

}
