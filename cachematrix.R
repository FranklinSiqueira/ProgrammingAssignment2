## Caching the Inverse of a Matrix
## Matrix inversion is usually a costly computation.
## There may be some benefit to caching the inverse of
## a matrix rather than compute it repeatedly.

makeCacheMatrix <- function(x=matrix()) {

## This function creates a list of functions that
## caches the inverse of a given matrix.

    cache <- NULL
    
        # in line 11, cache stores the cached value
        # and initializes it to NULL;

    set <- function(y) {

        x <<- y

        cache <<- NULL

    }
    
    # Lines 16 to 22, creates the matrix in the working environment;

    get <- function() x
    
    # In the line 26, get gets the value of the matrix;

    setInverse <- function(inverse) cache <<-inverse
    
    # In the line 30, setInverse gets the inverse of the matrix and stores it in cache;

    getInverse <- function() cache
    
    # In the line 34, getInverse gets the inverted matrix from cache;

    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
         
        # Lines 38 to 40, returns the created functions to the working environment.

}

cacheSolve <- function(x, ...) {

## This function computes the inverse of the given matrix returned
## by makeCacheMatrix() (i.g. if the inverse hasn't
## already been calculated). If that's the case
## it retrieves these values from cache.
## It’s assumed that the matrix supplied is always invertible.

    cache <- x$getInverse()
    
    # Firstly, in the line 56, cache is stored;

    if ( ! is.null(cache)) {

        print("Getting values from cached data...")

        return(cache)

    }
    
    # From line 58 to line 64, cache is assessed and, if it has values, data are retrived; 

    cache <- solve(x$get())
    
    # creates matrix, known that after assessment from lines 58 to 64, it was empty;

    x$setInverse(cache)
    
    # Sets the inverted matrix in cache;

    cache
    
    # Finally, displays resulting matrix.

}
