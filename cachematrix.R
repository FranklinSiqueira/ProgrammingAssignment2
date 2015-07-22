## Thanks for the help of all the committed people out there in the in the Web. 

## Calling the functions and getting results:

## v<-makeCacheMatrix();
## v$set(matrix(c(1,0,5,2,3,5,7,0,4), 3, 3));
## cacheSolve(v) - Fisrt time;
##           [,1]        [,2]        [,3];
## [1,] -0.1290323 -0.29032258  0.22580645;
## [2,]  0.0000000  0.33333333  0.00000000;
## [3,]  0.1612903 -0.05376344 -0.03225806;
## cacheSolve(v) - Second time;
## [1] "Getting values from cached data..."
##            [,1]        [,2]        [,3]
## [1,] -0.1290323 -0.29032258  0.22580645
## [2,]  0.0000000  0.33333333  0.00000000
## [3,]  0.1612903 -0.05376344 -0.03225806

## Caching the Inverse of a Matrix.
## Matrix inversion is a time consumming task so
## there are benefits to caching a matrix's inverse 
## rather than computing it every time its values are needed.

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
