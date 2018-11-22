##Caches the inverse of a matrix through a constructor 'makeCacheMatrix' and a calculator 'cacheSolve'

##Constructor for object of type makeCacheMatrix; contains repository caches 'x' and 'inv'

makeCacheMatrix <- function(x = numeric()) { #caches a new matrix in cache 'x'
        inv <- NULL #initial clearing of inverse cache 'inv' for new matrix
  
        set <- function(y) { #caches a new matrix and clears inverse cache
              x <<- y
              inv <<- NULL
        }

        get <- function() x #returns the cached matrix
        
        setinverse <- function(inverse) { #caches matrix inverse
              inv <<- inverse
        }
        
        getinverse <- function() inv #returns cached inverse
        
        list(set = set, setinverse = setinverse, get = get, getinverse = getinverse)
        #switchboard for direct access to getters and setters through makeCacheMatrix objects using '$' operator
}


##Calculates the inverse of a new matrix or returns the cached inverse for an unchanged matrix.

cacheSolve <- function(x, ...) {
        inv <- x$getinverse()
  
        if (!is.null(inv)) return(inv) #check for existing inverse and return

        matrix <- x$get()
        inv <- solve(matrix) #calculate matrix inverse
        x$setinverse(inv) #store inverse in cache
        inv
}
