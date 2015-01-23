## create caching matrix
makeCacheMatrix <- function(aMatrix = matrix()) {
  matrix <- aMatrix ## 'global' matrix
  inverse <- NULL   ## 'global' cache
  
  ## setter, set matrix and clean cache
  setMatrix <- function( aMatrix) {
    # store it in the 'global' matrix
    matrix <<- aMatrix
    
    # clean 'global' inverse
    inverse <<- NULL
  }
  
  ## matrix getter
  getMatrix <- function() { 
    matrix
  }
  
  ## inverse setter
  setInverse <- function( anInverse ) {
    # store it in the 'global' inverse
    inverse <<- anInverse
  }
  
  ## inverse getter
  getInverse <- function() {
    inverse
  }
  
  ## return Caching Matrix, it became sort of 'object' with methods and data (matrix, inverse)
  list( setMatrix = setMatrix,
        getMatrix = getMatrix,
        setInverse = setInverse,
        getInverse = getInverse)
}


## calculate inverse of a caching matrix
## use inverse from the cache if the cache not empty
cacheSolve <- function(aMatrix, ...) {
  myInverse <- aMatrix$getInverse()

  if( is.null(myInverse) ) {
      ## cache is empty
      ## calculate
      myInverse <- solve( aMatrix$getMatrix() )
      
      ## save for later
      aMatrix$setInverse( myInverse )    
    }
    
  # return inverse
  myInverse    
}
