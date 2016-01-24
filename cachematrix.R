## makeCacheMatrix is a function used for storing a cached version of a
## inverse matrix
##
## cacheSolve is a function used for calculating the inverse of a matrix

## this function will calculate and store an inverse matrix

makeCacheMatrix <- function(x = matrix()) {
  
  #store matrix for later use
  cmatrix<-NULL
  m<-x
  
  #set variable to matrix
  set<-function(y){
    m<<-y
  }
  
  #get currently used matrix(not inverse)
  get<-function(){
    m
  }
  
  #calculate the inverse of matrix
  icalc<-function(){
    
    if(!is.null(cmatrix)){
      message("getting cached version")
      return(cmatrix)
    }
    
    cmatrix<<-solve(m)
    return(cmatrix)
  }
  
  #get the cached version of inverse matrix
  getCache<-function(){
    if(!is.null(cmatrix)){
      return(cmatrix)
    }
    
    message("recalculating inverse matrix")
    icalc()
  }
  
  #clear the cached version of inverse matrix
  clear<-function(){
    cmatrix<<-NULL
    if(is.null(cmatrix)){
      message("cache is empty")
    }
  } 
  
  list(set=set, get=get, icalc=icalc, getCache=getCache, clear=clear)
  
}


## takes a matrix and inverts it

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  
  x$getCache()
}