##These both functions work to generate a matrix, return his inverse and store in a
##diferent enviroment
##The first one generate the matrix and store it in a diferent enviroment and the second evaluate
##if an inverse matrix has been stored and then return it or calculate if it hasn't been stored

##This function returns a list with a method to get the random generated matrix. 
##The dimensions and elements at the matrix are random generated.

makeCacheMatrix<-function(){
  inversa<-NULL
  #I generate a random number for square matrix dimension
  d<-floor(runif(1,1,100))
  #I generate random numbers for fill a square matrix with dimension d,d 
  fill<-lapply(lapply((d*d), runif,min=0,max=100),floor)
  #Fill a matrix with the numbers and save it in a diferent enviroment
  mimatriz<<-matrix(fill[[1]],nrow=d,ncol=d)
  #This function return the matrix
  get <- function() mimatriz
  #Function that save the inverse over the matrix
  setinversa <- function(inv)  inversa<<- inv
  #Function that return the inverse over the matrix
  getinversa <- function() inversa
  #return a list with the functions required for evaluate if an inverse has been made 
  #and the matrix to operate, the list is stored in another enviroment
  listafunciones <<-list(get = get,
       setinversa = setinversa,
       getinversa = getinversa)
}

##This function takes as parameter the matrix to be inversed and evaluate if 
##the inverse has been made previusly or calculate if not
cacheSolve<-function(matriz){
  matriz
  ##Compare the matrix with the stored
  oldmatrix<-listafunciones$get()
  if(!is.null(oldmatrix)){
    if(!identical(oldmatrix,matriz)){
      ##Retrieve the chached inverse matrix if both matrix are the same
      m<-listafunciones$getinversa()
      ##If the inverse matrix is not null then is returned
      if(!is.null(m)){
        message("getting cached data")
        return(m)
      }
    }
  }
 ##If the inverse matrix has not been calculated
  ##calculate the inversion matrix
  inv<-solve(oldmatrix)
  ##Stored in the cache place
  listafunciones$setinversa(inv)
  ##The inverse matrix is returned
  inv
}
