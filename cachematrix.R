## 201/03/21 Coursera IJHaasbroek
## Function is supposed to store the inverse of a matrix in a special variable 
## to save time if the inverse is needed later without explicitly doing it yourself. 

##At time of this functions creation my knowledge is imperfect and this is 
##primarily based on the example given in the assignment

##The following site xplains what the underlying principle of the program
##https://www.coursera.org/learn/r-programming/peer/tNy8H/programming-assignment-2-lexical-scoping/discussions/threads/Eivd_AKqEeeuBAowaKnz2A



## This should make a variables that wil be used to store the inverse in theory

makeCacheMatrix <- function(x = matrix()) {
  
    inv<<-NULL
  
    set<-function(y){
      x<<-y
      inv<<-NULL
    }
    
    get<-function(){
      x
    }
    
    setinverse<-function(inverse){
      inv<<-inverse
    }
    
    getinverse<-function(){
      inv
    }
    
    list(
      set=set,
      get=get,
      setinverse=setinverse,
      getinverse=getinverse
    )
}


## This should check if the inverse is stored in the varible made 
##by the previous function. if the variable is NULL it calculates 
##the inverse and stores it in the variable. If it is not NULL
##it returns the stored inverse and does not waste time recalculating it

cacheSolve <- function(x, ...) {
  
        inv<-x$getinverse()
        
        if(!is.null(inv) ){
          
          message("getting cached data")
          return(inv) 
        }
        
        data<-x$get()
        
        m<-solve(data, ...)
        
        x$setinverse(m)
}
