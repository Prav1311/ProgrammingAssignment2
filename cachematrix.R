
##makecachematrix wil be used for storing and displaying a matrix and it's inverse
##cacheSolve will be used for retriving inverse from cache or calculating and setting the inverse of martix
## cacheSolve function accepts an object of makeCacheMatrix type and can access all  functions in makeCacheMatrix

## makecachematrix function has set and get functions to set values of matix and display it.
## It also has setinv and getinv functions to set the value for the inverse of the entered matrix and display it

makeCacheMatrix<-function(x=matrix())  {
        m<-matrix() #matrix m intialized with NA value & 1 row and coloumn 
        set <- function(y=matrix()) { 
                x <<- y
                m <<- matrix()#matrix m intialized globally with NA value & 1 row and coloumn 
        }
        get <- function() x #to print value of entered matrix
        setinv <- function(inv=matrix()) m <<- inv #value of the matrix inverse assigned to matrix m 
        getinv <- function() m #to display value of matrix inverse
        list(set = set, get = get,
             setinv= setinv,
             getinv = getinv) #returning list for ease of accessing functions in makeCacheMatrix() with objectname an $ sign
}

## cacheSolve function checks whether inverse of matrix has already been calculated earlier .
## If it has been calculated it retrives value from cache, if not it caulcuates and stores inverse using the setinv function

cacheSolve<-function(y,..){
        m1<- y$getinv()  #value of matrix inverse (if available) will be stored in m1
        t1<- all(is.na(m1)) #checking if all values of current inverse matrix are NA 
        if(!t1) # if all values are not NA (meaning input matrix is unchanged) then return previously calculted matrix inverse
        {    message("getting cached inverse for matrix")
                return(m1)
        }
        else{ mat<- y$get() #if all values are NA it means set function has been used & input matrix has changed 
        m1<- solve(mat)#calculating inverse for new input matrix
        y$setinv(m1)#setting new value for inverse in the inverse matrix 
        m1}
}

