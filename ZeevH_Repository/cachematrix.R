## Title: "Programming Assignment 2"
## Date:  "16/08/2014 
## 
## This module contains two procedures "makeCacheMatrix" and "cacheSolve".
## The first procedure creates a special Matrix Object, the second retrives it inverse
## and if necessary caches the inverse matrix in the Object.
##
###########################################################################################

makeCacheMatrix <- function(x = matrix()) {

## Function: Creates a special object that holds a matrix and it's inverse and
##		 four methods ("get", "set", "get_inverse" and "set_inverse") to manipulate 
##		 the Object.
##
## Paramete: "x" - An Inverible matrix - mandatory
##
## Output:   An Object that hold a matrix, it's inverse and four methods as
##		 mentiond above. The matrix is initialized with the parameter value.
##		 
## 
####################################################################################
	  
	  ## Create object Variables

	  matrix_modified <<- TRUE
        matrix_inv <<- NULL
	  mat<<-NULL

	  ## Define object Methods
        set <- function(y=matrix())
	 {
		    mat<<- y
		    ## mat_inv <<- solve(mat) this line was disabled to enable
		    ## 		 		to use cacheSolve function
                matrix_inv <<- NULL
		    matrix_modified <<- TRUE
        }

        get <- function()
		{
		 mat
		}

        set_inverse <- function(x=matrix())
		{
		   matrix_inv <<- x
		   matrix_modified <<-FALSE
		 }
        get_inverse <- function()
		{
			if (matrix_modified)
				{
				return(NULL)
				}
			else return( matrix_inv)
	  	}
	  ## Set Objects Methods

        mat_obj <- list(set = set, get = get,
             set_inverse = set_inverse,
             get_inverse = get_inverse)

	  ## Initialize Object
	  if (is.na(x[1,1]))
	  {
	   	print ("The Object was created but not initialized")
		return(mat_obj)
	  }

	  else 
	  {	  
		mat_obj$set(x)
	  	mat_dummy <- cacheSolve(mat_obj) ## use cacheSolve function to set the
 	  }						   ## inverse matrix
	  
	  return(mat_obj)

## Exit function
}


## Function: Retrives the Inverse matrix of a Special Object matrix
##		 if neccessary computes its value and caches it back.
##
## Paramete: "x" - a Speicial obect matrix - mandatory
##
## Return a matrix that is the inverse of the matrix of 'x'
##
###########################################################################

cacheSolve <- function(x=list()){
	
	  ## Check input parater 

	  if (names(x)[4] != "get_inverse")
	  {
	  	print("Wrong input parameter!")
	  	return(NULL)
	  }
	  ## initialize local variables

 	  data <- NULL
	  m <- NULL

	  ## get inverse matrix from cache

        m <- x$get_inverse()

        ## if a valid inverse matrix return it's value

	  if(!is.null(m))  
	  {
                message<-("getting cached data")
		    print(message)
                return(m)
        }
	  
        else ## compute inverse matrix, cache it and reurn its value 
		{
			data <- x$get()
        		m <- solve(data)
        		x$set_inverse(m)
       		return(m)
		}
## end function
}

