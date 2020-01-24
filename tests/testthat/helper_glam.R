###  useful functions for array multiplication and roation
### see Currie, Durban and Eilers (2006 JRSSB)
### for the definitions

## Author: Luo Xiao at Cornell Univ.

# Row tensor of a matrix X
Rten = function(X){
	one = matrix(1,1,ncol(X))
	return(kronecker(X,one)*kronecker(one,X))
	}
# H-transform of an array A by a matrix X
H = function(X,A){
	d = dim(A)
	M = matrix(A,nrow=d[1])
	XM = X%*%M
	return(array(XM,c(nrow(XM),d[-1])))
	}
# Rotation of an array A
Rotate = function(A){
	d = 1:length(dim(A))
	d1= c(d[-1],d[1])
	aperm(A,d1)
	}
# Rotated H-transform of an array A by a matrix X
RH = function(X,A) return(Rotate(H(X,A)))
