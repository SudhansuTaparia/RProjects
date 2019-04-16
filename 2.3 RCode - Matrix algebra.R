## Basic matrix algebra
## Reference: http://www.statmethods.net/advstats/matrix.html

help(matrix)

A<-matrix(c(1,2),nrow=1,ncol=2,byrow=TRUE) # matrix assignment
A
t(A) # transpose of a mtrix

B<-matrix(c(1,2,3,4),nrow=2,ncol=2,byrow=TRUE)
B
t(B)

C<-matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=TRUE)
C
t(C)

A<-matrix(c(1,2,3,4,5,6),nrow=3,ncol=2,byrow=TRUE)
A
B<-matrix(c(1,2,3,4,5,6,7,8),nrow=2,ncol=4,byrow=TRUE)
B
C<-A%*%B # matrix multiplication
C
D<-t(B)%*%t(A) ## note,  B%*%A is not possible; how does D look like?
D

## Identity matrix 
help(diag)
A<-diag(5) # creates identity matrix of dimension 5x5
A

## Inverse of a square matrix, if exists
A <- matrix(c(1,2,3,4,5,6,9,1,2),nrow=3,ncol=3,byrow=TRUE)
B <- solve(A)
B%*%A
round(B%*%A)

## Generalized inverse of a rectangular matrix, if exists
library(MASS)
A <- matrix(c(1,2,3,9,1,2),nrow=3,ncol=2,byrow=TRUE)
B <- ginv(A)
B
B%*%A
round(B%*%A)
A%*%B
round(A%*%B)


