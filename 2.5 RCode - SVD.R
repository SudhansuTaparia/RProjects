
# Reference: http://www.statmethods.net/advstats/matrix.html

M=matrix(c(1,0,0,0,0,0,0,4,0,3,0,0,0,0,0,0,2,0,0,0),nrow=4,ncol=5)
M

X=svd(M)
X$u
X$d
X$v

X$u%*%diag(X$d)%*%t(X$v)
M