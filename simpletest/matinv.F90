SUBROUTINE MATINV(A,RA,N)

IMPLICIT NONE

INTEGER    :: N,lda,ipiv(N),info,lwork
COMPLEX*16 :: A(N,N),RA(N,N),work(N)

RA=A
lwork=N
lda=N

CALL ZGETRF(N,N,RA,lda,ipiv,info)
IF(info/=0) write(0,*) 'ERROR OCCURED IN ZGETRF!'

CALL ZGETRI(N,RA,lda,ipiv,work,lwork,info)
IF(info/=0) write(0,*) 'ERROR OCCURED IN ZGETRI'

END SUBROUTINE
