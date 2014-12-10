PROGRAM PROGTEST

USE HSL_EA20_double

IMPLICIT NONE

type (ea20_control)         :: cntl
type (ea20_info)            :: info
type (ea20_reverse)         :: rev

INTEGER(KIND=4),PARAMETER   :: IDIMV = 4
INTEGER(KIND=4),ALLOCATABLE :: SEEDA(:)
REAL(KIND=8)                :: NDMean, NDVar
REAL(KIND=8),ALLOCATABLE    :: NDNumb(:)
REAL(KIND=8),ALLOCATABLE    :: NDNumb2(:)
REAL(KIND=8),ALLOCATABLE    :: X(:,:)
REAL(KIND=8),ALLOCATABLE    :: A(:,:)

DIMENSION AB(4,4)
DOUBLE PRECISION AB,DET
 ! Parameters

integer, parameter :: wp = kind(0.0d0)    
integer, parameter :: n = 10
double precision, parameter :: zero = 0.0d0, one  = 1.0d0, two  = 2.0d0
integer :: i,ido
double precision, dimension(n) :: u
double precision, allocatable  :: w(:,:)
double precision               :: s

complex*16::ax(3,3),bx(3,3),cx(3,3)

data ax/10,2.5,(3,7),4,5,6,7,8,9/

  !! set u
  u = 0.d0    
  u(2:n-1) = one 

  !! set data
  s = 0.5d0

  !! set cntl
  cntl%d     = 3         !! delay
  cntl%tol   = 1.d-2     !! convergece tolerance
  cntl%maxit = 10        !! max number iteration

  cntl%diagnostics_level = 1 !! full error check

  ido = -1

  do while ( ido .ne. 0 .and. info%flag == 0)

     call EA20(n,u,s,ido,w,cntl,info,rev)

     select case ( ido )

     case (0)
        exit

     case (1) !! Matrix-Vector product w_out = A w(:,1)
        w(1,2) = two*w(1,1) - w(2,1)
        do i=2,n-1
           w(i,2) = -w(i-1,1)+two*w(i,1)-w(i+1,1)
        end do
        w(n,2) = -w(n-1,1)+two*w(n,1)

     case (2) !! Matrix-Vector product w(:,2) = M w(:,1)
        w(:,2) = w(:,1)

     case (3) !! Matrix-Vector product w_out = inv(M) w(:,1)
        w(:,2) = w(:,1)

     end select

  end do

  if (info%flag .ge. 0) then
     write(*,'(a,i5)') 'error code  = ',info%flag
     !! print the final solution
     write(*,'(a,i5)') 'number of iterations  = ',info%iter
     write(*,'(a,1x,1pe14.2)') 'estimated final error = ',info%error
     write(*,'(a)') '    i       X(i)'
     write(*,'(i5,1x,1pe14.3)') (i,u(i), i=1,n)
  end if
!AB = RESHAPE((/3.0,5.0,11.0,5.0,-3.0,-5.0,8.0,-1.0,-2.0,1.0,5.0,-3.0,4.0,8.0,-7.0,-1.0/),(/4,4/))
ALLOCATE(SEEDA(IDIMV*6))
ALLOCATE(NDNumb(IDIMV))
ALLOCATE(NDNumb2(IDIMV))
ALLOCATE(X(IDIMV,IDIMV))
ALLOCATE(A(IDIMV,IDIMV))

AB = RESHAPE((/3.0,5.0,11.0,5.0,-3.0,-5.0,8.0,-1.0,-2.0,1.0,5.0,-3.0,4.0,8.0,-7.0,-1.0/),(/4,4/))
!A = RESHAPE((/4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4/),(/6,6/))
A = RESHAPE((/4,4,4,4,4,4,4,4,4,4,4,4,4,4,4,4/),(/4,4/))

SEEDA(:) = 10

NDMean     = 0
NDVar      = 1
NDNumb(:)  = 0.0

CALL RANDOM_SEED(PUT=SEEDA(1:IDIMV*6))

CALL NDGen(SEEDA, NDMean, NDVar, IDIMV, NDNumb)

CALL NDGen(SEEDA, NDMean, NDVar, IDIMV, NDNumb2)

!CALL SQRTMAT(X,A,IDIMV)

CALL BSDET(AB,4,DET)

WRITE(*,10) DET
10 FORMAT(1X,'DET=',D15.6)

!PRINT*,NDNumb
!PRINT*,NDNumb2
PRINT*,A
PRINT*,X

! 测试利用lapack中的 zgetrf 和 zgetri 来求矩阵的逆


    print*,'ax='

    write(*,'(3(F8.4,"+i",F8.4,",  "))') , ax(1,:)

    write(*,'(3(F8.4,"+i",F8.4,",  "))') , ax(2,:)

    write(*,'(3(F8.4,"+i",F8.4,",  "))') , ax(3,:)



    bx=zInverse(3,ax)

    print*,'bx='

    write(*,'(3(F8.4,"+i",F8.4,",  "))') , bx(1,:)

    write(*,'(3(F8.4,"+i",F8.4,",  "))') , bx(2,:)

    write(*,'(3(F8.4,"+i",F8.4,",  "))') , bx(3,:)



    cx=matmul(ax,bx)

    print*,'cx='

    write(*,'(3(F8.4,"+i",F8.4,",  "))') , cx(1,:)

    write(*,'(3(F8.4,"+i",F8.4,",  "))') , cx(2,:)

    write(*,'(3(F8.4,"+i",F8.4,",  "))') , cx(3,:)



    contains



    function zInverse(n, a)  result(ra)

    integer::n,lda,ipiv(n),info,lwork

    complex*16::a(n,n),ra(n,n),work(n)

    ra=a

    lwork=n

    lda=n

    call zgetrf(n, n, ra, lda, ipiv, info)

    if(info/=0) write(0,*) 'Error occured in zgetrf!'

    call zgetri(n, ra, lda, ipiv, work, lwork, info)

    if(info/=0) write(0,*) 'Error occured in zgetri!'

    endfunction

END PROGRAM
