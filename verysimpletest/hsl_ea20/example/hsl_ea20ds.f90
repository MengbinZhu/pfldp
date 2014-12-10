program main

  ! Simple code to illustrate row entry to hsl_ea20
  use HSL_EA20_double
  implicit none

  ! Derived types
  type (ea20_control) :: cntl
  type (ea20_info)    :: info
  type (ea20_reverse) :: rev

  ! Parameters

  integer, parameter :: wp = kind(0.0d0)    
  integer, parameter :: n = 10
  double precision, parameter :: zero = 0.0d0, one  = 1.0d0, two  = 2.0d0
  integer :: i,ido
  double precision, dimension(n) :: u
  double precision, allocatable  :: w(:,:)
  double precision               :: s

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


end program main
