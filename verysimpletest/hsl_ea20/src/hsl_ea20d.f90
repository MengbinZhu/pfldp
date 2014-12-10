! COPYRIGHT (c) 2010 Council for the Central Laboratory
!               of the Research Councils
! Written by:  Mario Arioli
!
! Version 1.1.0
!
! See ChangeLog for version history.

!!!!!
! To generate the single precision version from the double version:
! (1) globally change _double to _single
! (2) change 0.0d0 to 0.0
! (3) globally change ddot, dgemv, and dpteqr to 
!     sdot, spteqr, and sgemv respectively.
!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!


module hsl_ea20_double

  implicit none

  !*************************************************
  ! Parameters (all private)
  ! All integers are declared to be either short or long
  integer, parameter, private  :: wp = kind(0.0d0)
  real (wp), parameter, private :: one = 1.0_wp
  real (wp), parameter, private :: zero = 0.0_wp
  !*************************************************
  !  provide control and inform values for Matlab interfaces to QPC

  interface EA20_double
     module procedure EA20
  end interface EA20_double

  !--------------------------
  !  Derived type definitions
  !--------------------------

  type EA20_control ! This type controls the printing level
     real(wp)       :: tol
     integer        :: d = 3          ! delay: we stop if 
                                      !|fnorm(i+d) - fnorm(i)| <=
                                      !       tol*fnorm(i)
     integer        :: maxit = 20     ! max number of Lanzos vectors
     integer        :: diagnostics_level = 1 
     ! Controls diagnostic .
     ! printing. Possible values are:
     ! < 0: no printing.
     !   0: error and warning messages only.
     ! > 1: as 1 plus some more detailed diagnostic msgs.
     integer        :: unit_diagnostics = 6 
     ! unit number for
     ! diagnostic printing.
     ! Printing is suppressed if unit_diagnostics  < 0.

     integer        :: unit_error = 6 
     ! unit number for error 
     ! messages. Printing is suppressed if unit_error  < 0.
     integer        :: unit_warning = 6 
     ! unit number for warning 
     ! messages. Printing is suppressed if unit_warning < 0
  end type EA20_control
  type EA20_info    !information
     integer        :: iter
     integer        :: flag = 0  
     ! exit status
     ! Value zero after successful entry.
     !            Possible error returns:
     !              -1 data%n < 0
     !              -2 size(data%u) < data%n
     !              -3 Allocation error in w(:,1) or 
     !                 work%out
     !              -4 Allocation error in work%v. If 
     !                 cntl%diagnostics_level > 0 a 
     !                 diagnostic msg is printed
     !              -5 Allocation error for the internal  
     !                 work space. If 
     !                 cntl%diagnostics_level > 0 a    
     !                 diagnostic msg is printed  
     !              -6 Allocation error for the small 
     !                 vectors and matrices. If 
     !                 cntl%diagnostics_level > 0 a    
     !                 diagnostic msg is printed 
     !              -7 info%LAP < 0. Impossible to compute 
     !                 eigenvalues and eigenvectors of the 
     !                 tridiagonal matrix
     !              -8 info%LAP > 0. Impossible to compute 
     !                 eigenvalues and eigenvectors of the 
     !                 tridiagonal matrix
     !              -9 Deallocation error
     !             -10 data%tol out of range (i.e. less than 
     !                 -1 or greater than 1)
     !             -11 data%d < 0
     !             -12 s less than or equal to -1, or, 
     !                 greater than or equal to 1)
     !            Possible Warning: 
     !               1 cntl%maxit is too small and the 
     !                 iterative processs does not 
     !                 converge. The solution can be 
     !                 inaccurate  

     integer        :: stat = 0  
     ! exit status
     ! Value zero after successful allocation or 
     ! deallocation. A positive value indicates 
     ! insufficient memory.          

     integer        :: LAP
     ! exit status of the LAPACK and BLAS routines
     ! Value zero after successful call.
     !            Possible error returns:
     !                      = 0:  successful exit.
     !          < 0:  if info%LAP = -i, 
     !           the i-th argument had an illegal value.
     !          > 0:  if info%LAP = i, and i is:
     !               <= N  the Cholesky factorization 
     !                     of the tridiagonal matrix could
     !                     not be performed because the 
     !                     matrix was not positive definite.
     !               > N   the SVD algorithm failed 
     !                     to converge; See LAPACK manual
     !                      for DPTEQR
     real(wp)       :: error = one           ! error estimate
  end type EA20_info

  type  EA20_reverse 
     !The scalar reverse of this  
     ! type holds data that is passed between subroutines
     integer              :: action
     integer              :: iter = 0      ! num. of Lanczos iterations
     logical              :: conv = .false.
     real(wp),allocatable :: fnorm(:)      ! norm estimates         
     real(wp),allocatable :: alpha(:)
     real(wp),allocatable :: beta(:)
     real(wp),allocatable :: d(:)
     real(wp),allocatable :: e(:)
     real(wp),allocatable :: e1(:)
     real(wp),allocatable :: e2(:)  
     real(wp),allocatable :: r(:)
     real(wp),allocatable :: p(:)
     real(wp),allocatable :: LAP(:) 
     real(wp),allocatable :: z(:,:)        ! Tridiagonal matrix
     real(wp),allocatable :: v(:,:)        ! Lanczos vectors
  end type EA20_reverse

contains


  subroutine EA20(n,u,s,ido,w,cntl,info,rev)

    type (EA20_control), intent (in)    :: cntl 
    ! See derived-type declaration
    type (EA20_info), intent (out)      :: info    
    ! See derived-type declaration
    type (EA20_reverse), intent(inout)  :: rev 
    ! See derived-type declaration

    ! external variables
    integer,intent(in)     :: n
    real(wp),intent(inout) :: u(:)  ! in OUTPUT the final solution
    real(wp),intent(in)    :: s
    integer,intent(inout)  :: ido   !Action required by user
                                    ! = -1  Initialisation 
                                    ! = 0   end of process 
                                    ! = 1   matrix A by vector
                                    ! = 2   matrix M by vector
                                    ! = 3   inverse of M by vector
    real(wp),allocatable,intent(inout) :: w(:,:) ! workspace

    !local variable
    real(wp) :: err,eps,temp
    integer  :: i,j
    integer  :: st

    intrinsic :: sqrt,abs
    real(wp),external :: ddot
    external :: dpteqr, dgemv

    eps = epsilon(one)
    if (ido == -1) then
       rev%iter = 0
    end if
    if (n .le. 0) then 
       info%flag = -1; call error_open
       return
    end if
    if (size(u) < n) then
       info%flag = -2; call error_open
       return
    end if
    if ((cntl%tol < 0) .or. (cntl%tol > 1)) then
       info%flag = -10; call error_open
       return
    end if
    if (cntl%d < 1) then
       info%flag = -11; call error_open
       return
    end if
    if ((s <= -1) .or. (s >= 1)) then
       info%flag = -12; call error_open
       return
    end if

    if (rev%iter == 0) then
       
       allocate(w(n,2),stat=st)
       if (st /= 0) then
          info%flag = -3; info%stat = st
          if (cntl%diagnostics_level >= 0) then 
             call error_open; 
          end if
          ido = 0
          return
       end if
       call initAlloc(cntl,info,rev)
       if (info%flag .ne. 0) then
          if (cntl%diagnostics_level >= 0) then 
             call error_open; 
          end if
          ido = 0
          return
       end if
       rev%fnorm = 0
       rev%action = -1
       rev%conv = .false.     
    end if

    do while (.not. rev%conv)
       select case (rev%action)

       case (-1)
          ido = 2          !The user must provide w_out= M*w_inp
          rev%iter = 1
          w(:,1) = u
          rev%action = 0
          return
       case (0)
          temp = ddot(n,w(:,2),1,w(:,1), 1)
          if (temp < zero) then
             info%flag = -7; ido = 0
             call error_open
             return
          end if
          rev%beta(rev%iter) = sqrt(temp)
          if (rev%beta(rev%iter) > eps) then
             rev%v(:,rev%iter) = w(:,1) / rev%beta(rev%iter)
             rev%r = w(:,2)
             rev%action = 1
          else
             ido = 0
             info%iter = rev%iter
             info%error = eps
             call finalization(w,rev)
             rev%conv = .true.
             return
          end if
       case (1)
          ido = 1      ! The user must provide w_out = A w_inp
          w(:,1) = rev%v(:,rev%iter)
          rev%action = 2
          return
       case (2)
          if (rev%iter == 1) then
             w(:,1) = w(:,2)
          else
             w(:,1) = w(:,2) - rev%beta(rev%iter) * rev%p
          end if
          rev%alpha(rev%iter) = ddot(n,w(:,1),1,rev%v(:,rev%iter),1)
          if ((rev%alpha(rev%iter)) <= zero) then
             info%flag = -7; ido = 0
             call error_open
             call finalization(w,rev)
             return
          end if
          rev%p = rev%r / rev%beta(rev%iter)
          w(:,1) = w(:,1)  - rev%alpha(rev%iter) * rev%p
          rev%r = w(:,1)
          rev%action = 3
          ido = 3      !The user must provide w_out= inv(M)*w_inp
          return
       case (3)
          if (rev%iter < cntl%maxit) then
             temp = ddot(n,w(:,2),1,w(:,1),1)
             if (temp < zero) then  !correction < substitutes <= (17 July 2013)
                info%flag = -7; ido = 0
                call error_open
                call finalization(w,rev)
                return
             end if
             rev%beta(rev%iter+1) = sqrt(temp)
             if (rev%beta(rev%iter+1) .ne. 0) then
                rev%v(:,rev%iter+1) = w(:,2) / rev%beta(rev%iter+1)
             else                   ! if beta = 0 we have the solution!17/7/13
                call autoval
                if (info%LAP .ne. 0)  return
                info%error = 0
                ido = 0; rev%conv = .true.; info%iter = rev%iter
                call solution
                call finalization(w,rev) 
                return
             end if
             call autoval
             if (info%LAP .ne. 0)  return
             call norma
             ! start the error check
             if (rev%iter > cntl%d) then
                err = abs(rev%fnorm(rev%iter-cntl%d)-rev%fnorm(rev%iter)) 
                info%error = err
                if (err < (cntl%tol*rev%fnorm(rev%iter))) then
                   ido = 0; rev%conv = .true.; info%iter = rev%iter
                   info%error = info%error /rev%fnorm(rev%iter)
                   call solution
                   call finalization(w,rev) 
                   return
                else
                   rev%iter = rev%iter + 1
                   rev%action = 1
                end if
             else
                rev%iter = rev%iter + 1
                rev%action = 1
             end if
          else
             info%flag = 1   ! max num. iter.s achieved
             info%iter = cntl%maxit
             ido = 0 
             call autoval    ! coumpute eigensolution
             if (info%LAP .ne. 0)  return
             call solution
             call error_open
             call finalization(w,rev)
             return
          end if

       end select
    end do

  contains

    subroutine autoval
!!!!!!!!!!!!!!!!!
!! This routine compute the eigenvalues 
!! eigenvector of the tridiagonal using LAPACK
!! The rev%alpha and rev% beta are passed to 
!! rev%d rev%e because _PTEQR destroy the input
!!!!!!!!!!!!!!!!!
      rev%d(1:rev%iter) = rev%alpha(1:rev%iter)
      if (rev%iter > 1) then
         do i = 2 , rev%iter
            rev%e(i-1) = rev%beta(i)
         end do
      end if
      call dpteqr('I', rev%iter, rev%d, rev%e, rev%z, &
           & cntl%maxit, rev%LAP,info%LAP)

      if (info%LAP .ne. 0) then
         info%flag = -8
         call error_open; 
      end if
    end subroutine autoval

    subroutine norma
!!!!!!!!!!!!!!!!!
      !! compute an approximation of the energy norm of the
      !! s-root of the pencil applied to u
!!!!!!!!!!!!!!!!!
      rev%e1(1:rev%iter) = rev%z(1,1:rev%iter)
      do j=1,rev%iter
         rev%e1(j) = rev%e1(j) * (rev%d(j)**s)
      end do

      do i=1,rev%iter
         rev%fnorm(rev%iter) = rev%fnorm(rev%iter) + &
              & (rev%d(i))**s * rev%z(1,i)**2
      end do
      rev%fnorm(rev%iter) = sqrt(rev%fnorm(rev%iter)*rev%beta(1)**2)

    end subroutine norma


    subroutine solution
!!!!!!!!!!!!!!!!!
!! Thic routine compute the solution
!!!!!!!!!!!!!!!!!

             rev%e1(1:rev%iter) = rev%z(1,1:rev%iter)
             do j=1,rev%iter
                rev%e1(j) = rev%e1(j) * (rev%d(j)**s)
             end do
             call dgemv('n', rev%iter, rev%iter, rev%beta(1), rev%z, &
                  cntl%maxit, rev%e1, 1, zero, rev%e2, 1)

             call dgemv('n', n, rev%iter, one, rev%v, n, & 
                  & rev%e2, 1, zero, u, 1)

    end subroutine solution
    
    subroutine error_open

      if (cntl%diagnostics_level >= 0) then

         select case (info%flag)

            !errors
         case (-1)
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A, I2)') 'ERROR_msg (EA20):&
                    & n < 0.',' info%flag = ',info%flag
            end if

         case (-2)
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A, I2)') 'ERROR_msg (EA20):&
                    & the vector u given in u has size greater than n.'&
                    &, 'info%flag = ', info%flag
            end if

         case (-3)
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A, I2)') 'ERROR_msg (EA20):&
                    & failure in &
                    & allocating the work space w.', &
                    &' info%flag = ', info%flag
               if ((cntl%unit_diagnostics > 0) .and. & 
                    & (cntl%diagnostics_level > 0)) then 
                  write(cntl%unit_diagnostics,'(A,/,A,/,A,/,A)')&
                       &'ERROR_msg diagnostic:', &
                       &  'Reducing the value of cntl%maxit can allow', &
                       &  'the code to work.',& 
                       &  'Check if w is not already allocated.'
               end if
            end if

         case (-4)
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A, I2)') 'ERROR_msg (EA20):&
                    & failure in allocating rev%v the Krylov space basis.', &
                    &' info%flag = ', info%flag
               if ((cntl%unit_diagnostics > 0) .and. & 
                    & (cntl%diagnostics_level > 0)) then 
                  write(cntl%unit_diagnostics,'(A,/,A,/A)') &
                       & 'ERROR_msg diagnostic:', &
                       & 'Reducing the value of cntl%maxit can allow&
                       &  the code to work.','  &
                       &Check if rev%v is not already allocated.'
               end if
            end if

         case (-5) 
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A, I2)') 'ERROR_msg (EA20):&
                    & failure in allocating internal working space.', &
                    &' info%flag = ', info%flag
               if ((cntl%unit_diagnostics > 0) .and. & 
                    & (cntl%diagnostics_level > 0)) then 
                  write(cntl%unit_diagnostics,'(A,/,A,/,A,/A)') 'ERROR_msg & 
                       &diagnostic:', &
                       &  'Reducing the value of cntl%maxit can allow',&
                       &  'the code to work.',&
                       & 'Check if rev%p or rev%r are already allocated'
               end if
            end if

         case (-6)
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A,/,A, I2)') 'ERROR_msg (EA20):',&
                    &'failure in allocating the small vectors and matrix& 
                    & for the tridiagonal T.', &
                    &' info%flag = ', info%flag
            end if

         case (-7)
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A, I2)') 'ERROR_msg (EA20):&
                    & the pencil (A,M) is not spd.',' info%flag = ', info%flag
            end if

         case (-8)
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A,/,A, I2)') 'ERROR_msg from DPTQR',&
                    &' A may be non positive definite', &
                    &' info%flag = ', info%flag
               if ((cntl%unit_diagnostics > 0) .and. & 
                    & (cntl%diagnostics_level > 0)) then
                  if (info%LAP < rev%iter) then
                     write(cntl%unit_diagnostics,'(A,/,A,I2)') 'ERROR_msg: & 
                          & diagnostic the tridiagonal matrix is not & 
                          & positive definite.', ' info%LAP =', info%LAP
                  end if
                  if (info%LAP > rev%iter) then
                     write(cntl%unit_diagnostics,'(A,/,A,I2)') 'ERROR_msg: &
                          & the SVD algorithm failed to converge.',      &
                          & 'See LAPACK manual for DPTEQR. info%LAP =', info%LAP
                  end if
               end if

            end if

         case (-9)
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A, I2)') 'ERROR_msg (EA20):&
                    & deallocation problems.',' info%flag = ', info%flag  
            end if

         case (-10)
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A,I3)') 'ERROR_msg (EA20):& 
                    & tolerance out of range 0 < cntl%tol < 1.',&
                    &' info%flag = ', info%flag
            end if

         case (-11) 
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A,I3)') 'ERROR_msg (EA20):&
                    & d out of range  1 <= cntl%d.', &
                    &' info%flag = ', info%flag
            end if

         case (-12) 
            if (cntl%unit_error > 0) then
               write(cntl%unit_error,'(A,/,A,I3)') 'ERROR_msg (EA20):&
                    & s out of range  -1 >= s or s >= 1.', &
                    & 'info%flag = ', info%flag
            end if

            !warning
         case (1) 
            if (cntl%unit_warning > 0) then
               write(cntl%unit_warning,'(A,/,A,/,A,/,A, I2)')&
                    & 'Warning_msg (EA20):',&
                    & 'cntl%maxit is too small and the iterative process', &
                    & 'does not converge. The solution can be inaccurate. ',&
                    &' info%flag = ', info%flag
            end if

         end select

      else
         return
      end if

    end subroutine error_open

    subroutine finalization(w,rev)
      type (EA20_reverse), intent(inout)  :: rev 
      ! See derived-type declaration
      real(wp),allocatable,intent(inout)  :: w(:,:) ! workspace
          deallocate(w,stat=st)

          deallocate(rev%p,rev%r,rev%v,rev%alpha,rev%beta,&
               &rev%d,rev%e,rev%z,rev%LAP,rev%fnorm,rev%e1, &
               & rev%e2,stat=st)
          if (st /= 0) then
             info%flag = -9; info%stat = st
             call error_open; 
          end if

    end subroutine finalization
    
    subroutine initAlloc(cntl,info,rev)
      
      type (EA20_control), intent (in)    :: cntl 
      ! See derived-type declaration
      type (EA20_info), intent (out)      :: info    
      ! See derived-type declaration
      type (EA20_reverse), intent(inout)  :: rev 
      ! See derived-type declaration
!      real(wp),allocatable,intent(inout)  :: w(:,:) ! workspace
      integer :: st


      allocate(rev%v(n,cntl%maxit),stat=st)
      if (st /= 0) then
         info%flag = -4; info%stat = st
         return
      end if
      allocate(rev%r(n),stat=st)
      if (st /= 0) then
         info%flag = -5; info%stat = st
         return
      end if
      allocate(rev%p(n),stat=st)
      if (st /= 0) then
         info%flag = -5; info%stat = st
         return
      end if
      allocate(rev%alpha(cntl%maxit),stat=st)
      if (st /= 0) then
         info%flag = -6; info%stat = st
         return
      end if
      allocate(rev%beta(cntl%maxit),stat=st)
      if (st /= 0) then
         info%flag = -6; info%stat = st
         return
      end if
      allocate(rev%d(cntl%maxit),stat=st)
      if (st /= 0) then
         info%flag = -6; info%stat = st
         return
      end if
      allocate(rev%e(cntl%maxit),stat=st)
      if (st /= 0) then
         info%flag = -6; info%stat = st
         return
      end if
      allocate(rev%z(cntl%maxit,cntl%maxit),stat=st)
      if (st /= 0) then
         info%flag = -6; info%stat = st
         return
      end if
      allocate(rev%LAP(4*cntl%maxit),stat=st)
      if (st /= 0) then
         info%flag = -6; info%stat = st
         return
      end if
      allocate(rev%fnorm(cntl%maxit),stat=st)
      if (st /= 0) then
         info%flag = -6; info%stat = st
         return
      end if
      allocate(rev%e2(cntl%maxit),stat=st)
      if (st /= 0) then
         info%flag = -6; info%stat = st
         return
      end if
      allocate(rev%e1(cntl%maxit),stat=st)
      if (st /= 0) then
         info%flag = -6; info%stat = st
         return
      end if
      
    end subroutine initAlloc
  end subroutine EA20

end module HSL_EA20_double
