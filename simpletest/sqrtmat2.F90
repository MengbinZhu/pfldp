      subroutine sqrtmat2(XX,AA,N)

      implicit none

      integer*8 N
      
      !integer*8 :: LDA = N
      !integer*8 :: LDVS = 2 * N
      !integer*8 :: LWORK = 3 * N
!c
!c     Input.
!c
      real*8 AA(N,N)
!c
!c     Ouput.
!c
      real*8 XX(N,N)
!c
!c     Local variables.
!c
      complex*16 A(N,N), VS(2*N,N)
      complex*16 WORK(3*N), W(N)

      double precision RWORK(N)
      integer*8 SDIM,INFO

      logical*8 BWORK(1:N)
      logical DUMMY_SELECT2
      external DUMMY_SELECT2

      integer*8   i,j,k
      complex*16  Q(N,N), QR(N,N), QH(N,N), R(N,N), X(N,N)
      complex*16  summ
!c
!c     Copy AA into the complex matrix A.
!c
      do j = 1,N
         do i = 1,N
            A(i,j) = dcmplx(AA(i,j))
         enddo
      enddo
!c
!c     Compute the eigenvalues and Schur vectors of A.
!c
      call ZGEES('V','N',DUMMY_SELECT2, N, A, N, SDIM, W, VS, 2*N, WORK,3*N, RWORK, BWORK, INFO)
!c
!c     Compute the square root R of T one column at a time.
!c
      do j = 1,N
         do i = 1,N
            R(i,j)=0.0
         enddo
      enddo
      summ=0.0
      do j = 1,N
         R(j,j) = sqrt(A(j,j))
         do i = j-1,1,-1
            do k = i+1,j-1
               summ = summ + R(i,k)*R(k,j)
            enddo
            R(i,j) = (A(i,j) - summ)/(R(i,i) + R(j,j))
         enddo
      enddo
!c
!c     Copy VS into Q.
!c
      do j = 1,N
         do i = 1,N
            Q(i,j) = VS(i,j)
         enddo
      enddo
!c
!c     Set X to be the square root of A : X = real(Q*R*Q**H).
!c
      do j = 1,N
         do i = 1,N
            QH(i,j) = CONJG(Q(j,i))
         enddo
      enddo
      QR = MATMUL(Q,R)
      X = MATMUL(QR,QH)
!c
!c     Set XX to be the real part of X.
!c
      do j = 1,N
         do i = 1,N
            XX(i,j) = dble(X(i,j))
         enddo
      enddo
!c
      return
      end
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
!c     Dummy select function.
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc
!c
      LOGICAL FUNCTION DUMMY_SELECT2(ARG)
!c
      IMPLICIT NONE
!c
!c     Input.
!c
      complex*16 ARG
!C
!C     The value computed is always .TRUE.  It is computed in the
!C     peculiar way below to avoid compiler messages about unused
!C     function arguments.
!c
      PRINT 1000
      DUMMY_SELECT2 = (ARG .EQ. ARG)
 1000 FORMAT (///1X, '**** ERROR:  ','DUMMY_SELECT FUNCTION CALLED BUT NOT AVAILABLE. ****')
!c
      stop
      end
!c
!ccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccccc

