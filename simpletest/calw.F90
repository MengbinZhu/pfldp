SUBROUTINE CALW(Weights,RMat,SVX,OBSY,IDIMV,ITIMESTEP,NTIMESTEP,IDIMEn,NumOfObs,Method)

IMPLICIT NONE

INTEGER(KIND=4),INTENT(IN)  :: Method   ! Method = 1 SIR; 2 Optimal Proposal Density; 3 New Schme
INTEGER(KIND=4),INTENT(IN)  :: NumOfObs ! Number of the Observations
INTEGER(KIND=4),INTENT(IN)  :: IDIMEn   ! Dimension of Ensembles
INTEGER(KIND=4),INTENT(IN)  :: ITIMESTEP! Timestep of the Whole System
INTEGER(KIND=4),INTENT(IN)  :: NTIMESTEP! Timestep of the Whole System
INTEGER(KIND=4),INTENT(IN)  :: IDIMV    ! Dimension of the Variables
REAL(KIND=8),INTENT(IN)     :: OBSY(IDIMV,ITIMESTEP,NumOfObs) ! Observation Variable
REAL(KIND=8),INTENT(IN)     :: SVX(IDIMV,0:ITIMESTEP,IDIMEn)  ! State Vector Variable
REAL(KIND=8),INTENT(IN)     :: RMat(IDIMV,IDIMV)
REAL(KIND=8),INTENT(INOUT)  :: Weights(IDIMEn)                ! Weights for Every Particle

REAL(KIND=8)                :: Jo       ! OBS COST Function Wi = exp(-1/2 Jo) / Total P
INTEGER(KIND=4)             :: I,J,K    ! Index Variables
REAL(KIND=8)                :: RMatInv(IDIMV,IDIMV)
REAL(KIND=8)                :: Di(IDIMV,1)
REAL(KIND=8)                :: DiT(1,IDIMV)
REAL(KIND=8)                :: TMPNUM(1,1)
REAL(KIND=8)                :: USIGMA(IDIMEn) ! -2log(Wi) for Every Particle
REAL(KIND=8)                :: WWeight        ! The Weights of All Particles

TMPNUM = 0.0
USIGMA(:) = 0.0
WWeight = 0.0

SELECT CASE(Method)
CASE(1)
CALL MATRIXINV(RMat,RMatInv,IDIMV)
DO I = 1, IDIMEn

   DO J = 1, NumOfObs
      DO K = 1, IDIMV
         DiT(1,K) = OBSY(K,NTIMESTEP,J) - SVX(K,NTIMESTEP,I)
         Di(K,1)  = DiT(1,K)
      END DO
      TMPNUM = MATMUL(DiT,MATMUL(RMatInv,Di))
      !PRINT*,"TEST OF DIM OF TMPNUM"
      !PRINT*, MATMUL(DiT,MATMUL(RMatInv,Di))
      !PRINT*, MATMUL(Di,MATMUL(RMatInv,DiT))
      !PRINT*,"END OF THE TEST"
      USIGMA(I) = USIGMA(I) + TMPNUM(1,1)
   END DO
   WWeight = WWeight +EXP(-0.5*USIGMA(I))
END DO
Weights(:) = EXP(-0.5*USIGMA(:))/WWeight
PRINT*,"TEST OF SIR Weights:"
PRINT*,USIGMA(:)
PRINT*,"==========================="
PRINT*,WWeight
PRINT*,"&&&&&&&&&&&&&&&&&&&&&&&&&&"
PRINT*,Weights(:)
PRINT*,"END OF THE TEST"

CASE(2)

CASE(3)

CASE DEFAULT
    Weights(:) = 1/IDIMEn
    WRITE(*,*)"Now is the CASE of DEFAULT"
END SELECT

END SUBROUTINE
