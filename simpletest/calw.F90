SUBROUTINE CALW(Weights,QMat,RMat,HMat,SVX,OBSY,IDIMV,ITIMESTEP,NTIMESTEP,IDIMEn,NumOfObs,Method)

IMPLICIT NONE

INTEGER(KIND=4),INTENT(IN)  :: Method   ! Method = 1 SIR; 2 Optimal Proposal Density; 3 New Schme
INTEGER(KIND=4),INTENT(IN)  :: NumOfObs ! Number of the Observations
INTEGER(KIND=4),INTENT(IN)  :: IDIMEn   ! Dimension of Ensembles
INTEGER(KIND=4),INTENT(IN)  :: ITIMESTEP! Timestep of the Whole System
INTEGER(KIND=4),INTENT(IN)  :: NTIMESTEP! Timestep of the Whole System
INTEGER(KIND=4),INTENT(IN)  :: IDIMV    ! Dimension of the Variables
REAL(KIND=8),INTENT(IN)     :: OBSY(ITIMESTEP,NumOfObs) ! Observation Variable
REAL(KIND=8),INTENT(IN)     :: SVX(IDIMV,0:ITIMESTEP,IDIMEn)  ! State Vector Variable
REAL(KIND=8),INTENT(IN)     :: QMat(IDIMV,IDIMV)
REAL(KIND=8),INTENT(IN)     :: RMat(NumOfObs,NumOfObs)
REAL(KIND=8),INTENT(IN)     :: HMat(NumOfObs,IDIMV)
REAL(KIND=8),INTENT(INOUT)  :: Weights(IDIMEn)                ! Weights for Every Particle

REAL(KIND=8)                :: Jo       ! OBS COST Function Wi = exp(-1/2 Jo) / Total P
INTEGER(KIND=4)             :: I,J,K    ! Index Variables
REAL(KIND=8)                :: RMatInv(NumOfObs,NumOfObs)
REAL(KIND=8)                :: HTMat(IDIMV,NumOfObs)
REAL(KIND=8)                :: HXMat(NumOfObs,1)
REAL(KIND=8)                :: XVector(IDIMV,1)
REAL(KIND=8)                :: DiT(1,NumOfObs)
REAL(KIND=8)                :: Di(NumOfObs,1)
REAL(KIND=8)                :: TMPNUM(1,1)
REAL(KIND=8)                :: USIGMA(IDIMEn)  ! -2log(Wi) for Every Particle
REAL(KIND=8)                :: TMPMat(NumOfObs,NumOfObs)
REAL(KIND=8)                :: TMPMatInv(NumOfObs,NumOfObs)
!Variables Defined Below have dimension problems.
REAL(KIND=8)                :: TMPNUM2(1,1)
REAL(KIND=8)                :: USIGMA2(IDIMEn) ! -2log(Wi) for Every Particle
REAL(KIND=8)                :: WWeight         ! The Weights of All Particles
REAL(KIND=8)                :: EW              ! The Weight from Last Timestep

TMPNUM     = 0.0
TMPNUM2    = 0.0
USIGMA(:)  = 0.0
USIGMA2(:) = 0.0
WWeight    = 0.0
XVector    = 0.0

SELECT CASE(Method)
CASE(1) !SIR Method
CALL MATRIXINV(RMat,RMatInv,NumOfObs)
WWeight = 0.0
!PRINT*,"OBSY = "
!PRINT*,OBSY(1,:)
DO I = 1, IDIMEn
   XVector(:,1) = SVX(:,NTIMESTEP,I)
   !PRINT*,"XVec ="
   !PRINT*,XVector(:,1)
   HXMat = MATMUL(HMat,XVector)
   !PRINT*,"HXMAT =" 
   !PRINT*,HXMat
   DO J = 1, NumOfObs
      DiT(1,J) = OBSY(NTIMESTEP,J) - HXMat(J,1)
      Di(J,1)  = DiT(1,J)
      !PRINT*,"Di = "
      !PRINT*,Di(J,1)
   END DO
   TMPNUM = MATMUL(DiT,MATMUL(RMatInv,Di))
   USIGMA(I) = TMPNUM(1,1)
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

CASE(2) !The Optimal Proposal Density Method
    
CALL MatTrans(HMat,HTMat,NumOfObs,IDIMV)
!Start from Here 17:47 Dec 02 2014
TMPMat = MATMUL(HMat,MATMUL(QMat,HTMat)) + RMat
CALL MATRIXINV(TMPMat,TMPMatInv,IDIMV)

DO I = 1, IDIMEn

   DO J = 1, NumOfObs
      DO K = 1, IDIMV
        ! DiT(1,K) = OBSY(K,NTIMESTEP,J) - SVX(K,NTIMESTEP,I)
        ! Di(K,1)  = DiT(1,K)
      END DO
      !TMPNUM = MATMUL(DiT,MATMUL(TMPMatInv,Di))
      !USIGMA(I) = USIGMA(I) + TMPNUM(1,1)
      !TMPNUM2 = MATMUL(DiT,MATMUL(RMatInv,Di))
      !USIGMA2(I) = USIGMA2(I) + TMPNUM2(1,1)
   END DO
   !WWeight = WWeight +EXP(-0.5*USIGMA2(I))
   !Weights(I) = (1/IDIMEn)*(EXP(-0.5*USIGMA(I))/EXP(-0.5*USIGMA2(I)))*(SQRT(RMat(1,1)/TMPMat(1,1)))
   EW = IDIMEn
   EW = 1/EW
   !PRINT*,"==================================="
   !PRINT*,EXP(-0.5*USIGMA(I))
   !PRINT*,EXP(-0.5*USIGMA2(I))
   !PRINT*,EW
   !Weights(I) = EXP(-0.5*USIGMA(I))/WWeight
END DO
PRINT*,"TEST OF THE OPTIMAL PROPOSAL DENSITY WEIGHTS"
PRINT*,"============================================"
!PRINT*,Weights(:)
PRINT*,"END OF THE OPTIMAL PROPOSAL DENSITY WEIGHTS"

CASE(3)

CASE DEFAULT
    Weights(:) = 1/IDIMEn
    WRITE(*,*)"Now is the CASE of DEFAULT"
END SELECT

END SUBROUTINE
