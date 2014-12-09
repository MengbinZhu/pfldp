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
REAL(KIND=8)                :: TMPNUM2(1,1)
REAL(KIND=8)                :: USIGMA(IDIMEn)  ! -2log(Wi) for Every Particle
REAL(KIND=8)                :: USIGMA2(IDIMEn) ! -2log(Wi) for Every Particle
REAL(KIND=8)                :: TMPMat(NumOfObs,NumOfObs)
REAL(KIND=8)                :: TMPMatInv(NumOfObs,NumOfObs)
REAL(KIND=8)                :: WWeight         ! The Weights of All Particles
REAL(KIND=8)                :: EW              ! The Weight from Last Timestep
REAL(KIND=8)                :: ASSISTNUM

TMPNUM     = 0.0
TMPNUM2    = 0.0
USIGMA(:)  = 0.0
USIGMA2(:) = 0.0
WWeight    = 0.0
XVector    = 0.0

SELECT CASE(Method)
!============================SIR Method===============================
CASE(1) !SIR Method
CALL MATRIXINV(RMat,RMatInv,NumOfObs)
WWeight = 0.0
!PRINT*,"OBSY = "
!PRINT*,OBSY
DO I = 1, IDIMEn
   XVector(:,1) = SVX(:,NTIMESTEP,I) !Maybe some problems here about SVX!
   !PRINT*,"XVector ="
   !PRINT*,XVector
   HXMat = MATMUL(HMat,XVector)
   !PRINT*,"HXMat = "
   !PRINT*,HXMat
   DO J = 1, NumOfObs
      DiT(1,J) = OBSY(NTIMESTEP,J) - HXMat(J,1)
      Di(J,1)  = DiT(1,J)
   END DO
   TMPNUM = MATMUL(DiT,MATMUL(RMatInv,Di))
   USIGMA(I) = TMPNUM(1,1)
   !WWeight = WWeight +EXP(-0.5*USIGMA(I))
END DO
ASSISTNUM = 0.5*(SUM(USIGMA(:))/IDIMEn)
DO I = 1,IDIMEn
   WWeight = WWeight +EXP(-0.5*USIGMA(I)+ASSISTNUM)
END DO
Weights(:) = EXP(-0.5*USIGMA(:)+ASSISTNUM)/WWeight
!PRINT*,"WWeight = "
!PRINT*,WWeight
PRINT*,"TEST OF SIR Weights:"
PRINT*,"==========================="
PRINT*,Weights(:)
PRINT*,"END OF THE TEST"

!========================The Optimal Proposal Density====================
CASE(2) !The Optimal Proposal Density Method
    
HTMat(:,:) = 0.0
HTMat = TRANSPOSE(HMat)

!TMPMat = MATMUL(HMat,MATMUL(QMat,HTMat)) + RMat
TMPMat = QMat + RMat
CALL MATRIXINV(TMPMat,TMPMatInv,NumOfObs)

EW = IDIMEn
EW = 1/EW

DO I = 1, IDIMEn
   XVector(:,1) = SVX(:,NTIMESTEP-1,I) !Maybe some problems here about SVX!
   !PRINT*,"SVX(:,0,I) = "
   !PRINT*,XVector(:,1)
   HXMat = MATMUL(HMat,XVector)
   DO J = 1, NumOfObs
      DiT(1,J) = OBSY(NTIMESTEP,J) - HXMat(J,1)
      Di(J,1)  = DiT(1,J)
   END DO
   TMPNUM     = MATMUL(DiT,MATMUL(RMatInv,Di))
   USIGMA(I)  = TMPNUM(1,1)
   TMPNUM2    = MATMUL(DiT,MATMUL(TMPMatInv,Di))
   USIGMA2(I) = TMPNUM2(1,1)
   WWeight = WWeight +EXP(-0.5*USIGMA(I))
   !Weights(I) = (1/IDIMEn)*(EXP(-0.5*USIGMA(I))/EXP(-0.5*USIGMA2(I)))*(SQRT(RMat(1,1)/TMPMat(1,1)))
END DO
!PRINT*,"USIGMA2 = "
!PRINT*,USIGMA2(:)
ASSISTNUM = 0.5*(SUM(USIGMA2(:))/IDIMEn)
Weights(:) = EXP(-0.5*USIGMA2(:)+ASSISTNUM)
Weights(:) = Weights(:)/SUM(Weights) !Relative Weights Here
PRINT*,"TEST OF THE OPTIMAL PROPOSAL DENSITY WEIGHTS"
PRINT*,"============================================"
PRINT*,Weights(:)
PRINT*,"END OF THE OPTIMAL PROPOSAL DENSITY WEIGHTS"

!==============================The New Scheme================================
CASE(3) !The New Scheme Method

CASE DEFAULT
    Weights(:) = 1/IDIMEn
    WRITE(*,*)"Now is the CASE of DEFAULT"
END SELECT

END SUBROUTINE
