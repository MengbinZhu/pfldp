PROGRAM SIMEXP

IMPLICIT NONE

INTEGER(KIND=4),PARAMETER   :: IDIMEn = 10    ! Number of Ensemble Size
INTEGER(KIND=4),PARAMETER   :: IDIMV  = 1     ! Size Number of the State Variables
INTEGER(KIND=4),PARAMETER   :: ITIMESTEP  = 1 ! Number of Time Steps
INTEGER(KIND=4),PARAMETER   :: NumOfObs = 1   ! Number of Observations
INTEGER(KIND=4),ALLOCATABLE :: SEEDA(:)       ! Seed of the Random Number
REAL(KIND=8)                :: NDMean, NDVar  ! Test need Mean Value and Var Value
REAL(KIND=8),ALLOCATABLE    :: NDNumb(:)      ! Test need Output Random Number Vector
REAL(KIND=8),ALLOCATABLE    :: ETA(:)         ! The Model Error N(0,Q)
REAL(KIND=8),ALLOCATABLE    :: EPSIL(:)       ! The Observation Error N(0,R) EPSIL(NumOfObs)

INTEGER(KIND=4)             :: I,J,K

!Step 0. System Perference About the PARAMETERs
REAL(KIND=8),ALLOCATABLE    :: BMat(:,:)      !Background Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: RMat(:,:)      !Observation Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: QMat(:,:)      !System Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: HMat(:,:)      !Observation Operator Matrix
REAL(KIND=8),ALLOCATABLE    :: GMat(:,:)      !Matrix For the Calculation of G(X*) 
REAL(KIND=8),ALLOCATABLE    :: PMat(:,:)      !Matrix For the Calculation of P^{-1} Inverse of P
REAL(KIND=8),ALLOCATABLE    :: SVX(:,:,:)     !State Vector Variable X(Dim, TimeSeq, EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: OBSY(:,:,:)    !Observation Vector Variable Y(Dim, TimeSeq,NumOfObs)
REAL(KIND=8),ALLOCATABLE    :: dist(:,:)      !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: PHI(:,:)       !The function of {\phi}_i
REAL(KIND=8),ALLOCATABLE    :: SI(:,:)        !The function of S_i

!Step 1. Initialize the B, R, Q Matrices and SVX(IDIMV,0)
ALLOCATE(BMat(IDIMV,IDIMV))
ALLOCATE(RMat(IDIMV,IDIMV))
ALLOCATE(QMat(IDIMV,IDIMV))
ALLOCATE(HMat(IDIMV,IDIMV))
ALLOCATE(GMat(IDIMV,IDIMV))
ALLOCATE(PMat(IDIMV,IDIMV))

ALLOCATE(SVX(1:IDIMV,0:ITIMESTEP,1:IDIMEn))
ALLOCATE(OBSY(1:IDIMV,1:ITIMESTEP,1:NumOfObs))
ALLOCATE(dist(1:IDIMV,1:IDIMEn))
ALLOCATE(PHI(1:IDIMV,1:IDIMEn))
ALLOCATE(SI(1:IDIMV,1:IDIMEn))

!! 1.1 Init B, R, Q
BMat(:,:) = 0.0
RMat(:,:) = 0.0
QMat(:,:) = 0.0
HMat(:,:) = 1.0

DO I = 1, IDIMV
   BMat(I,I) = 1.00
   RMat(I,I) = 0.16
   QMat(I,I) = 0.01
END DO

!! 1.2 Init the SVX(:,0)
SVX(1:IDIMV,0,1:IDIMEn) = 0.0   !Take it as the Truth

! Step 2. Generate the Ensemble Member

ALLOCATE(SEEDA(IDIMEn)) !ALLOCATE the SEED of Random Number
ALLOCATE(NDNumb(IDIMEn))!ALLOCATE the Output
SEEDA(:) = 10           !Initialize the Seed 

!! 2.1 Generate the X(Dim,0,Ens)
NDMean    = 0
NDVar     = BMat(1,1)
NDNumb(:) = 0.0 
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn, NDNumb)
DO I = 1,IDIMV
   SVX(I,0,1:IDIMEn) = NDNumb(1:IDIMEn)  !Every dimension of X is the same.
   PRINT *,"The initial state vector X at timestep 0 :"
   PRINT *,SVX(I,0,:)
END DO

!! 2.X Additional Step for the evolution of the Model
ALLOCATE(ETA(IDIMEn))
NDMean    = 0
NDVar     = QMat(1,1)
ETA(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn, ETA)

!! 2.2 Generate the Y(Dim, ITIMESTEP, NumOfObs)
ALLOCATE(EPSIL(NumOfObs))
NDMean   = 0
NDVar    = RMat(1,1)
EPSIL(:) = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, NumOfObs, EPSIL)
DO I = 1,IDIMV
   OBSY(I,1,1:NumOfObs) = EPSIL(1:NumOfObs)
   PRINT *,"The observations at timestep 1: "
   PRINT *,OBSY(I,1,:)
END DO

!! 2.3 Calculate the Function of G(X*), Simplified Process

!!! 2.3.1 Calculate the GMat for the G(x*) expression
GMat(:,:) = QMat(:,:)*HMat(:,:)*(HMat(:,:)*QMat(:,:)*HMat(:,:)+RMat(:,:))**(-1)
PMat(:,:) = (QMat(:,:)**(-1)+HMat(:,:)*RMat(:,:)**(-1)*HMat(:,:))
PRINT *,"The G(x*) is:"
PRINT *, GMat(:,:)

!!! 2.3.2 Calculate the di for the G(x*) expression
dist(:,:) = 0.0
DO I = 1, NumOfObs
   DO J = 1, IDIMEn
   SVX(:,1,J) = SVX(:,0,J) + ETA(J)
   dist(:,J) = dist(:,J) + OBSY(:,1,I) - SVX(:,1,J) !Simplified Equation For H=1
   END DO
END DO
PRINT *,"dist is :"
PRINT *,dist(:,:)

!! 2.4 Calculate the function of {\phi}_i and the det|S_i| = exp({PHI})
PHI(:,:) = dist(:,:)*dist(:,:)*((HMat(:,:)*QMat(1,1)*HMat(:,:)+RMat(1,1))**(-1))
PRINT *,"PHI Function is :"
PRINT *,PHI(:,:)
SI(:,:) = exp(PHI(:,:))
PRINT *,"SI is :"
PRINT *,SI(:,:)


! Test Call for the Normal Distribution Generator
NDMean     = 0
NDVar      = 0.16
NDNumb(:)  = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn, NDNumb)
PRINT *,"This is just a test print for the Normal Distribution Random Number Generator"
PRINT *,NDNumb
! End of the Test Call

END PROGRAM
