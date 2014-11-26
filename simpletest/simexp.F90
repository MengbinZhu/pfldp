PROGRAM SIMEXP

IMPLICIT NONE

INTEGER(KIND=4),PARAMETER   :: IDIMEn = 10    ! Number of Ensemble Size
INTEGER(KIND=4),PARAMETER   :: IDIMV  = 1     ! Size Number of the State Variables
INTEGER(KIND=4),PARAMETER   :: ITIMESTEP  = 1 ! Size Number of the State Variables
INTEGER(KIND=4),ALLOCATABLE :: SEEDA(:)       ! Seed of the Random Number
REAL(KIND=8)                :: NDMean, NDVar  ! Test need Mean Value and Var Value
REAL(KIND=8),ALLOCATABLE    :: NDNumb(:)      ! Test need Output Random Number Vector
REAL(KIND=8),ALLOCATABLE    :: ETA(:)         ! The Model Error N(0,Q)
REAL(KIND=8),ALLOCATABLE    :: EPSIL(:)       ! The Observation Error N(0,R)

INTEGER(KIND=4)             :: I,J,K

!Step 0. System Perference About the PARAMETERs
REAL(KIND=8),ALLOCATABLE    :: BMat(:,:)      !Background Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: RMat(:,:)      !Observation Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: QMat(:,:)      !System Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: SVX(:,:,:)     !State Vector Variable X(Dim, TimeSeq, EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: OBSY(:,:)      !Observation Vector Variable Y(Dim, TimeSeq)

!Step 1. Initialize the B, R, Q Matrices and SVX(IDIMV,0)
ALLOCATE(BMat(IDIMV,IDIMV))
ALLOCATE(RMat(IDIMV,IDIMV))
ALLOCATE(QMat(IDIMV,IDIMV))
ALLOCATE(SVX(1:IDIMV,0:ITIMESTEP,1:IDIMEn))
ALLOCATE(OBSY(1:IDIMV,1:ITIMESTEP))

!! 1.1 Init B, R, Q
BMat(:,:) = 0.0
RMat(:,:) = 0.0
QMat(:,:) = 0.0

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
   SVX(I,0,1:IDIMEn) = NDNumb(1:IDIMEn)
   PRINT *,SVX(I,0,:)
END DO

!! 2.2 Generate the Y(Dim, ITIMESTEP)
NDMean   = 0
NDVar    = QMat(1,1)


! Test Call for the Normal Distribution Generator
NDMean     = 0
NDVar      = 0.16
NDNumb(:)  = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn, NDNumb)
! End of the Test Call

PRINT*,NDNumb

END PROGRAM
