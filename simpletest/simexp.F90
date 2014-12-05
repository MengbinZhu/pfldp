PROGRAM SIMEXP

IMPLICIT NONE

INTEGER(KIND=4),PARAMETER   :: IDIMEn = 20    ! Number of Ensemble Size
INTEGER(KIND=4),PARAMETER   :: IDIMV  = 10   ! Size Number of the State Variables = 10
INTEGER(KIND=4),PARAMETER   :: ITIMESTEP  = 1 ! Number of Time Steps
INTEGER(KIND=4),PARAMETER   :: NumOfObs = 2  ! Number of Observations
INTEGER(KIND=4),ALLOCATABLE :: SEEDA(:)       ! Seed of the Random Number
REAL(KIND=8)                :: NDMean, NDVar  ! Test need Mean Value and Var Value
REAL(KIND=8),ALLOCATABLE    :: NDNumb(:)      ! Test need Output Random Number Vector
REAL(KIND=8),ALLOCATABLE    :: ETA(:)         ! The Model Error N(0,Q)
REAL(KIND=8),ALLOCATABLE    :: XI(:)          ! The OPD draws XI(IDIMEn)
REAL(KIND=8),ALLOCATABLE    :: XINEW(:)          ! The OPD draws XI(IDIMEn)
REAL(KIND=8),ALLOCATABLE    :: EPSIL(:)       ! The Observation Error N(0,R) EPSIL(NumOfObs)
REAL(KIND=8),ALLOCATABLE    :: ETATRUTH(:)         ! The Model Error N(0,Q)

INTEGER(KIND=4)             :: I,J,K

!Step 0. System Perference About the PARAMETERs
REAL(KIND=8),ALLOCATABLE    :: BMat(:,:)      !Background Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: RMat(:,:)      !Observation Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: RIMat(:,:)     !Inverse Of Observation Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: QMat(:,:)      !System Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: QIMat(:,:)     !Inverse of System Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: HMat(:,:)      !Observation Operator Matrix
REAL(KIND=8),ALLOCATABLE    :: HTMat(:,:)     !Transpose of Observation Operator Matrix
REAL(KIND=8),ALLOCATABLE    :: HXMat(:,:)     !Observation Operator
REAL(KIND=8),ALLOCATABLE    :: GMat(:,:)      !Matrix of G(X*) 
REAL(KIND=8),ALLOCATABLE    :: GMat1(:,:)     !Matrix For the Calculation of G(X*) 
REAL(KIND=8),ALLOCATABLE    :: GMat2(:,:)     !Matrix For the Calculation of G(X*) 
REAL(KIND=8),ALLOCATABLE    :: MATMP(:,:)     !Matrix For the TMP 
REAL(KIND=8),ALLOCATABLE    :: MATMP2(:,:)    !Matrix For the TMP 2
REAL(KIND=8),ALLOCATABLE    :: PMat(:,:)      !Matrix For the Calculation of P
REAL(KIND=8),ALLOCATABLE    :: PMatHalf(:,:)  !P^{1/2}
REAL(KIND=8),ALLOCATABLE    :: PMatInv(:,:)   !P^{-1} Inverse of P
REAL(KIND=8),ALLOCATABLE    :: SVX(:,:,:)     !State Vector Variable X(Dim, TimeSeq, EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: SVX0(:,:)      !Truth X(Dim, TimeSeq)
REAL(KIND=8),ALLOCATABLE    :: SVXB(:,:,:)    !Backup State Vector Variable X(Dim, TimeSeq, EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: OBSY(:,:)      !Observation Vector Variable Y(TimeSeq,NumOfObs)
REAL(KIND=8),ALLOCATABLE    :: Di(:,:)        !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: DiT(:,:)       !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: Di2(:,:)       !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: DiT2(:,:)      !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: PHI(:,:)       !The function of {\phi}_i
REAL(KIND=8),ALLOCATABLE    :: DETSI(:)       !Det(Si) S_i
REAL(KIND=8),ALLOCATABLE    :: Weights(:)     !The weights of Every Particle Filter
REAL(KIND=8),ALLOCATABLE    :: TMPMat(:,:)    !Matrix For the TMP 
REAL(KIND=8),ALLOCATABLE    :: TMPMatInv(:,:) !Matrix For the TMP 2
REAL(KIND=8)                :: PDet           !Det(P)
REAL(KIND=8),ALLOCATABLE    :: Alpha(:)       !Alpha(I)
REAL(KIND=8),ALLOCATABLE    :: OWeights(:)       !Alpha(I)

REAL(KIND=8)                :: TMPNUM(1,1)
REAL(KIND=8)                :: USIGMA(IDIMEn)
REAL(KIND=8)                :: XVec(IDIMV,1)
REAL(KIND=8)                :: XVecT(1,IDIMV)

!Step 1. Initialize the B, R, Q Matrices and SVX(IDIMV,0)
ALLOCATE(BMat(IDIMV,IDIMV))
ALLOCATE(RMat(NumOfObs,NumOfObs))
ALLOCATE(RIMat(NumOfObs,NumOfObs))
ALLOCATE(QMat(IDIMV,IDIMV))
ALLOCATE(QIMat(IDIMV,IDIMV))
ALLOCATE(HMat(NumOfObs,IDIMV))
ALLOCATE(HTMat(IDIMV,NumOfObs))
ALLOCATE(HXMat(NumOfObs,1))
ALLOCATE(GMat(IDIMV,IDIMEn))
ALLOCATE(GMat1(NumOfObs,NumOfObs))
ALLOCATE(GMat2(IDIMV,NumOfObs))
ALLOCATE(MATMP(NumOfObs,NumOfObs))
ALLOCATE(MATMP2(IDIMV,IDIMV))
ALLOCATE(PMat(IDIMV,IDIMV))
ALLOCATE(PMatHalf(IDIMV,IDIMV))
ALLOCATE(PMatInv(IDIMV,IDIMV))
ALLOCATE(TMPMat(IDIMV,IDIMV))
ALLOCATE(TMPMatInv(IDIMV,IDIMV))

ALLOCATE(SVX(1:IDIMV,0:ITIMESTEP,1:IDIMEn))
ALLOCATE(SVX0(1:IDIMV,0:ITIMESTEP))
ALLOCATE(SVXB(1:IDIMV,0:ITIMESTEP,1:IDIMEn))
ALLOCATE(OBSY(1:ITIMESTEP,1:NumOfObs))
ALLOCATE(Di(1:IDIMV,1:IDIMEn))
ALLOCATE(DiT(1:IDIMEn,1:IDIMV))
ALLOCATE(Di2(1:IDIMV,1))
ALLOCATE(DiT2(1,1:IDIMV))
ALLOCATE(PHI(1:IDIMEn,1:IDIMEn))
ALLOCATE(DETSI(1:IDIMEn))
ALLOCATE(ALPHA(1:IDIMEn))
ALLOCATE(OWeights(1:IDIMEn))
ALLOCATE(Weights(IDIMEn))

!! 1.1 Init B, R, Q

TMPNUM    = 0.0
USIGMA(:) = 0.0

BMat(:,:) = 0.0
RMat(:,:) = 0.0
QMat(:,:) = 0.0
HMat(:,:) = 1.0

DO I = 1, IDIMV
   BMat(I,I) = 1.0
   QMat(I,I) = 0.01
END DO
DO I = 1, NumOfObs
   RMat(I,I) = 0.16
END DO

!! 1.2 Init the SVX(:,0)
SVX(1:IDIMV,0,1:IDIMEn) = 0.0   !Traj 1
SVX0(1:IDIMV,0) = 0.0   !Take it as the Truth
SVXB(1:IDIMV,0,1:IDIMEn) = 0.0   !Backup of Traj 1, Traj 1'

! Step 2. Generate the Ensemble Member

ALLOCATE(SEEDA(IDIMEn*IDIMV)) !ALLOCATE the SEED of Random Number
ALLOCATE(NDNumb(IDIMEn*IDIMV))!ALLOCATE the Output
!================THIS IS THE PLACE WE CHANGE SEED================
SEEDA(:) = 12           !Initialize the Seed 

CALL RANDOM_SEED(PUT=SEEDA(:))

!! 2.1 Generate the initial Ensemble Xi(Dim,0,Ens)
NDMean    = 0
NDVar     = SQRT(BMat(1,1))
NDNumb(:) = 0.0 
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, NDNumb)

DO J = 1, IDIMEn
   DO I = 1,IDIMV
      SVX(I,0,J)  = SVX0(I,0) + NDNumb((J-1)*IDIMV+I)  !Every dimension of X is the same.
      SVXB(I,0,J) = SVX0(I,0) + NDNumb((J-1)*IDIMV+I)  !Every dimension of X is the same.
   END DO
END DO

!! 2.X Additional Step for the evolution of the Model, I think it is f(x_i^{n-1}) + force
!! I need to change the equation here to the call of the model equations
!! Model Evolution For Every Variable in State Vector
ALLOCATE(ETA(IDIMV*IDIMEn))
NDMean    = 0
NDVar     = SQRT(QMat(1,1))
ETA(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMV*IDIMEn, ETA)

DO I = 1, IDIMEn
   DO J = 1, IDIMV
      SVX(J,1,I)  = SVX(J,0,I) + ETA((I-1)*IDIMV+J) ! Do the evolution of the Model for Every Ensemble Member
      SVXB(J,1,I) = SVXB(J,0,I) + ETA((I-1)*IDIMV+J) ! Do the evolution of the Model for Every Ensemble Member
   END DO
END DO
 
ALLOCATE(ETATRUTH(IDIMV))
NDMean    = 0
NDVar     = SQRT(QMat(1,1))
ETA(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMV, ETATRUTH)
DO I = 1, IDIMV
   SVX0(I,1) = SVX0(I,0) + ETATRUTH(I) ! Do the Model run for the Truth, There is no uncertainty here.
END DO

!! 2.2 Generate the Y(ITIMESTEP, NumOfObs)
ALLOCATE(EPSIL(NumOfObs))
NDMean   = 0
NDVar    = SQRT(RMat(1,1))
EPSIL(:) = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, NumOfObs, EPSIL)
XVec(:,1) = SVX0(:,1)
HXMat = MATMUL(HMat,XVec)
OBSY(1,1:NumOfObs) = HXMat(1:NumOfObs,1) + EPSIL(1:NumOfObs)

! 3. Do the different Algorithms of Particle Filters, just for 1 timestep

!==============================================================================
!! 3.1 the Sequential Importance Resampling Algorithm

CALL CALW(Weights,QMat,RMat,HMat,SVX,OBSY,IDIMV,1,1,IDIMEn,NumOfObs,1)

!! Still need some statistic Variables here.

!==============================================================================
!! 3.2 Perform the Optimal Proposal Density Algorithm
!!! 3.2.1 Calculate the GMat and PMatInv for the G(x*) expression
!!! I need to make sure that the Multiplications of the Matrices are correct.

CALL CALW(Weights,QMat,RMat,HMat,SVX,OBSY,IDIMV,1,1,IDIMEn,NumOfObs,2)

!GMat = Q*HT*(H*Q*HT+R)^(-1)
HTMat = TRANSPOSE(HMat)
GMat1 = MATMUL(HMat,MATMUL(QMat,HTMat)) + RMat ! Dimension = NumOfObs
CALL MATRIXINV(GMat1,MATMP,NumOfObs)
GMat2 = MATMUL(QMat,MATMUL(HTMat,MATMP))       ! Dimension = IDIMV
!PRINT*,GMat2

!P^{-1} = Q^{-1}+HT*R^{-1}*H
CALL MATRIXINV(QMat,QIMat,IDIMV)                   ! Calculate the Inverse of Matrix Q
CALL MATRIXINV(RMat,RIMat,NumOfObs)                   ! Calculate the Inverse of Matrix R
PMatInv = MATMUL(HTMat,MATMUL(RIMat,HMat)) + QIMat
CALL MATRIXINV(PMatInv,PMat,IDIMV)
!PRINT*,PMat

!!! 3.2.2 Calculate the di for the G(x*) expression
Di2(:,:) = 0.0
DiT2(:,:) = 0.0
DO I = 1, IDIMEn
   XVec(:,1) = SVX(:,0,I)
   HXMat = MATMUL(HMat,XVec)
   DO J = 1, NumOfObs
      DiT2(1,J) = OBSY(1,J) - HXMat(J,1) !Simplified Equation For H=1
      Di2(J,1) = DiT2(1,J)
   END DO
   Di(:,I) = Di2(:,1)
   DiT(I,:) = DiT2(1,:)
END DO

!GMat = MATMUL(GMat2,Di) !Calculation of G(x*)

!!! 3.2.3 Draw from the Optimal Proposal Density
ALLOCATE(XI(IDIMEn*IDIMV))
NDMean   = 0
NDVar    = 1.0
XI(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, XI)
DO I = 1, IDIMEn
   DO J = 1, IDIMV
      !SVX(J,1,I) = XI((I-1)*IDIMV+J)
      XVec(J,1) = XI(J+(I-1)*IDIMV)
   END DO
   SVX(:,1,I) = GMat(:,I) + SVX(:,0,I) + XVec(:,1) ! This equation is not completed. I need to compute P^{1/2}
END DO

!==============================================================================
!! 3.3 Perform the New Scheme

!! 3.3.1 Calculate the function of {\phi}_i and the det|S_i| = exp({PHI})

PHI =  MATMUL(DiT,MATMUL(MATMP,Di)) !Only the diagnal elements are the values of {\phi}_i
CALL BSDET(PMat,IDIMV,PDet)
PRINT*,"P Matrix = "
PRINT*,PMat
PRINT*,"Det(P) = "
PRINT*,PDet
DO I=1,IDIMEn
   Alpha(I) = EXP(PHI(I,I)/IDIMV)
   PRINT*,Alpha(I)
END DO

!!! 3.3.2 Draw from the New Scheme
ALLOCATE(XINEW(IDIMEn*IDIMV))
NDMean   = 0
NDVar    = 1.0
XINEW(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, XINEW)
DO I = 1, IDIMEn
   DO J = 1, IDIMV
      XVec(J,1) = XINEW(J+(I-1)*IDIMV)
   END DO
   SVXB(:,1,I) = GMat(:,I) + SVXB(:,0,I) + XVec(:,1) ! This equation is not completed. I need to compute P^{1/2}
   XVecT = TRANSPOSE(XVec)
   TMPNUM = MATMUL(XVecT,XVec)
   OWeights(I) = TMPNUM(1,1)
END DO

Weights(:) = (ALPHA(:)-1)*OWeights(:)
Weights(:) = Weights(:)/SUM(Weights(:))
PRINT*,"=======================The Weights Of New Scheme============================"
PRINT*,Weights
PRINT*,"=======================END OF THE WEIGHTS OF NEW SCHEME====================="

! Test Call for the Normal Distribution Generator
!NDMean     = 0
!NDVar      = 0.16
!NDNumb(:)  = 0.0
!CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn, NDNumb)
!PRINT *,"This is just a test print for the Normal Distribution Random Number Generator"
!PRINT *,NDNumb
! End of the Test Call

!DEALLOCATE the Variables to free the memory
!DO I = 1, IDIMV
!DEALLOCATE(BMat(I,:))
!END DO
!DEALLOCATE(RMat(:,:))
!DEALLOCATE(QMat(:,:))
!DEALLOCATE(HMat(:,:))
!DEALLOCATE(GMat(:,:))
!DEALLOCATE(MATMP(:,:))
!DEALLOCATE(MATMP2(:,:))
!DEALLOCATE(PMat(:,:))

!DEALLOCATE(SVX(:,:,:))
!DEALLOCATE(OBSY(:,:,:))
!DEALLOCATE(dist(:,:))
!DEALLOCATE(PHI(:,:))
!DEALLOCATE(SI(:,:))
!DEALLOCATE(Weights(:))

END PROGRAM
