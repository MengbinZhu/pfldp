PROGRAM SIMEXP

IMPLICIT NONE

INTEGER(KIND=4),PARAMETER   :: IDIMEn = 40    ! Number of Ensemble Size
INTEGER(KIND=4),PARAMETER   :: IDIMV  = 10    ! Size Number of the State Variables = 10
INTEGER(KIND=4),PARAMETER   :: ITIMESTEP  = 1 ! Number of Time Steps
INTEGER(KIND=4),PARAMETER   :: NumOfObs = 10  ! Number of Observations
INTEGER(KIND=4),ALLOCATABLE :: SEEDA(:)       ! Seed of the Random Number
REAL(KIND=8)                :: NDMean, NDVar  ! Test need Mean Value and Var Value
REAL(KIND=8),ALLOCATABLE    :: NDNumb(:)      ! Test need Output Random Number Vector
REAL(KIND=8),ALLOCATABLE    :: ETA(:)         ! The Model Error N(0,Q)
REAL(KIND=8),ALLOCATABLE    :: XI(:)          ! The OPD draws XI(IDIMEn)
REAL(KIND=8),ALLOCATABLE    :: EPSIL(:)       ! The Observation Error N(0,R) EPSIL(NumOfObs)

INTEGER(KIND=4)             :: I,J,K

!Step 0. System Perference About the PARAMETERs
REAL(KIND=8),ALLOCATABLE    :: BMat(:,:)      !Background Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: RMat(:,:)      !Observation Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: QMat(:,:)      !System Error Covariance Matrix
REAL(KIND=8),ALLOCATABLE    :: HMat(:,:)      !Observation Operator Matrix
REAL(KIND=8),ALLOCATABLE    :: HXMat(:,:)     !Observation Operator
REAL(KIND=8),ALLOCATABLE    :: GMat(:,:)      !Matrix For the Calculation of G(X*) 
REAL(KIND=8),ALLOCATABLE    :: MATMP(:,:)     !Matrix For the TMP 
REAL(KIND=8),ALLOCATABLE    :: MATMP2(:,:)    !Matrix For the TMP 2
REAL(KIND=8),ALLOCATABLE    :: PMat(:,:)      !Matrix For the Calculation of P
REAL(KIND=8),ALLOCATABLE    :: PMatHalf(:,:)  !Matrix For the Calculation of P
REAL(KIND=8),ALLOCATABLE    :: PMatInv(:,:)   !Matrix For the Calculation of P^{-1} Inverse of P
REAL(KIND=8),ALLOCATABLE    :: SVX(:,:,:)     !State Vector Variable X(Dim, TimeSeq, EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: SVX0(:,:)     !State Vector Variable X(Dim, TimeSeq, EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: SVXB(:,:,:)    !State Vector Variable X(Dim, TimeSeq, EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: OBSY(:,:)      !Observation Vector Variable Y(TimeSeq,NumOfObs)
REAL(KIND=8),ALLOCATABLE    :: Di(:,:)        !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: DiT(:,:)       !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: Di2(:,:)       !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: DiT2(:,:)      !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: PHI(:,:)       !The function of {\phi}_i
REAL(KIND=8),ALLOCATABLE    :: SI(:,:)        !The function of S_i
REAL(KIND=8),ALLOCATABLE    :: Weights(:)     !The weights of Every Particle Filter
REAL(KIND=8),ALLOCATABLE    :: TMPMat(:,:)    !Matrix For the TMP 
REAL(KIND=8),ALLOCATABLE    :: TMPMatInv(:,:) !Matrix For the TMP 2

REAL(KIND=8)                :: TMPNUM(1,1)
REAL(KIND=8)                :: USIGMA(IDIMEn)
REAL(KIND=8)                :: XVec(IDIMV,1)

!Step 1. Initialize the B, R, Q Matrices and SVX(IDIMV,0)
ALLOCATE(BMat(IDIMV,IDIMV))
ALLOCATE(RMat(NumOfObs,NumOfObs))
ALLOCATE(QMat(IDIMV,IDIMV))
ALLOCATE(HMat(NumOfObs,IDIMV))
ALLOCATE(HXMat(NumOfObs,1))
ALLOCATE(GMat(IDIMV,IDIMV))
ALLOCATE(MATMP(IDIMV,IDIMV))
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
ALLOCATE(PHI(1:IDIMV,1:IDIMEn))
ALLOCATE(SI(1:IDIMV,1:IDIMEn))
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
   !HMat(I,I) = 1.0
   !RMat(I,I) = 0.16
   QMat(I,I) = 0.01
END DO
DO I = 1, NumOfObs
   RMat(I,I) = 0.16
END DO

!! 1.2 Init the SVX(:,0)
SVX(1:IDIMV,0,1:IDIMEn) = 0.0   !Take it as the Truth
SVX0(1:IDIMV,0) = 0.0   !Take it as the Truth
SVXB(1:IDIMV,0,1:IDIMEn) = 0.0   !Take it as the Truth

! Step 2. Generate the Ensemble Member

ALLOCATE(SEEDA(IDIMEn*IDIMV)) !ALLOCATE the SEED of Random Number
ALLOCATE(NDNumb(IDIMEn*IDIMV))!ALLOCATE the Output
!================THIS IS THE PLACE WE CHANGE SEED================
SEEDA(:) = 12           !Initialize the Seed 

!! 2.1 Generate the initial Ensemble Xi(Dim,0,Ens)
NDMean    = 0
NDVar     = SQRT(BMat(1,1))
NDNumb(:) = 0.0 
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, NDNumb)
DO I = 1,IDIMV
   DO J = 1, IDIMEn
      SVX(I,0,J)  = SVX0(I,0) + NDNumb((J-1)*IDIMEn+I)  !Every dimension of X is the same.
      SVXB(I,0,J) = SVX0(I,0) + NDNumb((J-1)*IDIMEn+I)  !Every dimension of X is the same.
      !PRINT *,"The initial state vector X at timestep 0 :"
      !PRINT *,SVX(I,0,:)
   END DO
END DO
PRINT *,"The initial state vector X at timestep 0 :"
PRINT *,SVX(:,0,1)

!! 2.X Additional Step for the evolution of the Model, I think it is f(x_i^{n-1})
!! Model Evolution For Every Variable in State Vector
ALLOCATE(ETA(IDIMV*IDIMEn))
NDMean    = 0
NDVar     = SQRT(QMat(1,1))
ETA(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMV*IDIMEn, ETA)

DO I = 1, IDIMEn
   DO J = 1, IDIMV
      SVX(J,1,I)  = SVX(J,0,I) + ETA((J-1)*IDIMEn+I) ! Do the evolution of the Model
      SVX0(J,1)   = SVX0(J,0)  + ETA(J) ! Do the evolution of the Model
      SVXB(J,1,I) = SVX(J,0,I) + ETA((J-1)*IDIMEn+I) ! Do the evolution of the Model
   END DO
END DO
 
PRINT*,"SVX(:,1,1) = "
PRINT*,SVX(:,1,1)
PRINT*,"SVX0(:,1,1) = "
PRINT*,SVX0(:,1)
!! 2.2 Generate the Y(ITIMESTEP, NumOfObs)
ALLOCATE(EPSIL(NumOfObs))
NDMean   = 0
NDVar    = SQRT(RMat(1,1))
EPSIL(:) = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, NumOfObs, EPSIL)
XVec(:,1) = SVX0(:,1)
HXMat = MATMUL(HMat,XVec)
PRINT*,"XVec in Main Program ="
PRINT*,XVec(:,1)
PRINT*,"HXMat in Main Program = "
PRINT*,HXMat
OBSY(1,1:NumOfObs) = HXMat(1:NumOfObs,1) + EPSIL(1:NumOfObs)
PRINT *,"The observations at timestep 1: "
PRINT *,OBSY(1,:)

! 3. Do the different Algorithms of Particle Filters, just for 1 timestep

!==============================================================================
!! 3.1 the Sequential Importance Resampling Algorithm

CALL CALW(Weights,QMat,RMat,HMat,SVX,OBSY,IDIMV,1,1,IDIMEn,NumOfObs,1)

!==============================================================================
!! 3.2 Perform the Optimal Proposal Density Algorithm
!!! 3.2.1 Calculate the GMat and PMatInv for the G(x*) expression
!!! I need to make sure that the Multiplications of the Matrices are correct.

!CALL CALW(Weights,QMat,RMat,HMat,SVX,OBSY,IDIMV,1,1,IDIMEn,NumOfObs,2)

!GMat = Q*HT*(H*Q*HT+R)^(-1)
!GMat = MATMUL(HMat,MATMUL(QMat,HMat)) + RMat
!CALL MATRIXINV(GMat,MATMP,IDIMV)
!GMat = MATMUL(QMat,MATMUL(HMat,MATMP))

!P^{-1} = Q^{-1}+H*R^{-1}*H
!CALL MATRIXINV(QMat,MATMP,IDIMV)   ! Calculate the Inverse of Matrix Q
!CALL MATRIXINV(RMat,MATMP2,IDIMV)  ! Calculate the Inverse of Matrix R
!PMatInv = MATMUL(HMat,MATMUL(MATMP2,HMat)) + MATMP
!CALL MATRIXINV(PMatInv,PMat,IDIMV)
!DO I = 1, IDIMV
!   PMatHalf(I,I) = SQRT(PMat(I,I)) ! Calculate the P^{1/2}
!END DO
!PRINT *,"The GMat of G(x*) is:"
!PRINT *, GMat(:,:)
!PRINT *,"The PMat is:"
!PRINT *, PMat(:,:)

!!! 3.3.2 Calculate the di for the G(x*) expression
Di(:,:) = 0.0
DiT(:,:) = 0.0
DO I = 1, IDIMEn
   DO J = 1, NumOfObs
      DO K = 1, IDIMV
         !Di(K,I) = (OBSY(K,1,J) - SVX(K,1,I)) !Simplified Equation For H=1
         !DiT(I,K) = Di(K,I)
      END DO
   END DO
END DO
!PRINT *,"The X state Vector at timestep 1 is:"
!PRINT *,SVX(1,1,:)
!PRINT *,"dist is :"
!PRINT *,dist(:,:)

!!! 3.3.3 Draw from the Optimal Proposal Density
ALLOCATE(XI(IDIMEn*IDIMV))
NDMean   = 0
NDVar    = 1.0
XI(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, XI)
DO I = 1, IDIMV
   DO J = 1, IDIMEn
      SVXB(I,1,J) = XI(J*(IDIMEn-1)+I)
   END DO
END DO

!!!PROBLEM HERE IS HOW TO HANDLE THE Observations, if we have many Observations.
DO I = 1, IDIMEn
   !SVX(:,1,I) = MATMUL(PMat,(MATMUL(MATMP,SVX(:,1,I)) + MATMUL(MATMP2,OBSY(:,1,J))))
   !SVX(:,1,I) = MATMUL(PMatHalf,SVXB(:,1,I)) + SVX(:,1,I)
END DO

!==============================================================================
!! 3.3 Perform the New Scheme

!! 3.3.1 Calculate the function of {\phi}_i and the det|S_i| = exp({PHI})

!TMPMat = MATMUL(HMat,MATMUL(QMat,HMat)) + RMat
!CALL MATRIXINV(TMPMat,TMPMatInv,IDIMV)

DO I = 1,IDIMEn
   DO J = 1, NumOfObs
      DO K = 1, IDIMV
         !DiT2(1,K) = OBSY(K,1,J) - SVX(K,1,I)
         !Di2(K,1) = DiT2(1,K)
      END DO
      !TMPNUM = MATMUL(DiT2,MATMUL(TMPMatInv,Di2))
      !USIGMA(I) = USIGMA(I) + TMPNUM(1,1)
   END DO
   !SI(:,I) = EXP(USIGMA(I))
END DO
!PRINT *,"SI is :"
!PRINT*,SI(1,:)


!PHI(:,:) = dist(:,:)*dist(:,:)*((HMat(:,:)*QMat(1,1)*HMat(:,:)+RMat(1,1))**(-1))
!PRINT *,"PHI Function is :"
!PRINT *,PHI(:,:)
!SI(:,:) = exp(PHI(:,:))
!PRINT *,"SI is :"
!PRINT *,SI(:,:)


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
