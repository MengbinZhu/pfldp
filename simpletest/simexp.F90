SUBROUTINE SIMEXP(SEEDA,IDIMEn,IDIMV,ITIMESTEP,NumOfObs,EnsembleMean,TruthRun)

USE HSL_EA20_double

IMPLICIT NONE

INTEGER(KIND=4),INTENT(IN)  :: IDIMEn         ! Number of Ensemble Size
INTEGER(KIND=4),INTENT(IN)  :: IDIMV          ! Size Number of the State Variables = 10
INTEGER(KIND=4),INTENT(IN)  :: ITIMESTEP      ! Number of Time Steps
INTEGER(KIND=4),INTENT(IN)  :: NumOfObs       ! Number of Observations
INTEGER(KIND=4),INTENT(IN)  :: SEEDA(IDIMEn*IDIMV)       ! Seed of the Random Number
REAL(KIND=8),INTENT(INOUT)  :: EnsembleMean(ITIMESTEP,3)
REAL(KIND=8),INTENT(INOUT)  :: TruthRun(ITIMESTEP)

!Derived Types for HSL_EA20
TYPE(EA20_CONTROL)          :: CNTL
TYPE(EA20_INFO)             :: INFO
TYPE(EA20_REVERSE)          :: REV

!INTEGER(KIND=4),PARAMETER   :: IDIMEn = 10    ! Number of Ensemble Size
!INTEGER(KIND=4),PARAMETER   :: IDIMV  = 10    ! Size Number of the State Variables = 10
!INTEGER(KIND=4),PARAMETER   :: ITIMESTEP  = 1 ! Number of Time Steps
!INTEGER(KIND=4),PARAMETER   :: NumOfObs = 4   ! Number of Observations
!INTEGER(KIND=4),ALLOCATABLE :: SEEDA(:)       ! Seed of the Random Number
REAL(KIND=8)                :: NDMean, NDVar  ! Test need Mean Value and Var Value
REAL(KIND=8),ALLOCATABLE    :: NDNumb(:)      ! Test need Output Random Number Vector
REAL(KIND=8),ALLOCATABLE    :: ETA(:)         ! The Model Error N(0,Q)
REAL(KIND=8),ALLOCATABLE    :: XI(:)          ! The OPD draws XI(IDIMEn)
REAL(KIND=8),ALLOCATABLE    :: XINEW(:)       ! The OPD draws XI(IDIMEn)
REAL(KIND=8),ALLOCATABLE    :: EPSIL(:)       ! The Observation Error N(0,R) EPSIL(NumOfObs)
REAL(KIND=8),ALLOCATABLE    :: ETATRUTH(:)    ! The Model Error N(0,Q)

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
REAL(KIND=8),ALLOCATABLE    :: PW(:,:)      !Matrix For the Calculation of P
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
REAL(KIND=8)                :: UVec(IDIMV,IDIMEn)
REAL(KIND=8)                :: UVec1(IDIMV)
REAL(KIND=8)                :: WVec1(IDIMV,1)
REAL(KIND=8)                :: WVec2(IDIMV,1)

!Statistic Variables Defination
REAL(KIND=8)                :: EnsMean_SIR(ITIMESTEP)
REAL(KIND=8)                :: EnsMean_OPD(ITIMESTEP)
REAL(KIND=8)                :: EnsMean_New(ITIMESTEP)
REAL(KIND=8)                :: Sumtmp


double precision, parameter :: zero = 0.0d0, one = 1.0d0, two = 2.0d0
double precision            :: s
integer                     :: ido

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
!ALLOCATE(PW(IDIMV,IDIMV))
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

!! SET DATA
s = -0.5d0
!! SET CNTL FOR HSL_EA20
CNTL%d      = 3      !! delay
CNTL%tol    = 1.d-2  !! convergence tolerance
CNTL%maxit  = 10    !! max number iteration
CNTL%diagnostics_level = 1 !! full error check

!! 1.1 Init B, R, Q

TMPNUM    = 0.0
USIGMA(:) = 0.0

BMat(:,:) = 0.0
RMat(:,:) = 0.0
QMat(:,:) = 0.0
HMat(:,:) = 0.0

DO I = 1, IDIMV
   BMat(I,I) = 1.0
   QMat(I,I) = 0.01
END DO
DO I = 1, NumOfObs
   RMat(I,I) = 0.16
   HMat(I,I) = 1
END DO

!! 1.2 Init the SVX(:,0)
SVX(1:IDIMV,0,1:IDIMEn) = 0.0   !Traj 1
SVX0(1:IDIMV,0) = 0.0   !Take it as the Truth
SVXB(1:IDIMV,0,1:IDIMEn) = 0.0   !Backup of Traj 1, Traj 1'

! Step 2. Generate the Ensemble Member

!ALLOCATE(SEEDA(IDIMEn*IDIMV)) !ALLOCATE the SEED of Random Number
ALLOCATE(NDNumb(IDIMEn*IDIMV))!ALLOCATE the Output
!================THIS IS THE PLACE WE CHANGE SEED================
!SEEDA(:) = 10           !Initialize the Seed 

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

TruthRun(1) = SUM(SVX0(:,1))/IDIMV

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

!!! 3.1.1 the Weights Calculation

CALL CALW(Weights,QMat,RMat,HMat,SVX,OBSY,IDIMV,1,1,IDIMEn,NumOfObs,1)

!! Still need some statistic Variables here.
!! 3.1.2 the Ensemble Mean

CALL ENSMEAN(EnsMean_SIR(1),SVX(:,1,:),IDIMV,IDIMEn)
!PRINT*,"THE ENSEMBLE MEAN OF SIR IS = "
!PRINT*,EnsMean_SIR(1)

!==============================================================================
!! 3.2 Perform the Optimal Proposal Density Algorithm
!!! 3.2.1 Calculate the GMat and PMatInv for the G(x*) expression
!!! I need to make sure that the Multiplications of the Matrices are correct.

CALL CALW(Weights,QMat,RMat,HMat,SVX,OBSY,IDIMV,1,1,IDIMEn,NumOfObs,2)

!GMat2 = Q*HT*(H*Q*HT+R)^(-1)
HTMat = TRANSPOSE(HMat)
GMat1 = MATMUL(HMat,MATMUL(QMat,HTMat)) + RMat ! Dimension = NumOfObs
CALL MATRIXINV(GMat1,MATMP,NumOfObs)           ! MATMP = (H*Q*HT+R)^{-1}
!PRINT*,"ALGORITHM 1 TO CAL MATRIX INVERSE"
!PRINT*,MATMP
!CALL MATINV(GMat1,MATMP,NumOfObs)           ! MATMP = (H*Q*HT+R)^{-1}
!PRINT*,"ALGORITHM 2 TO CAL MATRIX INVERSE"
!PRINT*,MATMP
GMat2 = MATMUL(QMat,MATMUL(HTMat,MATMP))       ! Dimension = IDIMV
!PRINT*,GMat2

!P^{-1} = Q^{-1}+HT*R^{-1}*H
CALL MATRIXINV(QMat,QIMat,IDIMV)                      ! Calculate the Inverse of Matrix Q
!CALL MATINV(QMat,QIMat,IDIMV)                      ! Calculate the Inverse of Matrix Q
CALL MATRIXINV(RMat,RIMat,NumOfObs)                   ! Calculate the Inverse of Matrix R
!CALL MATINV(RMat,RIMat,NumOfObs)                   ! Calculate the Inverse of Matrix R
PMatInv = MATMUL(HTMat,MATMUL(RIMat,HMat)) + QIMat
!PRINT*,"The inverse of P Matrix is = "
!PRINT*,PMatInv
CALL MATRIXINV(PMatInv,PMat,IDIMV)
!CALL MATINV(PMatInv,PMat,IDIMV)
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

GMat = MATMUL(GMat2,Di) !Calculation of G(x*)

!!! 3.2.3 Draw from the Optimal Proposal Density
!!!! 3.2.3.1 First calculate the value of P^{1/2}{\xi}
!PW(:,:) = PMat(:,:)
ALLOCATE(XI(IDIMEn*IDIMV))
NDMean   = 0
NDVar    = 1.0
XI(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, XI)
!!!=================The calculation of P^{1/2}*{\xi}_i^n=====================
!!!   HSL(2013). A collection of Fortran codes for large scale scientific !!!
!!!   computation. http://www.hsl.rl.ac.uk                                !!!
!!!==========================================================================
DO I = 1, IDIMEn
  DO J = 1, IDIMV
      UVec(J,I) = XI(J+(I-1)*IDIMV)
  END DO
  ido = -1
  !PW(:,:) = PMat(:,:)
  UVec1(:) = UVec(:,I)
  do while (ido .ne. 0 .and. info%flag == 0)
     call EA20(IDIMV,UVec1,s,ido,PW,CNTL,INFO,REV)
     select case(ido)
     case(0)
         exit
     case(1) !! Matrix-Vector product w_out = A w(:,1)
         WVec1(:,1) = PW(:,1)
         WVec2(:,1) = PW(:,2)
         WVec2 = MATMUL(PMatInv,WVec1)
         PW(:,2) = WVec2(:,1)
     case(2) !! Matrix-Vector product w(:,2) = M w(:,1)
         PW(:,2) = PW(:,1)
     case(3) !! Matrix-Vector product w_out = inv(M) w(:,1)
         PW(:,2) = PW(:,1)
     end select
  end do
  UVec(:,I) = UVec1(:)
  if(info%flag .ge. 0) then
      !write(*,'(a,i5)') 'error code =',info%flag
      !write(*,'(a,i5)') 'number of interations = ',info%iter
      !write(*,'(a,1x,1pe14.2)') 'estimated final error = ',info%error
      !write(*,'(a)') '  k    X(k)'
      !write(*,'(i5,1x,1pe14.7)') (k, UVec1(k), k=1,IDIMV)
  end if
END DO
!!!==============End of The calculation of P^{1/2}*{\xi}_i^n=================

!!!! 3.2.3.2 Finish the final step!
!ALLOCATE(XI(IDIMEn*IDIMV))
!NDMean   = 0
!NDVar    = 1.0
!XI(:)    = 0.0
!CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, XI)
DO I = 1, IDIMEn
   !DO J = 1, IDIMV
      !SVX(J,1,I) = XI((I-1)*IDIMV+J)
   !   XVec(J,1) = XI(J+(I-1)*IDIMV)
   !END DO
   SVX(:,1,I) = GMat(:,I) + SVX(:,0,I) + UVec(:,I) ! This equation is completed now.
END DO

!!! 3.2.4 Do the Statistic Step: Ensemble Mean

CALL ENSMEAN(EnsMean_OPD(1),SVX(:,1,:),IDIMV,IDIMEn)
!PRINT*,"THE ENSEMBLE MEAN OF OPD IS = "
!PRINT*,EnsMean_OPD(1)


!==============================================================================
!! 3.3 Perform the New Scheme

!! 3.3.1 Calculate the function of {\phi}_i and the det|S_i| = exp({PHI})

PHI =  MATMUL(DiT,MATMUL(MATMP,Di)) !Only the diagnal elements are the values of {\phi}_i
CALL BSDET(PMat,IDIMV,PDet)

!PRINT*,"P Matrix = "
!PRINT*,PMat
!PRINT*,"Det(P) = "
!PRINT*,PDet

!PRINT*,"PHI_ii = "
Sumtmp = 0.0
DO I= 1, IDIMEn
   Sumtmp = Sumtmp + PHI(I,I)
!PRINT*,PHI(I,I)
END DO

PRINT*,"ALPHA = ,There always are some values that we cannot choose!"
DO I=1,IDIMEn
   Alpha(I) = EXP((PHI(I,I)-(Sumtmp/IDIMEn))/IDIMV)
   PRINT*,Alpha(I)
END DO

!!! 3.3.2 Draw from the New Scheme
ALLOCATE(XINEW(IDIMEn*IDIMV))
NDMean   = 0
NDVar    = 1.0
XINEW(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, XINEW)

!!!=================The calculation of P^{1/2}*{\xi}_i^n=====================
!!!   HSL(2013). A collection of Fortran codes for large scale scientific !!!
!!!   computation. http://www.hsl.rl.ac.uk                                !!!
!!!==========================================================================
DO I = 1, IDIMEn
  DO J = 1, IDIMV
      UVec(J,I) = XINEW(J+(I-1)*IDIMV)
  END DO
  ido = -1
  !PW(:,:) = PMat(:,:)
  UVec1(:) = UVec(:,I)
  do while (ido .ne. 0 .and. info%flag == 0)
     call EA20(IDIMV,UVec1,s,ido,PW,CNTL,INFO,REV)
     select case(ido)
     case(0)
         exit
     case(1) !! Matrix-Vector product w_out = A w(:,1)
         WVec1(:,1) = PW(:,1)
         WVec2(:,1) = PW(:,2)
         WVec2 = MATMUL(PMatInv,WVec1)
         PW(:,2) = WVec2(:,1)
     case(2) !! Matrix-Vector product w(:,2) = M w(:,1)
         PW(:,2) = PW(:,1)
     case(3) !! Matrix-Vector product w_out = inv(M) w(:,1)
         PW(:,2) = PW(:,1)
     end select
  end do
  UVec(:,I) = UVec1(:)
  if(info%flag .ge. 0) then
      !write(*,'(a,i5)') 'error code =',info%flag
      !write(*,'(a,i5)') 'number of interations = ',info%iter
      !write(*,'(a,1x,1pe14.2)') 'estimated final error = ',info%error
      !write(*,'(a)') '  k    X(k)'
      !write(*,'(i5,1x,1pe14.7)') (k, UVec1(k), k=1,IDIMV)
  end if
END DO
!!!==============End of The calculation of P^{1/2}*{\xi}_i^n=================

DO I = 1, IDIMEn
   DO J = 1, IDIMV
      XVec(J,1) = XINEW(J+(I-1)*IDIMV)
   END DO
   !SVXB(:,1,I) = GMat(:,I) + SVXB(:,0,I) + UVec(:,I)*ALPHA(I) ! This equation is completed now.
   IF(ABS(ALPHA(I)) > 2.0) THEN
     SVXB(:,1,I) = GMat(:,I) + SVXB(:,0,I) + UVec(:,I)           ! This equation is completed now.
   ELSE
     SVXB(:,1,I) = GMat(:,I) + SVXB(:,0,I) + UVec(:,I)*SQRT(ABS(ALPHA(I))) ! This equation is completed now.
   END IF
   XVecT = TRANSPOSE(XVec)
   TMPNUM = MATMUL(XVecT,XVec)
   OWeights(I) = TMPNUM(1,1)
END DO

!!! 3.3.3 Calculate the Weights
Weights(:) = (ABS(ALPHA(:)-1))*OWeights(:)
Weights(:) = Weights(:)/SUM(Weights(:))
PRINT*,"=======================The Weights Of New Scheme============================"
PRINT*,Weights
PRINT*,"=======================END OF THE WEIGHTS OF NEW SCHEME====================="

!!! 3.3.4 Do the Ensemble Mean Calculation
CALL ENSMEAN(EnsMean_New(1),SVXB(:,1,:),IDIMV,IDIMEn)
!PRINT*,"THE ENSEMBLE MEAN OF New Scheme IS = "
!PRINT*,EnsMean_New(:,1)

EnsembleMean(1,1) = EnsMean_SIR(1)
EnsembleMean(1,2) = EnsMean_OPD(1)
EnsembleMean(1,3) = EnsMean_New(1)

! Test Call for the Normal Distribution Generator
!NDMean     = 0
!NDVar      = 0.16
!NDNumb(:)  = 0.0
!CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn, NDNumb)
!PRINT *,"This is just a test print for the Normal Distribution Random Number Generator"
!PRINT *,NDNumb
! End of the Test Call

!DEALLOCATE the Variables to free the memory

IF(ALLOCATED(BMat)) DEALLOCATE(BMat)
IF(ALLOCATED(RMat)) DEALLOCATE(RMat)
IF(ALLOCATED(QMat)) DEALLOCATE(QMat)
IF(ALLOCATED(HMat)) DEALLOCATE(HMat)
IF(ALLOCATED(HTMat)) DEALLOCATE(HTMat)
IF(ALLOCATED(HXMat)) DEALLOCATE(HXMat)
IF(ALLOCATED(GMat)) DEALLOCATE(GMat)
IF(ALLOCATED(GMat1)) DEALLOCATE(GMat1)
IF(ALLOCATED(GMat2)) DEALLOCATE(GMat2)
IF(ALLOCATED(MATMP)) DEALLOCATE(MATMP)
IF(ALLOCATED(MATMP2)) DEALLOCATE(MATMP2)
IF(ALLOCATED(PMat)) DEALLOCATE(PMat)
IF(ALLOCATED(PMatInv)) DEALLOCATE(PMatInv)
IF(ALLOCATED(PMatHalf)) DEALLOCATE(PMatHalf)
IF(ALLOCATED(TMPMat)) DEALLOCATE(TMPMat)
IF(ALLOCATED(TMPMatInv)) DEALLOCATE(TMPMatInv)
IF(ALLOCATED(SVX)) DEALLOCATE(SVX)
IF(ALLOCATED(RIMat)) DEALLOCATE(RIMat)
IF(ALLOCATED(QIMat)) DEALLOCATE(QIMat)
IF(ALLOCATED(SVX0)) DEALLOCATE(SVX0)
IF(ALLOCATED(SVXB)) DEALLOCATE(SVXB)
IF(ALLOCATED(OBSY)) DEALLOCATE(OBSY)
IF(ALLOCATED(Di)) DEALLOCATE(Di)
IF(ALLOCATED(DiT)) DEALLOCATE(DiT)
IF(ALLOCATED(Di2)) DEALLOCATE(Di2)
IF(ALLOCATED(DiT2)) DEALLOCATE(DiT2)
IF(ALLOCATED(PHI)) DEALLOCATE(PHI)
IF(ALLOCATED(DETSI)) DEALLOCATE(DETSI)
IF(ALLOCATED(ALPHA)) DEALLOCATE(ALPHA)
IF(ALLOCATED(OWeights)) DEALLOCATE(OWeights)
IF(ALLOCATED(Weights)) DEALLOCATE(Weights)

END SUBROUTINE
