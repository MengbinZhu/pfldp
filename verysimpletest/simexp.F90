SUBROUTINE SIMEXP(SEEDA,IDIMEn,IDIMV,ITIMESTEP,NumOfObs,EnsembleMean,TruthRun)

USE HSL_EA20_double
USE qsort_c_module

IMPLICIT NONE

INTEGER(KIND=4),INTENT(IN)  :: IDIMEn         ! Number of Ensemble Size
INTEGER(KIND=4),INTENT(IN)  :: IDIMV          ! Size Number of the State Variables = 10
INTEGER(KIND=4),INTENT(IN)  :: ITIMESTEP      ! Number of Time Steps
INTEGER(KIND=4),INTENT(IN)  :: NumOfObs       ! Number of Observations
INTEGER(KIND=4),INTENT(IN)  :: SEEDA(IDIMEn*IDIMV)       ! Seed of the Random Number
REAL(KIND=8),INTENT(INOUT)  :: EnsembleMean(ITIMESTEP,4)
REAL(KIND=8),INTENT(INOUT)  :: TruthRun(ITIMESTEP)

!Derived Types for HSL_EA20
TYPE(EA20_CONTROL)          :: CNTL
TYPE(EA20_INFO)             :: INFO
TYPE(EA20_REVERSE)          :: REV

REAL(KIND=8)                :: NDMean, NDVar  ! Test need Mean Value and Var Value
REAL(KIND=8),ALLOCATABLE    :: NDNumb(:)      ! Test need Output Random Number Vector
REAL(KIND=8),ALLOCATABLE    :: ETA(:)         ! The Model Error N(0,Q)
REAL(KIND=8),ALLOCATABLE    :: XI(:)          ! The OPD draws XI(IDIMEn)
REAL(KIND=8),ALLOCATABLE    :: XINEW(:)       ! The OPD draws XI(IDIMEn)
REAL(KIND=8),ALLOCATABLE    :: EPSIL(:)       ! The Observation Error N(0,R) EPSIL(NumOfObs)
REAL(KIND=8),ALLOCATABLE    :: ETATRUTH(:)    ! The Model Error N(0,Q)

INTEGER(KIND=4)             :: I,J,K

!Step 0. System Perference About the PARAMETERs
REAL(KIND=8)    :: BMat      !Background Error Covariance Matrix
REAL(KIND=8)    :: RMat      !Observation Error Covariance Matrix
REAL(KIND=8)    :: RIMat     !Inverse Of Observation Error Covariance Matrix
REAL(KIND=8)    :: QMat      !System Error Covariance Matrix
REAL(KIND=8)    :: QIMat     !Inverse of System Error Covariance Matrix
REAL(KIND=8)    :: HMat      !Observation Operator Matrix
REAL(KIND=8)    :: HTMat     !Transpose of Observation Operator Matrix
REAL(KIND=8)    :: HXMat    !Observation Operator
REAL(KIND=8)    :: GMat      !Matrix of G(X*) 
REAL(KIND=8)    :: GMat1     !Matrix For the Calculation of G(X*) 
REAL(KIND=8)    :: GMat2     !Matrix For the Calculation of G(X*) 
REAL(KIND=8)    :: MATMP     !Matrix For the TMP 
REAL(KIND=8)    :: MATMP2    !Matrix For the TMP 2
REAL(KIND=8)    :: PMat      !Matrix For the Calculation of P
REAL(KIND=8)    :: PW        !Matrix For the Calculation of P
REAL(KIND=8)    :: PMatHalf  !P^{1/2}
REAL(KIND=8)    :: PMatInv   !P^{-1} Inverse of P
REAL(KIND=8),ALLOCATABLE    :: SVX(:,:,:)     !State Vector Variable X(Dim, TimeSeq, EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: SVX0(:,:)      !Truth X(Dim, TimeSeq)
REAL(KIND=8),ALLOCATABLE    :: SVXB(:,:,:)    !Backup State Vector Variable X(Dim, TimeSeq, EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: SVXEW(:,:,:)   !Backup State Vector Variable X(Dim, TimeSeq, EnsembleNum) for EWPF
REAL(KIND=8),ALLOCATABLE    :: OBSY(:,:)      !Observation Vector Variable Y(TimeSeq,NumOfObs)
REAL(KIND=8),ALLOCATABLE    :: Di(:,:)        !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: DiT(:,:)       !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: Di2(:,:)       !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: DiT2(:,:)      !Distance between y^n and Hf(x_i^{n-1}) dist(Dim,EnsembleNum)
REAL(KIND=8),ALLOCATABLE    :: PHI(:,:)       !The function of {\phi}_i
REAL(KIND=8),ALLOCATABLE    :: Weights(:)     !The weights of Every Particle Filter
REAL(KIND=8)    :: TMPMat    !Matrix For the TMP 
REAL(KIND=8)    :: TMPMatInv !Matrix For the TMP 2
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

!Variables for EWPF
REAL(KIND=8)                :: a_i(IDIMEn)
REAL(KIND=8)                :: b_i(IDIMEn)
REAL(KIND=8)                :: alpha_i(IDIMEn)
REAL(KIND=8)                :: c_weights(IDIMEn)
REAL(KIND=8)                :: c_sort(IDIMEn)
REAL(KIND=8)                :: w_rest(IDIMEn)
REAL(KIND=8)                :: mat(IDIMEn,IDIMEn)
REAL(KIND=8)                :: mat2(IDIMEn,IDIMEn)
REAL(KIND=8)                :: xtest(IDIMV,IDIMEn)
REAL(KIND=8)                :: xtest_t(IDIMEn,IDIMV)
REAL(KIND=8)                :: cc
REAL(KIND=8),PARAMETER      :: CCC = 0.9
REAL(KIND=8),PARAMETER      :: factor = 1.e-5
REAL(KIND=8),ALLOCATABLE    :: xi_ew(:)

!Statistic Variables Defination
REAL(KIND=8)                :: EnsMean_SIR(ITIMESTEP)
REAL(KIND=8)                :: EnsMean_OPD(ITIMESTEP)
REAL(KIND=8)                :: EnsMean_New(ITIMESTEP)
REAL(KIND=8)                :: EnsMean_EWPF(ITIMESTEP)
REAL(KIND=8)                :: Sumtmp
REAL(KIND=8)                :: Func


double precision, parameter :: zero = 0.0d0, one = 1.0d0, two = 2.0d0
double precision            :: s
integer                     :: ido

!Step 1. Initialize the B, R, Q Matrices and SVX(IDIMV,0)

ALLOCATE(SVX(1:IDIMV,0:ITIMESTEP,1:IDIMEn))
ALLOCATE(SVX0(1:IDIMV,0:ITIMESTEP))
ALLOCATE(SVXB(1:IDIMV,0:ITIMESTEP,1:IDIMEn))
ALLOCATE(SVXEW(1:IDIMV,0:ITIMESTEP,1:IDIMEn))
ALLOCATE(OBSY(1:ITIMESTEP,1:NumOfObs))
ALLOCATE(Di(1:IDIMV,1:IDIMEn))
ALLOCATE(DiT(1:IDIMEn,1:IDIMV))
ALLOCATE(Di2(1:IDIMV,1))
ALLOCATE(DiT2(1,1:IDIMV))
ALLOCATE(PHI(1:IDIMEn,1:IDIMEn))
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

BMat = 1.0
QMat = 0.01
RMat = 0.16
HMat = 1

!! 1.2 Init the SVX(:,0)
SVX(1:IDIMV,0,1:IDIMEn) = 0.0   !Traj 1
SVX0(1:IDIMV,0) = 0.0   !Take it as the Truth
SVXB(1:IDIMV,0,1:IDIMEn) = 0.0   !Backup of Traj 1, Traj 1'
SVXEW(1:IDIMV,0,1:IDIMEn) = 0.0   !Backup of Traj 1, Traj 1'

! Step 2. Generate the Ensemble Member

ALLOCATE(NDNumb(IDIMEn*IDIMV))!ALLOCATE the Output
!================THIS IS THE PLACE WE CHANGE SEED================

CALL RANDOM_SEED(PUT=SEEDA(:))

!! 2.1 Generate the initial Ensemble Xi(Dim,0,Ens)
NDMean    = 0
NDVar     = SQRT(BMat)
NDNumb(:) = 0.0 
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, NDNumb)

DO J = 1, IDIMEn
   DO I = 1,IDIMV
      SVX(I,0,J)  = SVX0(I,0) + NDNumb((J-1)*IDIMV+I)  !Every dimension of X is the same.
      SVXB(I,0,J) = SVX0(I,0) + NDNumb((J-1)*IDIMV+I)  !Every dimension of X is the same.
      SVXEW(I,0,J) = SVX0(I,0) + NDNumb((J-1)*IDIMV+I)  !Every dimension of X is the same.
   END DO
END DO

!! 2.X Additional Step for the evolution of the Model, I think it is f(x_i^{n-1}) + force
!! I need to change the equation here to the call of the model equations
!! Model Evolution For Every Variable in State Vector
ALLOCATE(ETA(IDIMV*IDIMEn))
NDMean    = 0
NDVar     = SQRT(QMat)
ETA(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMV*IDIMEn, ETA)

DO I = 1, IDIMEn
   DO J = 1, IDIMV
      CALL MODEL(Func,SVX(J,0,I),0,0)
      SVX(J,1,I)  = Func + ETA((I-1)*IDIMV+J) ! Do the evolution of the Model for Every Ensemble Member
      CALL MODEL(Func,SVXB(J,0,I),0,0)
      SVXB(J,1,I) = Func + ETA((I-1)*IDIMV+J) ! Do the evolution of the Model for Every Ensemble Member
      CALL MODEL(Func,SVXEW(J,0,I),0,0)
      SVXEW(J,1,I) = Func + ETA((I-1)*IDIMV+J) ! Do the evolution of the Model for Every Ensemble Member
      !SVX(J,1,I)  = SVX(J,0,I) + ETA((I-1)*IDIMV+J) ! Do the evolution of the Model for Every Ensemble Member
      !SVXB(J,1,I) = SVXB(J,0,I) + ETA((I-1)*IDIMV+J) ! Do the evolution of the Model for Every Ensemble Member
   END DO
END DO
 
ALLOCATE(ETATRUTH(IDIMV))
NDMean    = 0
NDVar     = SQRT(QMat)
ETA(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMV, ETATRUTH)
DO I = 1, IDIMV
   CALL MODEL(Func,SVX0(I,0),0,0)
   SVX0(I,1) = Func + ETATRUTH(I) ! Do the Model run for the Truth, There is no uncertainty here.
   !SVX0(I,1) = SVX0(I,0) + ETATRUTH(I) ! Do the Model run for the Truth, There is no uncertainty here.
END DO

TruthRun(1) = SUM(SVX0(:,1))/IDIMV

!! 2.2 Generate the Y(ITIMESTEP, NumOfObs)
ALLOCATE(EPSIL(NumOfObs))
NDMean   = 0
NDVar    = SQRT(RMat)
EPSIL(:) = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, NumOfObs, EPSIL)
XVec(:,1) = SVX0(:,1)
!HXMat = MATMUL(HMat,XVec)
!OBSY(1,1:NumOfObs) = HXMat(1:NumOfObs,1) + EPSIL(1:NumOfObs)
OBSY(1,1:NumOfObs) = HMat*XVec(1:IDIMV,1) + EPSIL(1:NumOfObs) !IDIMV = NumOfObs

! 3. Do the different Algorithms of Particle Filters, just for 1 timestep

!==============================================================================
!! 3.1 the Sequential Importance Resampling Algorithm

!!! 3.1.1 the Weights Calculation

CALL CALW(Weights,QMat,RMat,HMat,SVX,OBSY,IDIMV,1,1,IDIMEn,NumOfObs,1)

!! Still need some statistic Variables here.
!! 3.1.2 the Ensemble Mean

CALL ENSMEAN(EnsMean_SIR(1),SVX(:,1,:),IDIMV,IDIMEn,Weights)
!PRINT*,"THE ENSEMBLE MEAN OF SIR IS = "
!PRINT*,EnsMean_SIR(1)

!==============================================================================
!! 3.2 Perform the Optimal Proposal Density Algorithm
!!! 3.2.1 Calculate the GMat and PMatInv for the G(x*) expression
!!! I need to make sure that the Multiplications of the Matrices are correct.

CALL CALW(Weights,QMat,RMat,HMat,SVX,OBSY,IDIMV,1,1,IDIMEn,NumOfObs,2)

!GMat2 = Q*HT*(H*Q*HT+R)^(-1)
HTMat = HMat
GMat1 = HMat*QMat*HTMat + RMat
MATMP = 1/GMat1
GMat2 = QMat*HTMat*MATMP

!P^{-1} = Q^{-1}+HT*R^{-1}*H
QIMat = 1/QMat
RIMat = 1/RMat
PMatInv = HTMat*RIMat*HMat + QIMat
PMat = 1/PMatInv

!!! 3.2.2 Calculate the di for the G(x*) expression
Di2(:,:) = 0.0
DiT2(:,:) = 0.0
DO I = 1, IDIMEn
   XVec(:,1) = SVX(:,0,I)
   !HXMat = MATMUL(HMat,XVec)
   DO J = 1, NumOfObs
      CALL MODEL(Func,XVec(J,1),0,0)
      DiT2(1,J) = OBSY(1,J) - HMat*Func !Simplified Equation For H=1 and model equation
      Di2(J,1) = DiT2(1,J)
   END DO
   Di(:,I) = Di2(:,1)
   DiT(I,:) = DiT2(1,:)
END DO

!!! 3.2.3 Draw from the Optimal Proposal Density
!!!! 3.2.3.1 First calculate the value of P^{1/2}{\xi}
!PW(:,:) = PMat(:,:)
ALLOCATE(XI(IDIMEn*IDIMV))
NDMean   = 0
NDVar    = 1.0
XI(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, XI)

!!!==========================================================================
DO I = 1, IDIMEn
  DO J = 1, IDIMV
      UVec(J,I) = XI(J+(I-1)*IDIMV)
  END DO
END DO
!!!==============End of The calculation of P^{1/2}*{\xi}_i^n=================

!!!! 3.2.3.2 Finish the final step!
DO I = 1, IDIMEn
   DO J = 1, IDIMV
   CALL MODEL(Func,SVX(J,0,I),0,0)
   SVX(J,1,I) = GMat2*Di(J,I) + Func + (SQRT(PMat))*UVec(J,I) ! Add model equation in this equation
   !SVX(J,1,I) = GMat2*Di(J,I) + SVX(J,0,I) + (SQRT(PMat))*UVec(J,I) ! This equation is completed now.
   END DO
END DO

CALL CALW(Weights,QMat,RMat,HMat,SVX,OBSY,IDIMV,1,1,IDIMEn,NumOfObs,2)

!!! 3.2.4 Do the Statistic Step: Ensemble Mean

CALL ENSMEAN(EnsMean_OPD(1),SVX(:,1,:),IDIMV,IDIMEn,Weights)
!PRINT*,"THE ENSEMBLE MEAN OF OPD IS = "
!PRINT*,EnsMean_OPD(1)

!==============================================================================
!! 3.3 Perform the New Scheme

!! 3.3.1 Calculate the function of {\phi}_i and the det|S_i| = exp({PHI})

PHI =  MATMUL(DiT,MATMP*Di) !Only the diagnal elements are the values of {\phi}_i

Sumtmp = 0.0
DO I= 1, IDIMEn
   Sumtmp = Sumtmp + PHI(I,I)
END DO

PRINT*,"ALPHA = ,There always are some values that we cannot choose!"
DO I=1,IDIMEn
   Alpha(I) = EXP((PHI(I,I)-(Sumtmp/IDIMEn))/IDIMV)
   !Alpha(I) = EXP(PHI(I,I)/IDIMV)
   !PRINT*,"---------------"
   !PRINT*,Alpha(I)
   !PRINT*,(PHI(I,I) - Sumtmp/IDIMEn)/IDIMV
END DO

!!! 3.3.2 Draw from the New Scheme
ALLOCATE(XINEW(IDIMEn*IDIMV))
NDMean   = 0
NDVar    = 1.0
XINEW(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, XINEW)

!!!==========================================================================
UVec(:,:) = 0.0
DO I = 1, IDIMEn
  DO J = 1, IDIMV
      UVec(J,I) = XINEW(J+(I-1)*IDIMV)
  END DO
END DO
!!!==============End of The calculation of P^{1/2}*{\xi}_i^n=================

DO I = 1, IDIMEn
   DO J = 1, IDIMV
      XVec(J,1) = XINEW(J+(I-1)*IDIMV)
   END DO
   DO K = 1, IDIMV
      CALL MODEL(Func,SVXB(K,0,I),0,0)
      SVXB(K,1,I) = GMat2*Di(K,I) + Func + (SQRT(Alpha(I)*PMat))*UVec(K,I) ! Add model equation here.
   END DO
   XVecT = TRANSPOSE(XVec)
   TMPNUM = MATMUL(XVecT,XVec)
   OWeights(I) = TMPNUM(1,1)
END DO
!PRINT*,"SVXB = "
!PRINT*,SVXB(:,1,1)

!!! 3.3.3 Calculate the Weights
Weights(:) = (ABS(ALPHA(:)-1))*OWeights(:)
PRINT*,"=======================The -log(2*Weights) Of New Scheme============================"
PRINT*,Weights
PRINT*,"==================END OF THE LOGIRATHM OF WEIGHTS OF NEW SCHEME====================="
c_sort(:) = Weights(:)
CALL QsortC(c_sort)
Weights(:) = exp(-0.5*Weights(:) + 0.5*c_sort(1))
Weights(:) = Weights(:)/SUM(Weights(:))
PRINT*,"=======================The Weights Of New Scheme============================"
PRINT*,Weights
PRINT*,"=======================END OF THE WEIGHTS OF NEW SCHEME====================="

!!! 3.3.4 Do the Ensemble Mean Calculation
CALL ENSMEAN(EnsMean_New(1),SVXB(:,1,:),IDIMV,IDIMEn,Weights)
!PRINT*,"THE ENSEMBLE MEAN OF New Scheme IS = "
!PRINT*,EnsMean_New(:,1)

!==============================================================================
!! 3.3 Perform the EWPF
! a_i = 0.5 d^T_i R^{-1} H K d_i                     ------RIMat*HMat*GMat2
! b_i = 0.5 d^T_i R^{-1} d_i - C - log w^{rest}_i
! K   = Q H^T (HQH^T + R)^{-1}                       ------GMat2
! d_i = y^n - H f(x^{n-1}_i)                         ------Di(1:IDIMV,1:IDIMEn)
! alpha_i = 1 + sqrt(1 - b_i/a_i + 0.00000001)
! MATMP = (HQH^T + R)^{-1}
! CCC = 0.9 keep 90% of all the particles

w_rest = 0.0
DO I = 1, IDIMEn
   c_weights(I) = w_rest(I) + 0.5*PHI(I,I) !Only the diagnal elements are the values of {\phi}_i
   c_sort(I) = c_weights(I)
END DO

CALL QsortC(c_sort)
cc = c_sort(ccc*IDIMEn)

mat = MATMUL(DiT,RIMat*HMat*GMat2*Di)
DO I = 1, IDIMEn
   a_i(I) = 0.5 * mat(I,I)
END DO

mat = MATMUL(DiT,RIMat*Di)
DO I = 1, IDIMEn
   b_i(I) = 0.5 * mat(I,I) - cc + w_rest(I)
END DO

DO I = 1, IDIMEn
   IF(c_weights(I) <= cc) THEN
     alpha_i(I) = 1 + sqrt(1 - b_i(I)/a_i(I) + 0.00000001)
   END IF
END DO

DO I = 1, IDIMEn
   IF(c_weights(I) <= cc) THEN
     DO K = 1, IDIMV
       CALL MODEL(Func,SVXEW(K,0,I),0,0)
       SVXEW(K,1,I) = alpha_i(I)*GMat2*Di(K,I) + Func ! Add model equation here.
     END DO
   END IF
END DO

! Add Random Part of The Proposal and Recalculate the Weights
ALLOCATE(xi_ew(IDIMEn*IDIMV))
NDMean   = 0
NDVar    = 1.0
xi_ew(:)    = 0.0
CALL NDGen(SEEDA, NDMean, NDVar, IDIMEn*IDIMV, xi_ew)

DO I = 1, IDIMEn
   DO J = 1, IDIMV
     SVXEW(J,1,I) = SVXEW(J,1,I) + factor*sqrt(QMat)*xi_ew(J+IDIMV*(I-1))
     Di2(J,1) = OBSY(1,J) - HMat*SVXEW(J,1,I)
     DiT2(1,J) = Di2(J,1)
     CALL MODEL(Func,SVXEW(J,0,I),0,0)
     xtest(J,I) = SVXEW(J,1,I) - Func
     xtest_t(I,J) = xtest(J,I)
   END DO
   Di(:,I)  = Di2(:,1)
   DiT(I,:) = DiT2(1,:)
END DO

mat  = MATMUL(DiT,RIMat*Di)
mat2 = MATMUL(xtest_t,QIMat*xtest)

DO I = 1, IDIMEn
   Weights(I) = w_rest(I) + 0.5*mat(I,I) + 0.5*mat2(I,I)
END DO

c_sort(:) = Weights(:)
CALL QsortC(c_sort)

Weights(:) = exp(-Weights(:)+c_sort(1))
Weights(:) = Weights(:)/SUM(Weights(:))

PRINT*,"======================= The Weights Of EWPF ============================"
PRINT*,Weights
PRINT*,"=======================END OF THE WEIGHTS OF EWPF====================="

CALL ENSMEAN(EnsMean_EWPF(1),SVXEW(:,1,:),IDIMV,IDIMEn,Weights)

EnsembleMean(1,1) = EnsMean_SIR(1)
EnsembleMean(1,2) = EnsMean_OPD(1)
EnsembleMean(1,3) = EnsMean_New(1)
EnsembleMean(1,4) = EnsMean_EWPF(1)

!DEALLOCATE the Variables to free the memory

IF(ALLOCATED(SVX)) DEALLOCATE(SVX)
IF(ALLOCATED(SVX0)) DEALLOCATE(SVX0)
IF(ALLOCATED(SVXB)) DEALLOCATE(SVXB)
IF(ALLOCATED(OBSY)) DEALLOCATE(OBSY)
IF(ALLOCATED(Di)) DEALLOCATE(Di)
IF(ALLOCATED(DiT)) DEALLOCATE(DiT)
IF(ALLOCATED(Di2)) DEALLOCATE(Di2)
IF(ALLOCATED(DiT2)) DEALLOCATE(DiT2)
IF(ALLOCATED(PHI)) DEALLOCATE(PHI)
IF(ALLOCATED(ALPHA)) DEALLOCATE(ALPHA)
IF(ALLOCATED(OWeights)) DEALLOCATE(OWeights)
IF(ALLOCATED(Weights)) DEALLOCATE(Weights)

END SUBROUTINE
