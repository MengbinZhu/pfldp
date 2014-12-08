PROGRAM MAINPROGRAM

IMPLICIT NONE

INTEGER(KIND=4),PARAMETER   :: IDIMEn = 10    ! Number of Ensemble Size
INTEGER(KIND=4),PARAMETER   :: IDIMV  = 100   ! Size Number of the State Variables = 10
INTEGER(KIND=4),PARAMETER   :: ITIMESTEP  = 1 ! Number of Time Steps
INTEGER(KIND=4),PARAMETER   :: NumOfObs = 1  ! Number of Observations
INTEGER(KIND=4),ALLOCATABLE :: SEEDA(:)       ! Seed of the Random Number
REAL(KIND=8)                :: EnsembleMean(IDIMV,ITIMESTEP,3)
REAL(KIND=8)                :: TruthRun(IDIMV,ITIMESTEP)
REAL(KIND=8)                :: RMSE_DataEns(IDIMV,10,3)
REAL(KIND=8)                :: RMSE_DataTruth(IDIMV,10)
REAL(KIND=8)                :: RMSE_Ens(IDIMV,3)
REAL(KIND=8)                :: RMSE_Truth(IDIMV)

REAL(KIND=8)                :: MEAN_ENS(IDIMV,3)
REAL(KIND=8)                :: MEAN_Truth(IDIMV)

REAL(KIND=8)                :: RMSE_SUM(IDIMV)

INTEGER(KIND=4)             :: I,J,K

ALLOCATE(SEEDA(IDIMEn*IDIMV))

RMSE_SUM(:) = 0.0

DO I = 1, 10
   SEEDA(:) = 8*I
   CALL SIMEXP(SEEDA,IDIMEn,IDIMV,ITIMESTEP,NumOfObs,EnsembleMean,TruthRun)
   RMSE_DataEns(:,I,:) = EnsembleMean(:,1,:)
   RMSE_DataTruth(:,I) = TruthRun(:,1)
END DO

DO I = 1, IDIMV
   DO J = 1, 3
      MEAN_ENS(I,J) = SUM(RMSE_DataEns(I,:,J))/10
   END DO
   MEAN_Truth(I) = SUM(RMSE_DataTruth(I,:))/10
END DO

DO I = 1, 3
   RMSE_SUM(:) = 0.0
   DO K = 1, IDIMV
      DO J = 1, 10
         RMSE_SUM(K) = RMSE_SUM(K) + (RMSE_DataEns(K,J,I) - MEAN_ENS(K,I))**2
         !RMSE_Ens(K,I) = SQRT(((RMSE_DataEns(K,J,I)-MEAN_ENS(K,I))**2)/10)
      END DO
      RMSE_Ens(K,I) = SQRT(RMSE_SUM(K)/10)
   END DO
END DO

RMSE_SUM(:) = 0.0
DO K = 1, IDIMV
   DO J = 1, 10
      RMSE_SUM(K) =  RMSE_SUM(K) + (RMSE_DataTruth(K,J)-MEAN_Truth(K))**2
   END DO
   RMSE_Truth(K) = SQRT(RMSE_SUM(K)/10)
END DO

PRINT*,"================================================="
PRINT*,"THE RMSE OF THE TRUTH RUN IS = "
PRINT*,RMSE_Truth(:)
PRINT*,"THE RMSE OF ENSEMBLE MEAN OF SIR IS = "
PRINT*,RMSE_Ens(:,1)
PRINT*,"THE RMSE OF ENSEMBLE MEAN OF OPD IS = "
PRINT*,RMSE_Ens(:,2)
PRINT*,"THE RMSE OF ENSEMBLE MEAN OF New IS = "
PRINT*,RMSE_Ens(:,3)
PRINT*,"================================================="

END PROGRAM
