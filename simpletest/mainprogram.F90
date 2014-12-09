PROGRAM MAINPROGRAM

IMPLICIT NONE

INTEGER(KIND=4),PARAMETER   :: IDIMEn = 10    ! Number of Ensemble Size
INTEGER(KIND=4),PARAMETER   :: IDIMV  = 400   ! Size Number of the State Variables = 10
INTEGER(KIND=4),PARAMETER   :: ITIMESTEP  = 1 ! Number of Time Steps
INTEGER(KIND=4),PARAMETER   :: NumOfObs = 400 ! Number of Observations, always the same with IDIMV
INTEGER(KIND=4),ALLOCATABLE :: SEEDA(:)       ! Seed of the Random Number
REAL(KIND=8)                :: EnsembleMean(ITIMESTEP,3)
REAL(KIND=8)                :: TruthRun(ITIMESTEP)
REAL(KIND=8)                :: RMSE_DataEns(10,3)
REAL(KIND=8)                :: RMSE_DataTruth(10)
REAL(KIND=8)                :: RMSE_Ens(3)
REAL(KIND=8)                :: RMSE_Truth

REAL(KIND=8)                :: MEAN_ENS(3)
REAL(KIND=8)                :: MEAN_Truth

REAL(KIND=8)                :: RMSE_SUM

INTEGER(KIND=4)             :: I,J,K

ALLOCATE(SEEDA(IDIMEn*IDIMV))

RMSE_SUM = 0.0

DO I = 1, 10
   SEEDA(:) = 8*I
   CALL SIMEXP(SEEDA,IDIMEn,IDIMV,ITIMESTEP,NumOfObs,EnsembleMean,TruthRun)
   RMSE_DataEns(I,:) = EnsembleMean(1,:)
   RMSE_DataTruth(I) = TruthRun(1)
END DO

DO J = 1, 3
   MEAN_ENS(J) = SUM(RMSE_DataEns(:,J))/10
END DO
MEAN_Truth = SUM(RMSE_DataTruth(:))/10

DO I = 1, 3
   RMSE_SUM = 0.0
   DO K = 1, IDIMV
      DO J = 1, 10
         RMSE_SUM = RMSE_SUM + (RMSE_DataEns(J,I) - MEAN_ENS(I))**2
      END DO
      RMSE_Ens(I) = SQRT(RMSE_SUM/10)
   END DO
END DO

RMSE_SUM = 0.0
DO J = 1, 10
   RMSE_SUM =  RMSE_SUM + (RMSE_DataTruth(J)-MEAN_Truth)**2
END DO
RMSE_Truth = SQRT(RMSE_SUM/10)

PRINT*,"================================================="
PRINT*,"THE RMSE OF THE TRUTH RUN IS = "
PRINT*,RMSE_Truth
PRINT*,"THE RMSE OF ENSEMBLE MEAN OF SIR IS = "
PRINT*,RMSE_Ens(1)
PRINT*,"THE RMSE OF ENSEMBLE MEAN OF OPD IS = "
PRINT*,RMSE_Ens(2)
PRINT*,"THE RMSE OF ENSEMBLE MEAN OF New IS = "
PRINT*,RMSE_Ens(3)
PRINT*,"================================================="

END PROGRAM
