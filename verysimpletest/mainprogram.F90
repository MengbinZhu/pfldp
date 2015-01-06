PROGRAM MAINPROGRAM

IMPLICIT NONE

INTEGER(KIND=4)             :: IDIMEn = 20    ! Number of Ensemble Size
INTEGER(KIND=4)             :: IDIMV  = 1000   ! Size Number of the State Variables = 10
INTEGER(KIND=4),PARAMETER   :: ITIMESTEP  = 1 ! Number of Time Steps
INTEGER(KIND=4)             :: NumOfObs = 1000 ! Number of Observations, always the same with IDIMV
INTEGER(KIND=4),ALLOCATABLE :: SEEDA(:)       ! Seed of the Random Number
INTEGER(KIND=4),PARAMETER   :: RONDOM_TIMES = 10
REAL(KIND=8)                :: EnsembleMean(ITIMESTEP,4)
REAL(KIND=8)                :: TruthRun(ITIMESTEP)
REAL(KIND=8)                :: RMSE_DataEns(RONDOM_TIMES,4)
REAL(KIND=8)                :: RMSE_DataTruth(RONDOM_TIMES)
REAL(KIND=8)                :: RMSE_Ens(4)
REAL(KIND=8)                :: RMSE_Truth

REAL(KIND=8)                :: MEAN_ENS(4)
REAL(KIND=8)                :: MEAN_Truth

REAL(KIND=8)                :: RMSE_SUM

INTEGER(KIND=4)             :: I,J,K

NAMELIST /NADIM/ IDIMEn, IDIMV, NumOfObs 
OPEN(10,FILE="nadim.namelist")
READ(10,nml=NADIM)

ALLOCATE(SEEDA(IDIMEn*IDIMV))

!WRITE(*,nml=NADIM)
WRITE(*,*)IDIMEn
WRITE(*,*)IDIMV

RMSE_SUM = 0.0

DO I = 1, RONDOM_TIMES
   SEEDA(:) = 6*I
   CALL SIMEXP(SEEDA,IDIMEn,IDIMV,ITIMESTEP,NumOfObs,EnsembleMean,TruthRun)
   RMSE_DataEns(I,:) = EnsembleMean(1,:)
   RMSE_DataTruth(I) = TruthRun(1)
END DO

DO J = 1, 4
   MEAN_ENS(J) = SUM(RMSE_DataEns(:,J))/RONDOM_TIMES
END DO
MEAN_Truth = SUM(RMSE_DataTruth(:))/RONDOM_TIMES

DO I = 1, 4
   RMSE_SUM = 0.0
   DO K = 1, IDIMV
      DO J = 1, RONDOM_TIMES
         RMSE_SUM = RMSE_SUM + (RMSE_DataEns(J,I) - MEAN_ENS(I))**2
      END DO
      RMSE_Ens(I) = SQRT(RMSE_SUM/10)
   END DO
END DO

RMSE_SUM = 0.0
DO J = 1, RONDOM_TIMES
   RMSE_SUM =  RMSE_SUM + (RMSE_DataTruth(J)-MEAN_Truth)**2
END DO
RMSE_Truth = SQRT(RMSE_SUM/RONDOM_TIMES)

PRINT*,"================================================="
PRINT*,"THE RMSE OF THE TRUTH RUN IS = "
PRINT*,RMSE_Truth
PRINT*,"THE RMSE OF ENSEMBLE MEAN OF SIR IS = "
PRINT*,RMSE_Ens(1)
PRINT*,"THE RMSE OF ENSEMBLE MEAN OF OPD IS = "
PRINT*,RMSE_Ens(2)
PRINT*,"THE RMSE OF ENSEMBLE MEAN OF New IS = "
PRINT*,RMSE_Ens(3)
PRINT*,"THE RMSE OF ENSEMBLE MEAN OF EWPF IS = "
PRINT*,RMSE_Ens(4)
PRINT*,"================================================="

END PROGRAM
