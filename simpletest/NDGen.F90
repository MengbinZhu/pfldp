SUBROUTINE NDGen(SEED,NDMean,NDVar,IDIMV,NDNumb)

IMPLICIT NONE

INTEGER(KIND=4), INTENT(IN)      :: IDIMV
INTEGER(KIND=4), INTENT(INOUT)   :: SEED(IDIMV)
REAL(KIND=8)   , INTENT(IN)      :: NDMean,NDVar
REAL(KIND=8)   , INTENT(OUT)     :: NDNumb(IDIMV)
REAL(KIND=8)                     :: UDX1(IDIMV), UDX2(IDIMV)
REAL(KIND=8)                     :: NNum1(IDIMV), NNum2(IDIMV)
REAL(KIND=8)                     :: TMP(IDIMV)

INTEGER                          :: I

TMP(:) = 1.0
CALL RANDOM_SEED(PUT=SEED(1:IDIMV))

DO I=1,IDIMV

DO WHILE(TMP(I) >= 1.0)
CALL RANDOM_NUMBER(UDX1(I))
CALL RANDOM_NUMBER(UDX2(I))

!print *,UDX1(:)

UDX1(I) = 2.0 * UDX1(I) - 1.0
UDX2(I) = 2.0 * UDX2(I) - 1.0 
TMP(I) = UDX1(I)*UDX1(I) + UDX2(I)*UDX2(I)
END DO

END DO

TMP(:) = SQRT((- 2.0 * log(TMP(:)))/TMP(:))

NNum1(:) = UDX1(:)*TMP(:)
NNum2(:) = UDX2(:)*TMP(:)

NDNumb(:) = NDMean + NDVar*NNum2(:)

!print *,NDNumb(:)

END SUBROUTINE NDGen
