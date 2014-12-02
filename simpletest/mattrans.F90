SUBROUTINE MATTRANS(MOriginal,MTrans,ColDim,RowDim)

IMPLICIT NONE

REAL(KIND=8),INTENT(IN)    :: ColDim, RowDim
REAL(KIND=8),INTENT(IN)    :: MOriginal(ColDim,RowDim)
REAL(KIND=8),INTENT(INOUT) :: MTrans(RowDim,ColDim)

REAL(KIND=8)               :: I,J

DO I = 1, ColDim
   DO J = 1, RowDim
      MTrans(J,I) = MOriginal(I,J)
   END DO
END DO
END SUBROUTINE
