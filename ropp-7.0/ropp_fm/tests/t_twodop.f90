PROGRAM t_twodop

!****p* Programs/t_twodop *
!
! NAME
!    t_twodop - tests the compiled 2D fm model with a precalculated set of
!               profiles
!
! SYNOPSIS
!    t_twodop
!
! DESCRIPTION
!    This program reads the text 2D input file available in ../data. This file
!    has precalculated bending angles. This program then
!    recalculates these profiles and compares it to the precalculated data. It
!    is a modified version of ropp_fm_bg2ro_2d.f90.
!
! NOTES
!
! EXAMPLE
!
! SEE ALSO
!
! AUTHOR
!   ECMWF, UK.
!   Any comments on this software should be given via the ROM SAF
!   Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

!-------------------------------------------------------------------------------
! 1. Declarations
!-------------------------------------------------------------------------------

USE typesizes, only : wp => EightByteReal
USE ropp_fm
USE ropp_fm_types

INTEGER :: i,iargc,argc,ifail
REAL(wp) :: diff
REAL(wp), PARAMETER :: rtol=0.0001_wp

LOGICAL   :: compress = .FALSE.

TYPE(state2dFM) :: x
TYPE(Obs1dbangle) :: y,y_ec
CHARACTER(len =  256)                            :: buffer

!-------------------------------------------------------------------------------
! 2. Command line areguments
!-------------------------------------------------------------------------------

argc = iargc()
i = 1

DO WHILE(i <= argc)
   CALL getarg(i, buffer)
   SELECT CASE (buffer)
      CASE('-comp')                       ! Non-ideal gas corrections
           compress = .TRUE.
      CASE default                        ! Nothing
   END SELECT
   i = i + 1
END DO

!-------------------------------------------------------------------------------
! 3. Read the data in from a text file in the data directory
!-------------------------------------------------------------------------------

CALL get_test_data(x,y,y_ec,compress)

!-------------------------------------------------------------------------------
! 4. Call the 2d operator
!-------------------------------------------------------------------------------

IF (compress) x%non_ideal = .TRUE.

CALL ropp_fm_bangle_2d(x,y)

!-------------------------------------------------------------------------------
! 5. Compare the results with those computed at ECMWF
!-------------------------------------------------------------------------------

ifail = 0

DO i = 1, y%nobs

   diff = ABS((y%bangle(i)-y_ec%bangle(i))/y%bangle(i))

!!!   write (6,*) i,'**',ABS((y%bangle(i)-y_ec%bangle(i))/y%bangle(i))

   IF (diff > rtol) THEN
      ifail = ifail + 1

      WRITE (6,*) 'WARNING: FAILED in simulated bending angle check!'
      WRITE (6,'(I6,F8.1,3E16.8)') &
      i,y%impact(i)-y%r_curve,(y%bangle(i)-y_ec%bangle(i))/y%bangle(i),y%bangle(i),y_ec%bangle(i)

      WRITE (6,*) ''
      WRITE (6,*) '********************************'
      WRITE (6,*) '*** ropp_fm (t_twodop): FAIL ***'
      WRITE (6,*) '********************************'
      WRITE (6,*) ''
   ENDIF

ENDDO

IF (ifail == 0) THEN
  WRITE (6,*) 'PASS: All % differences in bending angle <',rtol
  WRITE (6,*) ''
  WRITE (6,*) '********************************'
  WRITE (6,*) '*** ropp_fm (t_twodop): PASS ***'
  WRITE (6,*) '********************************'
  WRITE (6,*) ''
ENDIF

!!!!CALL write_test_data(x,y)

END

!-------------------------------------------------------------------------------
! 6. 2D ECMWF data read
!-------------------------------------------------------------------------------

SUBROUTINE get_test_data(x,y,y_ec,compress)

USE ropp_fm_types

IMPLICIT NONE

TYPE(state2dFM),   INTENT(OUT) :: x
TYPE(Obs1dbangle), INTENT(OUT) :: y,y_ec

LOGICAL compress

INTEGER :: i,j

IF (compress) THEN

   OPEN (10,FILE='../data/ECMWF_2D_DATA_COMP.DAT',STATUS='OLD')
!!   OPEN (10,FILE='../data/ECMWF_2D_DATA.DAT',STATUS='OLD')
ELSE
   OPEN (10,FILE='../data/ECMWF_2D_DATA.DAT',STATUS='OLD')
ENDIF

READ (10,*)
READ (10,*)

! read the number of bending angles in profile

READ (10,'(I8)') y%nobs

! allocate the arrays in the observation structure

ALLOCATE(y%impact(y%nobs))
ALLOCATE(y%bangle(y%nobs))
ALLOCATE(y%rtan(y%nobs))
ALLOCATE(y%a_path(y%nobs,2))

! for storing the ecmwf results

ALLOCATE(y_ec%bangle(y%nobs))

! location of observation

READ (10,'(5F15.2)') y%lat,y%lon,y%azimuth,y%undulation,y%r_curve

! impact parameter information

READ (10,'(20F15.2)') (y%impact(i),i=1,y%nobs)

! bending angles calculated at ECMWF

READ (10,'(15E16.8)') (y_ec%bangle(i),i=1,y%nobs)

! state information

READ (10,'(2I8,E16.8)') x%n_lev,x%n_horiz,x%dtheta

! allocate the state vector information

ALLOCATE(x%lat(x%n_horiz))
ALLOCATE(x%lon(x%n_horiz))
ALLOCATE(x%temp(x%n_lev,x%n_horiz))
ALLOCATE(x%shum(x%n_lev,x%n_horiz))
ALLOCATE(x%pres(x%n_lev,x%n_horiz))
ALLOCATE(x%geop(x%n_lev,x%n_horiz))
ALLOCATE(x%refrac(x%n_lev,x%n_horiz))
ALLOCATE(x%nr(x%n_lev,x%n_horiz))

! read in the nwp profile information

DO j = 1,x%n_horiz
   READ (10,'(2f15.2)') x%lat(j),x%lon(j)
   DO i = 1,x%n_lev
      READ (10,'(6X,4E16.8)') x%pres(i,j),x%temp(i,j),x%shum(i,j),x%geop(i,j)
   ENDDO
ENDDO

CLOSE(10)
END

!-------------------------------------------
! Output test data
!--------------------------------------------

SUBROUTINE write_test_data(x,y)

USE ropp_fm_types

IMPLICIT NONE

TYPE(state2dFM),   INTENT(OUT) :: x
TYPE(Obs1dbangle), INTENT(OUT) :: y

INTEGER :: i,j

OPEN (10,FILE='../data/ECMWF_2D_DATA_COMP.OUT',STATUS='NEW')

WRITE (10,*)
WRITE (10,*)

! read the number of bending angles in profile

WRITE (10,'(I8)') y%nobs

! location of observation

WRITE (10,'(5F15.2)') y%lat,y%lon,y%azimuth,y%undulation,y%r_curve

! impact parameter information

WRITE (10,'(20F15.2)') (y%impact(i),i=1,y%nobs)

! bending angles calculated at ECMWF

WRITE (10,'(15E16.8)') (y%bangle(i),i=1,y%nobs)

! state information

WRITE (10,'(2I8,E16.8)') x%n_lev,x%n_horiz,x%dtheta


DO j = 1,x%n_horiz
   WRITE (10,'(2f15.2)') x%lat(j),x%lon(j)
   DO i = 1,x%n_lev
      WRITE (10,'(I6,4E16.8)') i,x%pres(i,j),x%temp(i,j),x%shum(i,j),x%geop(i,j)
   ENDDO
ENDDO

CLOSE(10)
END
