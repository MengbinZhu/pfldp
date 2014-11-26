PROGRAM t_twodad

!****p* Programs/t_twodad *
!
! NAME
!    t_twodad - tests the compiled 2D fm model and adjoint functions
!
! SYNOPSIS
!    t_twodad
!
! DESCRIPTION
!    This program reads the 2D ECWMF input file available in ../data. This file
!    has precalculated bending angles. This program then
!    recalculates these profiles with random perturbations applied.
!    Correctness of the adjoint routines is checked by testing the definition
!    of the adjoint operator: test inner products of <Ax,y> = <x, A*y> equal.
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

REAL(wp) :: norm1,norm2

TYPE(state2dFM)   :: x,x_tl,x_ad
TYPE(Obs1dbangle) :: y,y_tl,y_ad

INTEGER               :: i, iargc, argc
CHARACTER(len =  256) :: buffer
LOGICAL               :: compress = .FALSE.

!-------------------------------------------------------------------------------
! 2. Command line arguments
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
! 3. Read in the test data from the data directory, allocate x and y.
!-------------------------------------------------------------------------------

CALL get_test_data(x,y)

! Allocate the variables used in the adjoint test

CALL alloc_and_set_vars(x,x_tl,x_ad,y,y_tl,y_ad)

!-------------------------------------------------------------------------------
! 4. Compute tangent linear norm
!-------------------------------------------------------------------------------

! Use standard random number routine to generate random fields

CALL random_number(x_tl%temp(:,:))
CALL random_number(x_tl%pres(:,:))
CALL random_number(x_tl%geop(:,:))
CALL random_number(x_tl%shum(:,:))

!  increase the geopotential perturbations

x_tl%geop(:,:) = 100.0_wp*x_tl%geop(:,:)

! Call the tangent linear

IF (compress) x%non_ideal = .TRUE.

CALL ropp_fm_bangle_2d_tl(x,x_tl,y,y_tl)

! norm1 is calculated from the output of the TL routine
! ie, norm1  = (H.dx)^T * (H.dx)

norm1 = DOT_PRODUCT(y_tl%bangle(:),y_tl%bangle(:))

!-------------------------------------------------------------------------------
! 5. Compute adjoint norm
!-------------------------------------------------------------------------------

! initialise adjoint variables

x_ad%temp(:,:) = 0.0_wp
x_ad%pres(:,:) = 0.0_wp
x_ad%geop(:,:) = 0.0_wp
x_ad%shum(:,:) = 0.0_wp

! Set y_ad = (H.dx)

y_ad%bangle(:) = y_tl%bangle(:)

! call the adjoint routine

CALL ropp_fm_bangle_2d_ad(x,x_ad,y,y_ad)

! norm2 is calculated from the output of the AD routine
! and the initial perturbation.

norm2 = dot_product(RESHAPE(x_ad%temp,(/SIZE(x_ad%temp)/)),   &
                    RESHAPE(x_tl%temp,(/SIZE(x_tl%temp)/))) + &
        dot_product(RESHAPE(x_ad%pres,(/SIZE(x_ad%pres)/)),   &
                    RESHAPE(x_tl%pres,(/SIZE(x_tl%pres)/))) + &
        dot_product(RESHAPE(x_ad%geop,(/SIZE(x_ad%geop)/)),   &
                    RESHAPE(x_tl%geop,(/SIZE(x_tl%geop)/))) + &
        dot_product(RESHAPE(x_ad%shum,(/SIZE(x_ad%shum)/)),   &
                    RESHAPE(x_tl%shum,(/SIZE(x_tl%shum)/)))


write (6,'(a,2f15.3,f10.7)') 'Norms',norm1,norm2,norm1/norm2

IF(norm1/norm2 < 0.999_wp .OR. norm1/norm2 > 1.001_wp)THEN
  PRINT *," 2D bending angle TL/ADJOINT norm ratio test out of limits"
  PRINT *,''
  PRINT *,'********************************'
  PRINT *,'*** ropp_fm (t_twodad): FAIL ***'
  PRINT *,'********************************'
  PRINT *,''
ELSE
  PRINT *," 2D bending angle TL/ADJOINT norm ratio test close to unity"
  PRINT *,''
  PRINT *,'********************************'
  PRINT *,'*** ropp_fm (t_twodad): PASS ***'
  PRINT *,'********************************'
  PRINT *,''
ENDIF

END

!-------------------------------------------------------------------------------
! 6. Read 2D ECMWF text data
!-------------------------------------------------------------------------------

SUBROUTINE get_test_data(x,y)

USE ropp_fm_types

IMPLICIT NONE

TYPE(state2dFM),   INTENT(OUT) :: x
TYPE(Obs1dbangle), INTENT(OUT) :: y

INTEGER :: i,j,idum

OPEN (10,FILE='../data/ECMWF_2D_DATA.DAT',STATUS='OLD')

READ (10,*)
READ (10,*)

! read the number of bending angles

READ (10,'(I8)') y%nobs

! allocate the arrays in the observation structure

ALLOCATE(y%impact(y%nobs))
ALLOCATE(y%bangle(y%nobs))
ALLOCATE(y%rtan(y%nobs))
ALLOCATE(y%a_path(y%nobs,2))


READ (10,'(5F15.2)') y%lat,y%lon,y%azimuth,y%undulation,y%r_curve

READ (10,'(20F15.2)') (y%impact(i),i=1,y%nobs)
READ (10,'(15F16.8)') (y%bangle(i),i=1,y%nobs)

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

! read in the nwp information

DO j = 1,x%n_horiz
   READ (10,'(2f15.2)') x%lat(j),x%lon(j)
   DO i = 1,x%n_lev
      READ (10,'(I6,4E16.8)') idum,x%pres(i,j),x%temp(i,j),x%shum(i,j),x%geop(i,j)
   ENDDO
ENDDO

CLOSE(10)
END

!-------------------------------------------------------------------------------
! 6. Allocate and initialise TL and AD variables
!-------------------------------------------------------------------------------

SUBROUTINE alloc_and_set_vars(x,x_tl,x_ad,y,y_tl,y_ad)

USE ropp_fm_types

IMPLICIT NONE

TYPE(state2dFM),   INTENT(IN) :: x
TYPE(Obs1dbangle), INTENT(IN) :: y
TYPE(state2dFM),   INTENT(OUT) :: x_tl,x_ad
TYPE(Obs1dbangle), INTENT(OUT) :: y_tl,y_ad

! allocate x_tl

ALLOCATE(x_tl%temp(x%n_lev,x%n_horiz))
ALLOCATE(x_tl%shum(x%n_lev,x%n_horiz))
ALLOCATE(x_tl%pres(x%n_lev,x%n_horiz))
ALLOCATE(x_tl%geop(x%n_lev,x%n_horiz))

! allocate x_ad

ALLOCATE(x_ad%temp(x%n_lev,x%n_horiz))
ALLOCATE(x_ad%shum(x%n_lev,x%n_horiz))
ALLOCATE(x_ad%pres(x%n_lev,x%n_horiz))
ALLOCATE(x_ad%geop(x%n_lev,x%n_horiz))

! allocate y_tl

ALLOCATE(y_tl%bangle(y%nobs))

! allocate y_ad

ALLOCATE(y_ad%bangle(y%nobs))

END

