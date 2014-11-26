PROGRAM t_twodtl

!****p* Programs/t_twodtl *
!
! NAME
!    t_twodtl - tests the compiled 2D fm model and tangent linear functions
!
! SYNOPSIS
!    t_twodtl
!
! DESCRIPTION
!    This program reads the 2D ECWMF input file available in ../data. This file
!    has precalculated bending angles.  This program then
!    recalculates these profiles with random perturbations applied.
!    Correctness of the tangent linear routines is tested by ensuring the
!    relative error in the tangent linear tends to zero as the angle between
!    vectors tends to zero (cos angle -> 1)
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

REAL(wp) :: cos_alpha,rel_err

TYPE(state2dFM)   :: x,x_new,x_tl
TYPE(Obs1dbangle) :: y,y_new,y_save,y_tl

REAL(wp) :: max_alpha, min_err
INTEGER  :: imax, imin

INTEGER               :: i, iargc, argc
CHARACTER(len =  256) :: buffer
LOGICAL               :: compress = .FALSE.

!-------------------------------------------------------------------------------
! 1. Command line arguments
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
! 2. Read in the test data from the data directory, allocate x and y.
!-------------------------------------------------------------------------------

CALL get_test_data(x,y)

!-------------------------------------------------------------------------------
! 3. Call the 2d operator with test data
!-------------------------------------------------------------------------------

IF (COMPRESS) THEN
  x%non_ideal = .TRUE.
ENDIF

CALL ropp_fm_bangle_2d(x,y)

!-------------------------------------------------------------------------------
! 4. Test tangent linear
!-------------------------------------------------------------------------------

! Allocate the variables used in the tangent linear test

CALL alloc_and_set_vars(x,x_new,x_tl,y,y_save,y_new,y_tl)

IF (COMPRESS) THEN
  x_new%non_ideal = .TRUE.
ENDIF

! Save the bending angles calculated with the original field

y_save%bangle(:) = y%bangle(:)

! Use standard random number routine to generate random fields

CALL random_number(x_tl%temp(:,:))
CALL random_number(x_tl%pres(:,:))
CALL random_number(x_tl%geop(:,:))
CALL random_number(x_tl%shum(:,:))


!x_tl%geop(:,:) =0.0_wp
!x_tl%pres(:,:) =0.0_wp
!x_tl%shum(:,:) =0.0_wp
!x_tl%temp(:,:) =0.0_wp

!  increase the geopotential perturbations

x_tl%geop(:,:) = 100.0_wp*x_tl%geop(:,:)

! loop through, reducing the perturbation by a factor of 10

imax = -10000
Imin = 10000
max_alpha = -10000.0_wp
min_err = 10000.0_wp

DO i = 1,15

! update the perturbed variables

   x_new%temp(:,:) = x%temp(:,:) + x_tl%temp(:,:)
   x_new%pres(:,:) = x%pres(:,:) + x_tl%pres(:,:)
   x_new%geop(:,:) = x%geop(:,:) + x_tl%geop(:,:)
   x_new%shum(:,:) = x%shum(:,:) + x_tl%shum(:,:)

! simulate bending with perturbed variables

   CALL ropp_fm_bangle_2d(x_new,y_new)

! calculate the tangent linear

   CALL ropp_fm_bangle_2d_tl(x,x_tl,y,y_tl)

! angle between the vectors

   cos_alpha = DOT_PRODUCT(y_tl%bangle ,(y_new%bangle-y_save%bangle))/ &
               SQRT(DOT_PRODUCT(y_tl%bangle,y_tl%bangle)* &
               DOT_PRODUCT((y_new%bangle-y_save%bangle),(y_new%bangle-y_save%bangle)))

! relative error in the tangent linear

   rel_err = SQRT( Dot_Product((y_new%bangle(:) - y_save%bangle(:)-y_tl%bangle(:)),  &
            (y_new%bangle(:) - y_save%bangle(:) -y_tl%bangle(:)))) / &
             SQRT( Dot_Product((y_new%bangle(:) - y_save%bangle(:)),(y_new%bangle(:) - y_save%bangle(:))))


   write (6,*) i,cos_alpha,100.0*rel_err

   ! look for max/minimum values

   IF ( cos_alpha > max_alpha ) THEN
     max_alpha = cos_alpha
     imax = i
   ENDIF

   IF ( rel_err < min_err ) THEN
     min_err = rel_err
     imin = i
   ENDIF

! reduce the size of the perturbation by a factor of 10

   x_tl%temp = 0.1_wp*x_tl%temp
   x_tl%shum = 0.1_wp*x_tl%shum
   x_tl%pres = 0.1_wp*x_tl%pres
   x_tl%geop = 0.1_wp*x_tl%geop
ENDDO

! Check tangent linear
IF (imin == imax) THEN
  PRINT *, "   2D BENDING ANGLE TL check passed"
  PRINT *,''
  PRINT *,'********************************'
  PRINT *,'*** ropp_fm (t_twodtl): PASS ***'
  PRINT *,'********************************'
  PRINT *,''
ENDIF

IF (ABS(imin-imax) > 1) THEN
  PRINT *,''
  PRINT *,'********************************'
  PRINT *,'*** ropp_fm (t_twodtl): FAIL ***'
  PRINT *,'********************************'
  PRINT *,''
ENDIF

END

!-------------------------------------------------------------------------------
! 5. 2D ECMWF text file read
!-------------------------------------------------------------------------------

SUBROUTINE get_test_data(x,y)

USE ropp_fm_types

IMPLICIT NONE

TYPE(state2dFM),   INTENT(OUT) :: x
TYPE(Obs1dbangle), INTENT(OUT) :: y

INTEGER :: i,j,idum

OPEN (UNIT=10, FILE='../data/ECMWF_2D_DATA.DAT', STATUS='OLD')

READ (10, *)
READ (10, *)

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
! 6. Allocate and initialise TL variables
!-------------------------------------------------------------------------------

SUBROUTINE alloc_and_set_vars(x,x_new,x_tl,y,y_save,y_new,y_tl)

USE ropp_fm_types

IMPLICIT NONE

TYPE(state2dFM),   INTENT(IN) :: x
TYPE(Obs1dbangle), INTENT(IN) :: y
TYPE(state2dFM),   INTENT(OUT) :: x_new,x_tl
TYPE(Obs1dbangle), INTENT(OUT) :: y_new,y_save,y_tl


! allocate and set x_new

ALLOCATE(x_new%lat(x%n_horiz))
ALLOCATE(x_new%lon(x%n_horiz))
ALLOCATE(x_new%temp(x%n_lev,x%n_horiz))
ALLOCATE(x_new%shum(x%n_lev,x%n_horiz))
ALLOCATE(x_new%pres(x%n_lev,x%n_horiz))
ALLOCATE(x_new%geop(x%n_lev,x%n_horiz))
ALLOCATE(x_new%refrac(x%n_lev,x%n_horiz))
ALLOCATE(x_new%nr(x%n_lev,x%n_horiz))

x_new%n_lev       = x%n_lev
x_new%n_horiz     = x%n_horiz
x_new%dtheta      = x%dtheta
x_new%lat(:)      = x%lat(:)
x_new%lon(:)      = x%lon(:)
x_new%temp(:,:)   = x%temp(:,:)
x_new%shum(:,:)   = x%shum(:,:)
x_new%pres(:,:)   = x%pres(:,:)
x_new%geop(:,:)   = x%geop(:,:)

! allocate and set y_new

y_new%nobs = y%nobs
y_new%lat        = y%lat
y_new%lon        = y%lon
y_new%azimuth    = y%azimuth
y_new%undulation = y%undulation
y_new%r_curve    = y%r_curve
ALLOCATE(y_new%impact(y_new%nobs))
ALLOCATE(y_new%bangle(y_new%nobs))
ALLOCATE(y_new%rtan(y_new%nobs))
ALLOCATE(y_new%a_path(y_new%nobs,2))
y_new%impact(:)  = y%impact(:)

! allocate x_tl

ALLOCATE(x_tl%temp(x%n_lev,x%n_horiz))
ALLOCATE(x_tl%shum(x%n_lev,x%n_horiz))
ALLOCATE(x_tl%pres(x%n_lev,x%n_horiz))
ALLOCATE(x_tl%geop(x%n_lev,x%n_horiz))

! allocate y_tl

ALLOCATE(y_tl%bangle(y%nobs))

! allocate y_save

ALLOCATE(y_save%bangle(y%nobs))

END

