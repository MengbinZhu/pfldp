! $Id: nml_diff.f90 2197 2009-06-23 09:11:17Z frhl $

PROGRAM nml_diff

!****pi* Tests/nml_diff *
!
! NAME
!    nml_diff - Compares two bgr_profile namelists for meaningful differences.
!               Needed by t_grib2bgrasc.sh
!
! SYNOPSIS
!    nml_diff file1.nml file2.nml
! 
! DESCRIPTION
!    Compares two bgr_profile namelists.
!    Needed by t_grib2bgrasc.sh
!
! INPUTS
!    Fortran namelist files file1.nml and file2.nml.
!
! OUTPUT
!    Reports (to unit 6) whether the elements of the namelist are "similar".
!
! NOTES
!
! REFERENCES
!
! AUTHOR
!    Met Office, Exeter, UK.
!    Any comments on this software should be given via the ROM SAF
!    Helpdesk at http://www.romsaf.org
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

  USE typesizes,     wp => EightByteReal
  USE messages
  USE ropp_utils,    ONLY: ropp_MDFV, ropp_MDTV

  IMPLICIT NONE

  INTEGER, PARAMETER  :: Nlevs_max=200                 ! Maximum number of model levels

! Definition of namelist
  INTEGER             :: NLevs                        ! Number of full model levels
  INTEGER             :: year, mon, day               ! Background validity date
  INTEGER             :: hour, min, sec               ! Background validity time
  REAL(wp)            :: tlead                        ! Background forecast range (hr)
  REAL(wp)            :: lon, lat                     ! Tangent point lat, lon (deg)
  REAL(wp)            :: und=ropp_MDFV                ! Tangent point undulation (m)
  REAL(wp)            :: roc=ropp_MDFV                ! Tangent point radius of curvature (m)
  REAL(wp), DIMENSION(3) :: coc=(/ropp_MDFV, ropp_MDFV, ropp_MDFV/)  ! Tangent point centre of curvature (m)
  REAL(wp)            :: azi=ropp_MDFV                ! GNSS->LEO line of sight angle (degT)
  REAL(wp)                         :: Z0=ropp_MDFV    ! Surface geopotential height (m)
  REAL(wp)                         :: P0=ropp_MDFV    ! Surface pressure (Pa)
  REAL(wp), DIMENSION(Nlevs_max)   :: P=ropp_MDFV     ! Pressure (Pa)
  REAL(wp), DIMENSION(Nlevs_max)   :: T=ropp_MDFV     ! Temperature (K)
  REAL(wp), DIMENSION(Nlevs_max)   :: Q=ropp_MDFV     ! Specific humidity (g/kg)
  REAL(wp), DIMENSION(Nlevs_max)   :: Z=ropp_MDFV     ! Geopotential height (m)
  REAL(wp), DIMENSION(Nlevs_max+1) :: Ak=ropp_MDFV    ! Hybrid/Eta level A-coefficient (Pa)
  REAL(wp), DIMENSION(Nlevs_max+1) :: Bk=ropp_MDFV    ! Hybrid/Eta level B-coefficient

  NAMELIST / bgr_profile / year, mon, day, &
                           hour, min, sec, &
                           tlead, &
                           lon, lat, und, roc, coc, azi, &
                           Z0, P0, &
                           Nlevs, &
                           P, T, Q, Z, Ak, Bk

! Copy of namelist from file_in1
  CHARACTER (LEN=256) :: file_in1                      ! I/P file name
  INTEGER             :: NLevs1                        ! Number of full model levels
  INTEGER             :: year1, mon1, day1             ! Background validity date
  INTEGER             :: hour1, min1, sec1             ! Background validity time
  REAL(wp)            :: tlead1                        ! Background forecast range (hr)
  REAL(wp)            :: lon1, lat1                    ! Tangent point lat, lon (deg)
  REAL(wp)            :: und1=ropp_MDFV                ! Tangent point undulation (m)
  REAL(wp)            :: roc1=ropp_MDFV                ! Tangent point radius of curvature (m)
  REAL(wp), DIMENSION(3) :: coc1=(/ropp_MDFV, ropp_MDFV, ropp_MDFV/)  ! Tangent point centre of curvature (m)
  REAL(wp)            :: azi1=ropp_MDFV                ! GNSS->LEO line of sight angle (degT)
  REAL(wp)                         :: Z01=ropp_MDFV    ! Surface geopotential height (m)
  REAL(wp)                         :: P01=ropp_MDFV    ! Surface pressure (Pa)
  REAL(wp), DIMENSION(Nlevs_max)   :: P1=ropp_MDFV     ! Pressure (Pa)
  REAL(wp), DIMENSION(Nlevs_max)   :: T1=ropp_MDFV     ! Temperature (K)
  REAL(wp), DIMENSION(Nlevs_max)   :: Q1=ropp_MDFV     ! Specific humidity (g/kg)
  REAL(wp), DIMENSION(Nlevs_max)   :: Z1=ropp_MDFV     ! Geopotential height (m)
  REAL(wp), DIMENSION(Nlevs_max+1) :: Ak1=ropp_MDFV    ! Hybrid/Eta level A-coefficient (Pa)
  REAL(wp), DIMENSION(Nlevs_max+1) :: Bk1=ropp_MDFV    ! Hybrid/Eta level B-coefficient

! Copy of namelist from file_in2
  CHARACTER (LEN=256) :: file_in2                      ! I/P file name
  INTEGER             :: NLevs2                        ! Number of full model levels
  INTEGER             :: year2, mon2, day2             ! Background validity date
  INTEGER             :: hour2, min2, sec2             ! Background validity time
  REAL(wp)            :: tlead2                        ! Background forecast range (hr)
  REAL(wp)            :: lon2, lat2                    ! Tangent point lat, lon (deg)
  REAL(wp)            :: und2=ropp_MDFV                ! Tangent point undulation (m)
  REAL(wp)            :: roc2=ropp_MDFV                ! Tangent point radius of curvature (m)
  REAL(wp), DIMENSION(3) :: coc2=(/ropp_MDFV, ropp_MDFV, ropp_MDFV/)  ! Tangent point centre of curvature (m)
  REAL(wp)            :: azi2=ropp_MDFV                ! GNSS->LEO line of sight angle (degT)
  REAL(wp)                         :: Z02=ropp_MDFV    ! Surface geopotential height (m)
  REAL(wp)                         :: P02=ropp_MDFV    ! Surface pressure (Pa)
  REAL(wp), DIMENSION(Nlevs_max)   :: P2=ropp_MDFV     ! Pressure (Pa)
  REAL(wp), DIMENSION(Nlevs_max)   :: T2=ropp_MDFV     ! Temperature (K)
  REAL(wp), DIMENSION(Nlevs_max)   :: Q2=ropp_MDFV     ! Specific humidity (g/kg)
  REAL(wp), DIMENSION(Nlevs_max)   :: Z2=ropp_MDFV     ! Geopotential height (m)
  REAL(wp), DIMENSION(Nlevs_max+1) :: Ak2=ropp_MDFV    ! Hybrid/Eta level A-coefficient (Pa)
  REAL(wp), DIMENSION(Nlevs_max+1) :: Bk2=ropp_MDFV    ! Hybrid/Eta level B-coefficient

! Local variables
  INTEGER,  PARAMETER :: funit=11                     ! Lun to which namelist is attached
  INTEGER             :: narg                         ! No. of command line arguments
  INTEGER             :: iostatus                     ! I/O Status
  INTEGER             :: idiff                        ! Difference counter
  CHARACTER(LEN=3)    :: sdiff                        ! STRING(difference counter)
  CHARACTER(LEN=256)  :: test_name=""                 ! Name of test
  LOGICAL             :: exists                       ! File exists flag

! Some compilers may need the following declaration to be commented out
  INTEGER             :: IARGC


! 0. Read instructions
! --------------------

  narg = IARGC()

  IF (narg < 2) THEN
    CALL pass_fail(.false., test_name)
    CALL message ( msg_fatal, "nml_diff needs at least two arguments" )
  ENDIF
  
  file_in1 =  " "       ! no default for i/p file name
  CALL GETARG ( 1, file_in1 )

  file_in2 =  " "       ! no default for i/p file name
  CALL GETARG ( 2, file_in2 )

  IF (narg >= 3) CALL GETARG ( 3, test_name )

  CALL message ( msg_info, "Using nml_diff to compare " // &
                           TRIM(ADJUSTL(file_in1)) // " and " // &
                           TRIM(ADJUSTL(file_in2)) )


! 1. Read 1st namelist
! --------------------
  
  INQUIRE ( FILE=file_in1, EXIST=exists )
  IF ( .NOT. exists ) THEN
    CALL pass_fail(.false., test_name)
    CALL message ( msg_fatal, "Input namelist file " // TRIM(ADJUSTL(file_in1)) // " not found" )
  ENDIF
  
  OPEN ( UNIT=funit, FILE=file_in1, STATUS="OLD", ACTION="READ" )

  READ ( UNIT=funit, NML=bgr_profile, iostat=iostatus)

  IF ( iostatus > 0 ) THEN
    CALL pass_fail(.false., test_name)
    CALL message ( msg_fatal, "I/O error while reading " // file_in1 )
  ENDIF

  CLOSE ( UNIT=funit )

  year1 = year  ;  mon1 = mon  ;  day1 = day  ;  hour1 = hour  ;  min1 = min  ;  sec1 = sec  ;  tlead1 = tlead
  lon1 = lon  ;  lat1 = lat  ;  und1 = und  ;  roc1 = roc  ;  coc1 = coc
  Z01 = Z0  ;  P01 = P0
  Nlevs1 = Nlevs
  P1 = P  ;  T1 = T  ;  Q1 = Q  ;  Z1 = Z  ;  Ak1 = Ak  ;  Bk1 = Bk
  

! 2. Read 2nd namelist
! --------------------

  INQUIRE ( FILE=file_in2, EXIST=exists )
  IF ( .NOT. exists ) THEN
    CALL pass_fail(.false., test_name)
    CALL message ( msg_fatal, "Input namelist file " // TRIM(ADJUSTL(file_in2)) // " not found" )
  ENDIF

  OPEN ( UNIT=funit, FILE=file_in2, STATUS="OLD", ACTION="READ" )

  READ ( UNIT=funit, NML=bgr_profile, iostat=iostatus)

  IF ( iostatus > 0 ) THEN
    CALL pass_fail(.false., test_name)
    CALL message ( msg_fatal, "I/O error while reading " // file_in2 )
  ENDIF

  CLOSE ( UNIT=funit )

  year2 = year  ;  mon2 = mon  ;  day2 = day  ;  hour2 = hour  ;  min2 = min  ;  sec2 = sec  ;  tlead2 = tlead
  lon2 = lon  ;  lat2 = lat  ;  und2 = und  ;  roc2 = roc  ;  coc2 = coc
  Z02 = Z0  ;  P02 = P0
  Nlevs2 = Nlevs
  P2 = P  ;  T2 = T  ;  Q2 = Q  ;  Z2 = Z  ;  Ak2 = Ak  ;  Bk2 = Bk


! 3. Compare the two
! ------------------

  idiff = 0  ! Initialise difference counter

  IF (ABS(year1 - year2) > 0) THEN
    CALL message ( msg_error, "year1 /= year2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ABS(mon1 - mon2) > 0) THEN
    CALL message ( msg_error, "mon1 /= mon2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ABS(day1 - day2) > 0) THEN
    CALL message ( msg_error, "day1 /= day2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ABS(hour1 - hour2) > 0) THEN
    CALL message ( msg_error, "hour1 /= hour2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ABS(min1 - min2) > 0) THEN
    CALL message ( msg_error, "min1 /= min2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ABS(sec1 - sec2) > 0) THEN
    CALL message ( msg_error, "sec1 /= sec2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ABS(tlead1 - tlead2) > 1.0e-6_wp) THEN
    CALL message ( msg_error, "tlead1 /= tlead2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ABS(lon1 - lon2) > 1.0e-6_wp) THEN
    CALL message ( msg_error, "lon1 /= lon2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ABS(lat1 - lat2) > 1.0e-6_wp) THEN
    CALL message ( msg_error, "lat1 /= lat2" )
    idiff = idiff + 1
  ENDIF
  
! Flag differing undulations as a warning, not an error,
! as it may simply be due to GEOPOT_COEF and GEOPOT_CORR
! not being set correctly.
  IF (ABS(und1 - und2) > 1.0e-6_wp) THEN
    CALL message ( msg_warn, "und1 /= und2" )
    CALL message ( msg_info, "Suggest checking that GEOPOT_COEF and GEOPOT_CORR " // &
                             "have been set correctly. (See grib2bgrasc man page.) \n")
    idiff = idiff + 0
  ENDIF
  
  IF (ABS(roc1 - roc2) > 1.0_wp) THEN
    CALL message ( msg_error, "roc1 /= roc2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ANY(ABS(coc1 - coc2) > 1.0e-3_wp)) THEN
    CALL message ( msg_error, "coc1 /= coc2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ABS(azi1 - azi2) > 1.0e-6_wp) THEN
    CALL message ( msg_error, "azi1 /= azi2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ABS(Nlevs1 - Nlevs2) > 0) THEN
    CALL message ( msg_error, "Nlevs1 /= Nlevs2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ABS(Z01 - Z02) > 1.0e-3_wp) THEN
    CALL message ( msg_error, "Z01 /= Z02" )
    idiff = idiff + 1
  ENDIF

  IF (ABS(P01 - P02) > 1.0e-3_wp) THEN
    CALL message ( msg_error, "P01 /= P02" )
    idiff = idiff + 1
  ENDIF
  
  IF (ANY(ABS(P1 - P2) > 1.0e-6_wp)) THEN
    CALL message ( msg_error, "P1 /= P2" )
    idiff = idiff + 1
  ENDIF

  IF (ANY(ABS(T1 - T2) > 1.0e-6_wp)) THEN
    CALL message ( msg_error, "T1 /= T2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ANY(ABS(Q1 - Q2) > 1.0e-6_wp)) THEN
    CALL message ( msg_error, "Q1 /= Q2" )
    idiff = idiff + 1
  ENDIF

  IF (ANY(ABS(Z1 - Z2) > 1.0e-6_wp)) THEN
    CALL message ( msg_error, "Z1 /= Z2" )
    idiff = idiff + 1
  ENDIF

  IF (ANY(ABS(Ak1 - Ak2) > 1.0e-6_wp)) THEN
    CALL message ( msg_error, "Ak1 /= Ak2" )
    idiff = idiff + 1
  ENDIF
  
  IF (ANY(ABS(Bk1 - Bk2) > 1.0e-6_wp)) THEN
    CALL message ( msg_error, "Bk1 /= Bk2" )
    idiff = idiff + 1
  ENDIF
  
  
! 4. Summarise the results
! ------------------------

  IF (idiff > 0) THEN
    WRITE (sdiff, "(i3)") idiff
    CALL message ( msg_info, sdiff // " elements of " // &
                             TRIM(ADJUSTL(file_in1)) // " and " // &
                             TRIM(ADJUSTL(file_in2)) // " differ significantly" )
                             
    CALL pass_fail(.false., test_name)
  ELSE
    CALL message ( msg_info, "No significant differences between " // &
                             TRIM(ADJUSTL(file_in1)) // " and " // &
                             TRIM(ADJUSTL(file_in2)) )
    CALL pass_fail(.true., test_name)
  ENDIF


CONTAINS


  SUBROUTINE pass_fail(pass, test_name)

    CHARACTER(LEN=*)   :: test_name
    LOGICAL            :: pass

    IF (pass) THEN
      PRINT*, ' '
      PRINT*, '******************************'
      PRINT*, '*** ' // TRIM(ADJUSTL(test_name)) // ' PASS' // ' ***'
      PRINT*, '******************************'
      PRINT*, ' '
    ELSE
      PRINT*, ' '
      PRINT*, '******************************'
      PRINT*, '*** ' // TRIM(ADJUSTL(test_name)) // ' FAIL' // ' ***'
      PRINT*, '******************************'
      PRINT*, ' '
    ENDIF

  END SUBROUTINE pass_fail


END PROGRAM nml_diff
