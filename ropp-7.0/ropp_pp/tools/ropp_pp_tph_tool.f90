! $Id: ropp_pp_tph_tool.f90 3491 2013-02-06 12:43:43Z idculv $

PROGRAM ropp_pp_tph_tool

!****p* Programs/ropp_pp_tph_tool *
!
! NAME
!   ropp_pp_tph_tool
!
! SYNOPSIS
!   Tropopause height (TPH) diagnostic
!
!   > ropp_pp_tph_tool [<options>] <infile(s)>
!
! ARGUMENTS
!   <infile(s)>   One (or more) input file names.
!
! OPTIONS
!   -o <output_file>  name of ROPP netCDF output file
!   -b                calculate bending angle-based TPH
!   -n                calculate refractivity-based TPH
!   -y                calculate dry-temperature-based TPH
!   -t                calculate temperature-based TPH
!   -h                help
!   -d                output additional diagnostics
!   -v                version information
!
! DESCRIPTION
!   Diagnose tropopause height from the kinks in one or more RO profiles, 
!   using the covariance transform method of Lewis (GRL 2009).
!
! NOTES
!   If the input file is a multifile, or more than one input files are
!   specified, the output file is a multifile.
!
!   Existing output files will be overwritten.
!
!   If none of the {-b, -n, -y, -t} options is specified, the tool will 
!   attempt to calculate all four TPHs.

! AUTHOR
!   Met Office, Exeter, UK.
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

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_utils
  USE ropp_io
  USE ropp_io_types, ONLY: ROprof
  USE ropp_pp

  IMPLICIT NONE

  TYPE(ROprof)                                     :: ro_data, ro_data1

! Local variables

  REAL(wp)                                         :: und

  INTEGER                                          :: idummy
  INTEGER                                          :: i, iargc, argc, k
  INTEGER                                          :: n_files, n_profiles

  LOGICAL                                          :: give_help
  LOGICAL                                          :: l_bangle, l_refrac, &
                                                      l_tdry, l_temp, l_all_tph
  LOGICAL                                          :: l_diag

  CHARACTER(len = 4096), DIMENSION(:), ALLOCATABLE :: ifiles
  CHARACTER(len = 4096)                            :: ofile
  CHARACTER(len =  256)                            :: buffer
  CHARACTER(len =    4)                            :: istr
  CHARACTER(len =    6)                            :: nstr
  CHARACTER(len =   10)                            :: tph_str
  CHARACTER(len =   10)                            :: tpt_str
  CHARACTER(len = 4096)                            :: flag_meaning

  CHARACTER(len =  256)                            :: routine

!-------------------------------------------------------------------------------
! 2. Default settings
!-------------------------------------------------------------------------------

  CALL message_set_routine('ropp_pp_tph_tool')

  give_help = .FALSE.

  l_bangle  = .FALSE.
  l_refrac  = .FALSE.
  l_tdry    = .FALSE.
  l_temp    = .FALSE.
  l_all_tph = .FALSE.

  l_diag    = .FALSE.

  ofile     = "ropp_pp_tph.nc"

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '----------------------------------------------------------------------')
  CALL message(msg_noin, &
       '             ROPP Pre-processor Tropopause Height Tool                ')
  CALL message(msg_noin, &
       '----------------------------------------------------------------------')
  CALL message(msg_noin, '')

!-------------------------------------------------------------------------------
! 3. Command line arguments
!-------------------------------------------------------------------------------

  argc = iargc()
  i = 1
  n_files = 0
  ALLOCATE(ifiles(argc))

  DO WHILE(i <= argc)
    CALL getarg(i, buffer)
    SELECT CASE (buffer)
      CASE('-o')                          ! Output file name (netCDF output)
         CALL getarg(i+1, buffer)
         ofile = buffer
         i = i + 1
      CASE('-b')                          ! Calculate bending angle-based TPHs
         l_bangle  = .TRUE.
      CASE('-n')                          ! Calculate refractivity-based TPHs
         l_refrac  = .TRUE.
      CASE('-y')                          ! Calculate dry temperature-based TPHs
         l_tdry    = .TRUE.
      CASE('-t')                          ! Calculate temperature-based TPHs
         l_temp    = .TRUE.
      CASE('-h', '--help', '?')           ! Give some help
         give_help = .TRUE.
      CASE('-d')                          ! Output more diagnostic information
         msg_MODE = VerboseMode
         l_diag = .TRUE.
      CASE('-v', '-V', '--version')       ! Output version info
         CALL version_info()
         CALL EXIT(0)
      CASE default                        ! Input file name
         IF ( buffer(1:1) /= '-' ) THEN
           n_files = n_files + 1
           ifiles(n_files) = buffer
         END IF
    END SELECT
    i = i + 1
  END DO

  IF (argc == 0 .OR. n_files == 0 .OR. give_help) THEN
    CALL usage()
    CALL EXIT(0)
  END IF

  IF (.NOT. (l_bangle .OR. l_refrac .OR. l_tdry .OR. l_temp)) l_all_tph = .TRUE.

!-------------------------------------------------------------------------------
! 4. Remove pre-existing output file
!-------------------------------------------------------------------------------

  CALL file_delete(ofile, idummy)

!-------------------------------------------------------------------------------
! 5. Loop over all input files
!-------------------------------------------------------------------------------

  DO k=1,n_files

    CALL message(msg_info, "Processing file " // TRIM(ADJUSTL(ifiles(k))))

!-------------------------------------------------------------------------------
! 6. Loop over all profiles
!-------------------------------------------------------------------------------

    n_profiles = ropp_io_nrec(ifiles(k))

    DO i=1,n_profiles

      WRITE(istr, '(i4)') i
      WRITE(nstr, '(i6)') n_profiles
      CALL message(msg_info, "Processing profile " // istr // " of " // nstr )

!-------------------------------------------------------------------------------
! 7. Read data
!-------------------------------------------------------------------------------

      CALL ropp_io_read(ro_data1,  ifiles(k), rec=i, ranchk=.TRUE.)
      CALL message(msg_info, "Occultation ID: " // TRIM(ro_data%occ_id) // " \n")

! 7.1 Extend ro_data to include a lev2c structure (to hold the TPHs) if necessary

      IF (ro_data1%lev2c%npoints == 0) THEN
        CALL ropp_io_init(ro_data, &
                          ro_data1%lev1a%npoints, &
                          ro_data1%lev1b%npoints, &
                          ro_data1%lev2a%npoints, &
                          ro_data1%lev2b%npoints, &
                                               1, &
                          ro_data1%lev2d%npoints)
      END IF

      ro_data1%lev2c%npoints = 1

      ro_data = ro_data1

      ro_data%lev2c%geop_sfc  = 0.0_wp     ! To stop the range-checking zapping the lev2c component
      ro_data%lev2c%press_sfc = 1000.0_wp  ! To stop the range-checking zapping the lev2c component

      ro_data%lev2d%level_type = ro_data1%lev2d%level_type  ! Not yet done in ropp_io_assign.f90 

      CALL ropp_io_free(ro_data1)

! 7.2 Ensure that profiles are increasing in height - 1st element towards surface

      CALL ropp_io_ascend(ro_data)

!-------------------------------------------------------------------------------
! 8. Calculate TPH based on bending angle, if possible
!-------------------------------------------------------------------------------

      IF (l_bangle .OR. l_all_tph) THEN

        ro_data%lev2c%tph_bangle      = ropp_MDFV
        ro_data%lev2c%tpa_bangle      = ropp_MDFV
        ro_data%lev2c%tph_bangle_flag = ropp_MIFV

        CALL ropp_pp_tph_bangle(ro_data, diag=l_diag)

! 8.1 Bending angle

        CALL tph_flag_decode(ro_data%lev2c%tph_bangle_flag, flag_meaning)
        CALL message(msg_diag, "Bending angle-based TPH QC flag = " // &
                                TRIM(ADJUSTL(flag_meaning)))

        IF (ro_data%lev2c%tph_bangle > ropp_MDTV) THEN

          IF (ro_data%GEOref%undulation < ropp_MDTV) THEN
            und = 0.0_wp
          ELSE
            und = ro_data%GEOref%undulation
          END IF

          WRITE (tph_str, '(f10.5)') &
            (ro_data%lev2c%tph_bangle - ro_data%GEOref%roc - und)*1.0e-3_wp
          WRITE (tpt_str, '(f10.5)') ro_data%lev2c%tpa_bangle*1.0e3_wp
          CALL message(msg_diag, "Bending angle-based TPH: " // &
                                  tpt_str // " mrad at " // tph_str // " km \n")

        END IF

      END IF

!-------------------------------------------------------------------------------
! 9. Calculate TPH based on refractivity, if possible
!-------------------------------------------------------------------------------

      IF (l_refrac .OR. l_all_tph) THEN

        ro_data%lev2c%tph_refrac      = ropp_MDFV
        ro_data%lev2c%tpn_refrac      = ropp_MDFV
        ro_data%lev2c%tph_refrac_flag = ropp_MIFV

        CALL ropp_pp_tph_refrac(ro_data, diag=l_diag)

! 9.1 Refractivity

        CALL tph_flag_decode(ro_data%lev2c%tph_refrac_flag, flag_meaning)
        CALL message(msg_diag, "Refractivity-based TPH QC flag = " // &
                                TRIM(ADJUSTL(flag_meaning)))

        IF (ro_data%lev2c%tph_refrac > ropp_MDTV) THEN

          WRITE (tph_str, '(f10.5)') ro_data%lev2c%tph_refrac*1.0e-3_wp
          WRITE (tpt_str, '(f10.5)') ro_data%lev2c%tpn_refrac
          CALL message(msg_diag, "Refractivity-based TPH: " // &
                                  tpt_str // " N-unit at " // tph_str // " km \n")

        END IF

      END IF

!-------------------------------------------------------------------------------
! 10. Calculate TPH based on dry temperature, if possible
!-------------------------------------------------------------------------------

      IF (l_tdry .OR. l_all_tph) THEN

        ro_data%lev2c%tph_tdry_lrt      = ropp_MDFV
        ro_data%lev2c%tpt_tdry_lrt      = ropp_MDFV
        ro_data%lev2c%tph_tdry_lrt_flag = ropp_MIFV

        ro_data%lev2c%tph_tdry_cpt      = ropp_MDFV
        ro_data%lev2c%tpt_tdry_cpt      = ropp_MDFV
        ro_data%lev2c%tph_tdry_cpt_flag = ropp_MIFV

        ro_data%lev2c%prh_tdry_cpt      = ropp_MDFV
        ro_data%lev2c%prt_tdry_cpt      = ropp_MDFV
        ro_data%lev2c%prh_tdry_cpt_flag = ropp_MIFV

        CALL ropp_pp_tph_tdry(ro_data, diag=l_diag)

! 10.1 Lapse rate

        CALL tph_flag_decode(ro_data%lev2c%tph_tdry_lrt_flag, flag_meaning)
        CALL message(msg_diag, "Dry temperature lapse rate-based TPH QC flag = " // &
                                TRIM(ADJUSTL(flag_meaning)))

        IF (ro_data%lev2c%tph_tdry_lrt > ropp_MDTV) THEN

          WRITE (tph_str, '(f10.5)') ro_data%lev2c%tph_tdry_lrt*1.0e-3_wp
          WRITE (tpt_str, '(f10.5)') ro_data%lev2c%tpt_tdry_lrt
          CALL message(msg_diag, "Dry temperature lapse rate-based TPH: " // &
                                  tpt_str // " K at " // tph_str // " km \n")

        END IF

! 10.2 Cold point

        CALL tph_flag_decode(ro_data%lev2c%tph_tdry_cpt_flag, flag_meaning)
        CALL message(msg_diag, "Dry temperature cold point-based TPH QC flag = " // &
                                TRIM(ADJUSTL(flag_meaning)))

        IF (ro_data%lev2c%tph_tdry_cpt > ropp_MDTV) THEN

          WRITE (tph_str, '(f10.5)') ro_data%lev2c%tph_tdry_cpt*1.0e-3_wp
          WRITE (tpt_str, '(f10.5)') ro_data%lev2c%tpt_tdry_cpt
          CALL message(msg_diag, "Dry temperature cold point-based TPH: " // &
                                  tpt_str // " K at " // tph_str // " km \n")

        END IF

! 10.3 Profile Tmin

        CALL tph_flag_decode(ro_data%lev2c%prh_tdry_cpt_flag, flag_meaning)
        CALL message(msg_diag, "Entire profile dry temperature cold point QC flag = " // &
                                TRIM(ADJUSTL(flag_meaning)))

        IF (ro_data%lev2c%prh_tdry_cpt > ropp_MDTV) THEN

          WRITE (tph_str, '(f10.5)') ro_data%lev2c%prh_tdry_cpt*1.0e-3_wp
          WRITE (tpt_str, '(f10.5)') ro_data%lev2c%prt_tdry_cpt
          CALL message(msg_diag, "Cold point of entire dry temperature profile: " // &
                                  tpt_str // " K at " // tph_str // " km \n")

        END IF

      END IF

!-------------------------------------------------------------------------------
! 11. Calculate TPH based on temperature, if possible
!-------------------------------------------------------------------------------

      IF (l_temp .OR. l_all_tph) THEN

        ro_data%lev2c%tph_temp_lrt      = ropp_MDFV
        ro_data%lev2c%tpt_temp_lrt      = ropp_MDFV
        ro_data%lev2c%tph_temp_lrt_flag = ropp_MIFV

        ro_data%lev2c%tph_temp_cpt      = ropp_MDFV
        ro_data%lev2c%tpt_temp_cpt      = ropp_MDFV
        ro_data%lev2c%tph_temp_cpt_flag = ropp_MIFV

        ro_data%lev2c%prh_temp_cpt      = ropp_MDFV
        ro_data%lev2c%prt_temp_cpt      = ropp_MDFV
        ro_data%lev2c%prh_temp_cpt_flag = ropp_MIFV

        CALL ropp_pp_tph_temp(ro_data, diag=l_diag)


! 11.1 Lapse rate

        CALL tph_flag_decode(ro_data%lev2c%tph_temp_lrt_flag, flag_meaning)
        CALL message(msg_diag, "Temperature lapse rate-based TPH QC flag = " // &
                                TRIM(ADJUSTL(flag_meaning)))

        IF (ro_data%lev2c%tph_temp_lrt > ropp_MDTV) THEN

          WRITE (tph_str, '(f10.5)') ro_data%lev2c%tph_temp_lrt*1.0e-3_wp
          WRITE (tpt_str, '(f10.5)') ro_data%lev2c%tpt_temp_lrt
          CALL message(msg_diag, "Temperature lapse rate-based TPH: " // &
                                  tpt_str // " K at " // tph_str // " km \n")

        END IF


! 11.2 Cold point

        CALL tph_flag_decode(ro_data%lev2c%tph_temp_cpt_flag, flag_meaning)
        CALL message(msg_diag, "Temperature cold point-based TPH QC flag = " // &
                                TRIM(ADJUSTL(flag_meaning)))

        IF (ro_data%lev2c%tph_temp_cpt > ropp_MDTV) THEN

          WRITE (tph_str, '(f10.5)') ro_data%lev2c%tph_temp_cpt*1.0e-3_wp
          WRITE (tpt_str, '(f10.5)') ro_data%lev2c%tpt_temp_cpt
          CALL message(msg_diag, "Temperature cold point-based TPH: " // &
                                  tpt_str // " K at " // tph_str // " km \n")

        END IF


! 11.3 Profile Tmin

        CALL tph_flag_decode(ro_data%lev2c%prh_temp_cpt_flag, flag_meaning)
        CALL message(msg_diag, "Entire profile temperature cold point QC flag = " // &
                                TRIM(ADJUSTL(flag_meaning)))

        IF (ro_data%lev2c%prh_temp_cpt > ropp_MDTV) THEN

          WRITE (tph_str, '(f10.5)') ro_data%lev2c%prh_temp_cpt*1.0e-3_wp
          WRITE (tpt_str, '(f10.5)') ro_data%lev2c%prt_temp_cpt
          CALL message(msg_diag, "Cold point of entire temperature profile: " // &
                                 tpt_str // " K at " // tph_str // " km \n")

        END IF

      END IF

!-------------------------------------------------------------------------------
! 12. Write data
!-------------------------------------------------------------------------------

      CALL ropp_io_write(ro_data, ofile, append=.TRUE., ranchk=.TRUE.)

!-------------------------------------------------------------------------------
! 13. Clean up
!-------------------------------------------------------------------------------

      CALL ropp_io_free(ro_data)

    END DO  ! loop over profiles

  END DO  ! loop over files

CONTAINS

!-------------------------------------------------------------------------------
! 15. TPH QC flag decoding
!-------------------------------------------------------------------------------

  SUBROUTINE tph_flag_decode(tph_flag, meaning)

    INTEGER                                        :: tph_flag
    CHARACTER (LEN=4)                              :: flag_str
    CHARACTER (LEN=4096)                           :: meaning

    WRITE (flag_str, '(i4)') tph_flag

    meaning = flag_str // " ==> "

    IF (tph_flag < ropp_MITV) THEN
      meaning = TRIM(meaning) // " Initial value or incalculable ... \n"
      RETURN
    END IF

    IF (tph_flag == 0) THEN
      meaning = TRIM(meaning) // " Diagnosed value OK ..."
      RETURN
    END IF

    IF (BTEST(tph_flag, TPH_QC_data_invalid)) &
      meaning = TRIM(meaning) // " Invalid profile data ... \n"
    IF (BTEST(tph_flag, TPH_QC_prof_depth)) &
      meaning = TRIM(meaning) // " Profile not deep enough ... \n"
    IF (BTEST(tph_flag, TPH_QC_prof_height)) &
      meaning = TRIM(meaning) // " Profile not high enough ... \n"
    IF (BTEST(tph_flag, TPH_QC_CT_smooth_above)) &
      meaning = TRIM(meaning) // " Cov transform too smooth above TPH ..."
    IF (BTEST(tph_flag, TPH_QC_CT_smooth_below)) &
      meaning = TRIM(meaning) // " Cov transform too smooth below TPH ..."
    IF (BTEST(tph_flag, TPH_QC_double_trop)) &
      meaning = TRIM(meaning) // " Double tropopause detected ..."
    IF (BTEST(tph_flag, TPH_QC_too_low)) &
      meaning = TRIM(meaning) // " Diagnosed TPH too low ..."
    IF (BTEST(tph_flag, TPH_QC_too_high)) &
      meaning = TRIM(meaning) // " Diagnosed TPH too high ..."

  END SUBROUTINE tph_flag_decode

!-------------------------------------------------------------------------------
! 16. Usage information
!-------------------------------------------------------------------------------

  SUBROUTINE usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Tropopause height diagnostic'
    PRINT *, 'Usage:'
    PRINT *, '  > ropp_pp_tph_tool [<options>] <input_file(s)>'
    PRINT *, 'Options:'
    PRINT *, '  -o <output_file>  name of ROPP netCDF output file'
    PRINT *, '  -b                calculate bending angle-based TPH'
    PRINT *, '  -n                calculate refractivity-based TPH'
    PRINT *, '  -y                calculate dry temperature-based TPH'
    PRINT *, '  -t                calculate temperature-based TPH'
    PRINT *, '  -h                this help'
    PRINT *, '  -d                output additional diagnostics'
    PRINT *, '  -v                version information'
    PRINT *, ''
  END SUBROUTINE usage

!-------------------------------------------------------------------------------
! 17. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    CHARACTER (LEN=40) :: version
    version = ropp_pp_version()
    PRINT *, 'ropp_pp_tph_tool - Tropopause height diagnostic.'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (PP) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END PROGRAM ropp_pp_tph_tool
