! $Id: ucar2ropp.f90 3551 2013-02-25 09:51:28Z idculv $

PROGRAM ucar2ropp

!****x* Programs/ucar2ropp *
!
! NAME
!   ucar2ropp
!
! SYNOPSIS
!   Converts a UCAR 'atmPrf', 'atmPhs', 'sonPrf', 'ecmPrf', 'ncpPrf', or
!  'gfsPrf' netCDF file of COSMIC, CHAMP, C/NOFS or SAC-C data into an
!   ROPP netCDF file with optional thinning
!
!   > ucar2ropp ip_file [ip_file...] [-o op_file] [-p lev_file]
!                       [-m] [-a] [-u] [-i] [-d] [-h] [-v]
!
! ARGUMENTS
!   ip_file - one or more UCAR netCDF files of type 'atmPrf', 'atmPhs',
!             'sonPrf', 'ecmPrf', 'ncpPrf' or 'gfsPrf'.
!             There is no default for this argument.
!
! OPTIONS
!   -o specifies an output file name - mandatory argument if used
!     (default: name generated from the file header)
!   -p specifies a sampling levels control file for profile thinning
!      or a positive integer representing the maximum no. of levels
!      for implied SAMPLE mode (default: no thinning)
!   -m causes the output into a multifile (i.e., a single ROPP netCDF
!      file holding multiple profiles).
!   -a appends to an already existing file (default is to
!      overwrite an already existing file) (-a implies -m)
!   -u leaves profiles untouched (do not sort or thin input data).
!      -p option is ignored if -u present. (Default: sort profiles
!      into ascending order, thin according to -p option)
!   -i thins on impact altitudes (IP - RoC - undulation)
!   -d  writes additional diagnostic information to stdout
!   -h (or ?) causes only summary help to be output
!   -v outputs program version ID
!
! CALLS
!   ropp_io_nrec
!   ropp_io_read
!   ropp_io_write
!   ropp_io_occid
!   ropp_io_ascend
!   ropp_io_thin
!   ropp_io_rangecheck
!   ropp_io_version
!   FileDelete
!   message
!   message_set_routine
!
! DESCRIPTION
!   Converts a UCAR 'atmPrf', 'atmPhs', 'sonPrf', 'ecmPrf', 'ncpPrf', or
!  'gfsPrf' netCDF file of COSMIC, CHAMP, C/NOFS or SAC-C data into an
!   ROPP netCDF file.
!   Only single input profiles can be processed currently but multifiles
!   can be written. Also allows thinning of data. Descending profiles
!   are made ascending by default.
!
! SEE ALSO
!    ucar2ropp(1)
!
! NOTES
!   This is a trivial sample program showing a simple ROPP file I/O
!   interface for illustrative purposes only; it is not intended to be
!   a comprehensive or robust tool.
!
! REFERENCES
!   1. ROPP User Guide - Part I
!      SAF/ROM/METO/UG/ROPP/002
!   2. ROPP Thinner Algorithm.
!      SAF/GRAS/METO/REP/GSR/008
!
! AUTHOR
!   Met Office, Exeter, UK.
!   Please email any comments on this software to: romsaf@metoffice.gov.uk
!
! COPYRIGHT
!   (c) EUMETSAT. All rights reserved.
!   For further details please refer to the file COPYRIGHT
!   which you should have received as part of this distribution.
!
!****

! Modules

  USE messages
  USE ropp_io_types, ONLY: ROprof
  USE ropp_io,       ONLY: ropp_io_nrec,    &
                           ropp_io_read,    &
                           ropp_io_write,   &
                           ropp_io_occid,   &
                           ropp_io_ascend,  &
                           ropp_io_thin,    &
                           ropp_io_version
  USE ropp_utils,    ONLY: File_Delete

  IMPLICIT NONE

! Fixed parameters

  CHARACTER (LEN=*), PARAMETER :: DTfmt1 = & ! hh:mm dd-mm-yyyy
                     "(I2.2,':',I2.2,'UT ',I2.2,'-',A3,'-',I4.4)"

! Local variables

  TYPE(ROprof) :: ROdata

  CHARACTER (LEN=256), ALLOCATABLE :: ipfile(:)
  CHARACTER (LEN=256) :: opfile, thfile, arg
  CHARACTER (LEN=20)  :: ocentre = "UCAR"
  CHARACTER (LEN=80)  :: outstr                       ! Formatted output string
  CHARACTER (LEN=10)  :: number                       ! Number as string

  INTEGER :: i, ierr, narg, iarg, k, istatus
  INTEGER :: nipf=0, nprf=0, nptot=0

  LOGICAL :: multi     = .false.
  LOGICAL :: newfile   = .true.
  LOGICAL :: first     = .true.
  LOGICAL :: exists    = .false.
  LOGICAL :: untouched = .false.
  LOGICAL :: impactalt = .FALSE.

! Some compilers may need the following declaration to be commented out
  INTEGER :: IARGC

!-------------------------------------------------------------
! 1. Initialise
!-------------------------------------------------------------

  CALL message_set_routine ( "ucar2ropp" )

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, &
       '                  UCAR to ROPP netCDF Converter'                      )
  CALL message(msg_noin, &
       '---------------------------------------------------------------------')
  CALL message(msg_noin, '')

!-------------------------------------------------------------
! 2. Parse command line options
!-------------------------------------------------------------

  narg = IARGC()
  ALLOCATE ( ipfile(narg), STAT=istatus )
  IF ( istatus /= 0 ) THEN
    CALL message ( msg_fatal, "Failed to allocate memory for ipfile array" )
  END IF
  nipf = 0

  ipfile = " "
  opfile = " "
  thfile = "0"

  iarg = 1
  DO WHILE ( iarg <= narg )
    CALL GETARG ( iarg, arg )

    SELECT CASE (arg)
      CASE ("-a","-A")
        newfile = .false.
        multi   = .true.

      CASE ("-d","-D")
        msg_MODE = VerboseMode

      CASE ("-h","-H","--help","?")
        narg = -1

      CASE ("-i")
        impactalt = .TRUE.

      CASE ("-m","-M")
        multi = .true.

      CASE ("-u","-U")
        untouched = .true.

      CASE ("-o","-O")
        CALL GETARG ( iarg+1, arg )
        opfile = arg
        iarg   = iarg + 1

      CASE ("-p","-P")
        CALL GETARG ( iarg+1, arg )
        thfile = arg
        iarg   = iarg + 1

      CASE ("-v","-V","--version")
        CALL version_info()
        CALL EXIT(0)

      CASE DEFAULT
         IF ( arg(1:1) /= "-" ) THEN
           nipf         = nipf + 1
           ipfile(nipf) = arg
         END IF
    END SELECT

    iarg = iarg + 1
  END DO

  IF ( nipf == 0 .AND. narg /= -1 ) THEN
    CALL message ( msg_warn, "No input file(s) specified" )
    narg = 0
  END IF

  IF ( narg <= 0 ) THEN
    CALL Usage
    CALL EXIT(0)
  END IF

!-------------------------------------------------------------
! 3. Loop over all input files
!-------------------------------------------------------------

  DO k = 1, nipf

    INQUIRE ( FILE=ipfile(k), EXIST=exists )
    IF ( .NOT. exists ) THEN
      CALL message ( msg_warn, "UCAR input file "//TRIM(ipfile(k))// &
                               " not found" )
      CYCLE
    ENDIF

!    3.1 UCAR files should only have one record!
!    ------------------------------------------------

    CALL message ( msg_info, "Reading file "//TRIM(ipfile(k)) )

!    3.2 Write singlefiles (Loop over all profiles)
!    ----------------------------------------------

    CALL ropp_io_read ( ROdata,         &
                        file=ipfile(k), &
                        rec=i,          &
                        centre=ocentre, &
                        ierr=ierr )
    IF ( ierr /= 0 ) THEN
      WRITE ( number, FMT="(I6)" ) ierr
      CALL message ( msg_fatal, "Error - code "//ADJUSTL(number) )
    END IF

!   3.2.1 Ensure profile is in ascending order and thin (optional)
!   ---------------------------------------------------------------

    IF ( .NOT. untouched ) THEN
      CALL message ( msg_diag, "Ensuring all profiles are in "// &
                               "ascending height order." )
      CALL ropp_io_ascend ( ROdata )
      CALL ropp_io_thin   ( ROdata, thfile, impactalt )
    END IF

!   3.2.2 Check/create output file name; explcitly delete any
!         existing potential multifile file unless appending
!   -------------------------------------------------------------

    CALL ropp_io_occid ( ROdata )
    IF ( opfile == " " ) THEN
      opfile = TRIM(ROdata%occ_id) // '.nc'
      CALL To_Lower(opfile)
      first = .TRUE.
    END IF

    IF ( first ) THEN
      INQUIRE ( FILE=opfile, EXIST=exists )
      IF ( exists ) THEN
        IF ( newfile .AND. multi ) THEN
          CALL file_delete ( opfile, ierr )
        END IF
      ELSE
        newfile = .TRUE.
      END IF
      first = .FALSE.
    END IF

!   3.2.3 Write output file
!   ------------------------

    WRITE ( outstr, FMT="(A,I5,A)" ) "Profile", k, " : "//ROdata%occ_id
    CALL message ( msg_info, TRIM(outstr) )
    WRITE ( outstr, FMT="(F6.2,',',F7.2)" ) ROdata%GeoRef%Lat, ROdata%GeoRef%Lon
    CALL message ( msg_diag, "   Latitude/Longitude           : "//TRIM(outstr) )
    WRITE ( number, FMT="(I6)" ) ROdata%Lev1a%Npoints
    CALL message ( msg_diag, "   No. of phase/SNR samples     : "//TRIM(number) )
    WRITE ( number, FMT="(I6)" ) ROdata%Lev1b%Npoints
    CALL message ( msg_diag, "   No. of bending angle samples : "//TRIM(number) )
    WRITE ( number, FMT="(I6)" ) ROdata%Lev2a%Npoints
    CALL message ( msg_diag, "   No. of refractivity samples  : "//TRIM(number) )
    WRITE ( number, FMT="(I6)" ) ROdata%Lev2b%Npoints
    CALL message ( msg_diag, "   No. of geophysical samples   : "//TRIM(number) )
    WRITE ( number, FMT="(I6)" ) ROdata%Lev2c%Npoints
    CALL message ( msg_diag, "   No. of surface geo. samples  : "//TRIM(number) )
    WRITE ( number, FMT="(I6)" ) ROdata%Lev2d%Npoints
    CALL message ( msg_diag, "   No. of model coeff. levels   : "//TRIM(number) )

    CALL message ( msg_info, "Writing "//TRIM(opfile) )
    CALL ropp_io_write ( ROdata, file=opfile, append=multi )

    nptot = nptot + 1

    IF ( .NOT. multi ) opfile = " "

 END DO            ! end file loop

!--------------------------------------------------------------
! 4. Show summary of outcome
!--------------------------------------------------------------

 IF ( nptot > 1 ) THEN
   WRITE ( number, FMT="(I5)" ) nptot
   CALL message ( msg_info, number//" profiles processed" )

    IF ( .NOT. newfile ) THEN
       nprf = ropp_io_nrec ( opfile )
       WRITE ( number, FMT="(I5)" ) nprf
       CALL message ( msg_info, number//" profiles now in "//TRIM(opfile) )
    END IF
 END IF

!--------------------------------------------------------------
! 5. Tidy up
!--------------------------------------------------------------

  IF ( ALLOCATED(ipfile) ) DEALLOCATE ( ipfile )
  CALL message ( msg_noin, " " )

CONTAINS

!-------------------------------------------------------------------------------
! 6. Usage (help) information
!-------------------------------------------------------------------------------

  SUBROUTINE Usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Convert/thin a UCAR netCDF file to a ROPP netCDF file'
    PRINT *, 'Usage:'
    PRINT *, '  > ucar2ropp ip_file [ip_file...] [-o op_file] [-p thin_file|maxsamp]'
    PRINT *, '                      [-m] [-a] [-u] [-i] [-d] [-h] [-v]'
    PRINT *, '   where ip_file(s) are UCAR netCDF files'
    PRINT *, 'Options:'
    PRINT *, '  -o specifies an output file name'
    PRINT *, '  -p specifies a thinning control file name or max. no. samples'
    PRINT *, '  -m forces the output into a netCDF multifile (i.e. a single data file'
    PRINT *, '     holding multiple profiles).'
    PRINT *, '  -a appends to an already existing file. -a implies -m'
    PRINT *, '  -u leave profiles untouched (do not sort or thin input data)'
    PRINT *, '  -i thins on impact altitudes (IP - RoC - undulation)'
    PRINT *, '  -d prints out some additional diagnostics to stdout'
    PRINT *, '  -h this help'
    PRINT *, '  -v version information'
    PRINT *, 'Defaults:'
    PRINT *, '  Input  file name : required'
    PRINT *, '  Output file name : from (first) occultation ID'
    PRINT *, '  Output mode      : one output file per input file'
    PRINT *, '  Append mode      : exising file is overwritten'
    PRINT *, '  Sorting          : descending profiles made ascending'
    PRINT *, '  Thinning         : none'
    PRINT *, '  Thining on alts  : off'
    PRINT *, 'See ucar2ropp(1) for details.'
    PRINT *, ''
  END SUBROUTINE Usage

!-------------------------------------------------------------------------------
! 7. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    CHARACTER (LEN=40) :: version
    version = ropp_io_version()
    PRINT *, 'ucar2ropp - convert UCAR netCDF files to ROPP netCDF'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (IO) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END PROGRAM ucar2ropp
