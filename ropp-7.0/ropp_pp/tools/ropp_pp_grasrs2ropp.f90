! $Id: ropp_pp_grasrs2ropp.f90 2021 2009-01-16 10:49:04Z frhl $

PROGRAM ropp_pp_grasrs2ropp

!****p* Programs/ropp_pp_grasrs2ropp *
!
! NAME
!   ropp_pp_grasrs2ropp
!
! SYNOPSIS
!   Program to convert EUMETSAT GRAS Raw Sampling data netcdf file to standard
!   ROPP format netCDF structure, suitable for processing with ropp_pp_occ_tool.
!
!   > ropp_pp_grasrs2ropp [<options>] <infile(s)>
!
!
! ARGUMENTS
!   <infile(s)>   One (or more) input file names.
!
! OPTIONS
!   -o <output_file>  name of ROPP netCDF output file
!                     (default: ropp_pp_gras.nc)
!   -d                output additional diagnostics
!   -h                help
!   -v                version information
!
! DESCRIPTION
!   This program reads EUMETSAT format netCDF files containing closed loop and
!   raw sampling data from GRAS. Data are translated into the standard ROPP
!   data format structures and written to a ROPP formatted output file.
!
! NOTES
!   If the input file is a multifile, or more than one input files are
!   specified, the output file is a multifile.
!
!   Already existing output files will be overwritten.
!
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
  USE ropp_utils, ONLY: ropp_MDFV
  USE messages
  USE DateTimeTypes
  USE DateTimeProgs
  USE ncdf
  USE ropp_io
  USE ropp_io_types, ONLY: ROprof, PCD_rising, PCD_occultation, PCD_open_loop
  USE ropp_pp_utils, ONLY: ropp_pp_isnan
  USE ropp_pp

  IMPLICIT NONE

  TYPE(ROprof)                 :: ro_data      ! Output RO data structure

  INTEGER                      :: readint
  REAL(wp)                     :: readreal
  CHARACTER (len = 256)        :: readstr

  INTEGER,  DIMENSION(:),   ALLOCATABLE :: LCF       ! Lost carrier flag
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: cl_dtime  ! CL relative time [sec since RO start]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_dtime  ! RS relative time [sec since RO start]
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: cl_r_gns  ! CL GNSS coordinates [m]
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: cl_v_gns  ! CL GNSS velocities [m/s]
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: cl_r_leo  ! CL Rx coordinates [m]
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: cl_v_leo  ! CL Rx velocities [m/s]
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: rs_r_gns  ! RS GNSS coordinates [m]
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: rs_v_gns  ! RS GNSS velocities [m/s]
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: rs_r_leo  ! RS Rx coordinates [m]
  REAL(wp), DIMENSION(:,:), ALLOCATABLE :: rs_v_leo  ! RS Rx velocities [m/s]

  REAL(wp), DIMENSION(:),   ALLOCATABLE :: cl_exphase_ca ! CL excess phase CA [m]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: cl_exphase_p1 ! CL excess phase P1 [m]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: cl_exphase_p2 ! CL excess phase P2 [m]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: cl_snr_ca     ! CL SNR CA [V/V]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: cl_snr_p1     ! CL SNR P1 [V/V]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: cl_snr_p2     ! CL SNR P2 [V/V]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: cl_i_p2       ! CL I-component P2 [V]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: cl_q_p2       ! CL Q-component P2 [V]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: cl_exphase_l2_nco ! CL L2 NCO ex ph [m]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: cl_phase_l2_iq ! CL L2 phase from Q/I [rad]
  INTEGER,  DIMENSION(:),   ALLOCATABLE :: cl_lcf         ! CL lost carrier flag

  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_navbit_ext    ! RS navbits (external)
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_navbit_int    ! RS navbits (internal)
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_i_ca_uncorr   ! RS I component [V]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_q_ca_uncorr   ! RS Q component [V]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_snr_ca        ! RS SNR [V/V]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_exphase_l1_nco ! RS NCO excess phase [m]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_exphase_l1    ! RS excess phase [m]
  REAL(wp), DIMENSION(:),   ALLOCATABLE :: rs_phase_l1_iq ! RS phase addition from Q/I [rad]
  INTEGER,  DIMENSION(:),   ALLOCATABLE :: rs_lcf         ! RS lost carrier flag
  INTEGER,  DIMENSION(8)                :: DT8            ! Date/time array

  REAL(wp)           :: ts, ts1
  INTEGER            :: i, j, k, idummy, iargc, argc
  INTEGER            :: n_profiles, n_files, n_cl, n_rs, n_xyz
  INTEGER            :: n
  LOGICAL            :: give_help = .FALSE.
  LOGICAL            :: ranchk    = .TRUE.
  INTEGER            :: have_cl = 0
  INTEGER            :: have_rs = 0
  INTEGER            :: have_nb = 0

  CHARACTER(len = 4096), DIMENSION(:), ALLOCATABLE :: ifiles
  CHARACTER(len = 4096)                            :: ofile
  CHARACTER(len =  256)                            :: buffer
  CHARACTER(len =    4)                            :: istr
  CHARACTER(len =   10)                            :: nstr

!-------------------------------------------------------------------------------
! 2. Default settings
!-------------------------------------------------------------------------------

  give_help = .FALSE.
  ofile     = "ropp_pp_gras.nc"

  CALL message(msg_noin, '')
  CALL message(msg_noin, &
       '----------------------------------------------------------------------')
  CALL message(msg_noin, &
       '           ROPP GRAS RS-to-ROPP file conversion tool')
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
        CASE('-d')                          ! 'Diagnostic' output mode
          msg_MODE = VerboseMode
        CASE ('-no-ranchk')                 ! Use no rangecheck on output
           ranchk = .FALSE.
        CASE('-h', '--help', '?')           ! Give some help
           give_help = .TRUE.
        CASE('-v', '-V', '--version')       ! Give some version information
           CALL version_info()
           CALL EXIT(0)
        CASE default                        ! Input file name
           IF ( buffer(1:1) /= "-" ) THEN
             n_files = n_files + 1
             ifiles(n_files) = buffer
           END IF
    END SELECT
    i = i + 1
  END DO

  IF (argc == 0 .OR. n_files == 0 .OR. give_help) THEN
    CALL usage()
    CALL EXIT(0)
  ENDIF

!-------------------------------------------------------------------------------
! 4. Remove pre-existing output file
!-------------------------------------------------------------------------------

  CALL file_delete(ofile, idummy)

!-------------------------------------------------------------------------------
! 5. Loop over all input files and profiles
!-------------------------------------------------------------------------------

  DO k = 1, n_files

    n_profiles = MAX(ropp_io_nrec(ifiles(k)),1)

    DO i = 1, n_profiles

        WRITE(istr, '(i4)') i
        WRITE(nstr, '(i6)') n_profiles
        CALL message(msg_info, "Processing profile " // istr // " of " // nstr )

!-------------------------------------------------------------------------------
! 6. Read data direct from file
!-------------------------------------------------------------------------------

        IF (is_netcdf(ifiles(k))) THEN

          CALL ncdf_open(ifiles(k))

          ! 6.1 ID information / attributes

          IF (ncdf_isatt('title')) THEN

            CALL ncdf_getatt('title', buffer)

            IF (buffer(1:7) == 'MetOp-A') THEN
              ro_data%leo_id = 'META'
            ELSE IF (buffer(1:7) == 'MetOp-B') THEN
              ro_data%leo_id = 'METB'
            ELSE
              ro_data%leo_id = 'META'  ! In the absence of a more sensible default
            END IF

          ELSE

            ro_data%leo_id = 'META'  ! In the absence of a more sensible default

          END IF

          CALL ncdf_getvar('prn', readint)
          WRITE(ro_data%gns_id,'(A1,I3.3)') 'G', readint

          CALL ncdf_getatt('occ_type', readstr)
          IF (TRIM(readstr) == 'rising') THEN
            ro_data%PCD = IBSET(ro_data%PCD, PCD_rising)
          ELSE
            ro_data%PCD = IBCLR(ro_data%PCD, PCD_rising)
          ENDIF

          CALL ncdf_getatt('institution', ro_data%processing_centre)
          CALL ncdf_getatt('pod_method', ro_data%pod_method)
          CALL ncdf_getatt('phase_method', ro_data%phase_method)
          CALL ncdf_getatt('software_version', ro_data%software_version)
          CALL ncdf_getatt('processing_time', readstr)
          READ(readstr, "(i4.4,5(1x,i2.2),1x,i3.3)") &
                    ro_data%DTpro%Year,   &
                    ro_data%DTpro%Month,  &
                    ro_data%DTpro%Day,    &
                    ro_data%DTpro%Hour,   &
                    ro_data%DTpro%Minute, &
                    ro_data%DTpro%Second, &
                    ro_data%DTpro%Msec

          ! 6.2 Time information

          CALL ncdf_getvar('start_time_absdate', readint)
          readreal = REAL(readint, wp) - 0.5_wp   ! convert to real
          CALL TimeSince(DT8, readreal, -1,     &
             base="Days since 0001-01-01 00:00:00.00")
          ro_data%DTocc%Year  = DT8(1)
          ro_data%DTocc%Month = DT8(2)
          ro_data%DTocc%Day   = DT8(3)

          CALL ncdf_getvar('start_time_abstime', readreal)
          ro_data%DTocc%Hour = INT(readreal/(60*60))
          readreal = readreal - ro_data%DTocc%Hour*60*60
          ro_data%DTocc%Minute = INT(readreal/60)
          ro_data%DTocc%Second = readreal - ro_data%DTocc%Minute*60
          ro_data%DTocc%Msec = (readreal - ro_data%DTocc%Minute*60 -   &
             ro_data%DTocc%Second)*1000.

          ro_data%PCD = IBSET(ro_data%PCD, PCD_occultation)

          CALL ropp_io_occid(ro_data)
          CALL message(msg_diag, "OccID: " // ro_data%occ_id)

          ! 6.3 Georeferencing information

          CALL ncdf_getvar('longitude', ro_data%GEOref%lon)
          CALL ncdf_getvar('latitude', ro_data%GEOref%lat)
          CALL ncdf_getvar('azimuth', ro_data%GEOref%azimuth)

!-------------------------------------------------------------------------------
! 7. Closed loop data
!-------------------------------------------------------------------------------

          CALL ncdf_getvar('have_cl_data', have_cl)
          IF ( have_cl > 0 ) THEN
            CALL ncdf_getsize('cl_dtime', n_cl)
            WRITE(nstr, '(i6)') n_cl
            CALL message(msg_diag,"Reading "//nstr//"closed loop data points")

            ! 7.1 Time

            ALLOCATE(cl_dtime(n_cl))
            CALL ncdf_getvar('cl_dtime', cl_dtime)

            ! 7.2 Position and velocity

            CALL ncdf_getsize('cl_r_gns', n_xyz, dim=1)
            ALLOCATE(cl_r_gns(n_xyz, n_cl))
            CALL ncdf_getvar('cl_r_gns', cl_r_gns)
            ALLOCATE(cl_v_gns(n_xyz, n_cl))
            CALL ncdf_getvar('cl_v_gns', cl_v_gns)
            ALLOCATE(cl_r_leo(n_xyz, n_cl))
            CALL ncdf_getvar('cl_r_rec', cl_r_leo)
            ALLOCATE(cl_v_leo(n_xyz, n_cl))
            CALL ncdf_getvar('cl_v_rec', cl_v_leo)

            ! 7.3 Phase and amplitude

            ALLOCATE(cl_exphase_ca(n_cl))
            CALL ncdf_getvar('cl_exphase_ca', cl_exphase_ca)
            ALLOCATE(cl_exphase_p1(n_cl))
            CALL ncdf_getvar('cl_exphase_p1', cl_exphase_p1)
            ALLOCATE(cl_exphase_p2(n_cl))
            CALL ncdf_getvar('cl_exphase_p2', cl_exphase_p2)
            ALLOCATE(cl_snr_ca(n_cl))
            CALL ncdf_getvar('cl_snr_ca', cl_snr_ca)
            ALLOCATE(cl_snr_p1(n_cl))
            CALL ncdf_getvar('cl_snr_p1', cl_snr_p1)
            ALLOCATE(cl_snr_p2(n_cl))
            CALL ncdf_getvar('cl_snr_p2', cl_snr_p2)
            ALLOCATE(cl_i_p2(n_cl))
            CALL ncdf_getvar('cl_i_p2', cl_i_p2)
            ALLOCATE(cl_q_p2(n_cl))
            CALL ncdf_getvar('cl_q_p2', cl_q_p2)
            ALLOCATE(cl_exphase_l2_nco(n_cl))
            CALL ncdf_getvar('cl_exphase_l2_nco', cl_exphase_l2_nco)

            DO j=1,n_cl
              IF (ropp_pp_isnan(cl_i_p2(j))) THEN
                cl_i_p2(j) = 0.0_wp     ! Check for NaN
              ENDIF
              IF (ropp_pp_isnan(cl_q_p2(j))) THEN
                cl_q_p2(j) = 0.0_wp     ! Check for NaN
              ENDIF
              IF (ropp_pp_isnan(cl_exphase_l2_nco(j))) THEN
                cl_exphase_l2_nco(j) = 0.0_wp     ! Check for NaN
              ENDIF
            ENDDO

            ! 7.4 Compute closed loop L2 phase

            ALLOCATE(cl_phase_l2_iq(n_cl))
            cl_phase_l2_iq(:) = ATAN2(cl_q_p2(:), cl_i_p2(:)+tiny(1.0_wp))
            CALL Accumulate_Phase(cl_phase_l2_iq)
            cl_exphase_p2(:) = cl_exphase_l2_nco(:) + &
               (c_light/(2.0_wp*pi*f_L2))*cl_phase_l2_iq(:)

            ! 7.5 Closed loop lost carrier flag

            ALLOCATE(cl_LCF(n_cl))
            cl_LCF(:) = 0

            ts = MINVAL(ABS(cl_dtime(2:n_cl) - cl_dtime(1:n_cl-1)))

            IF (BTEST(ro_data%PCD, PCD_rising)) THEN    ! Rising occultation
              DO j=n_cl-1,1,-1
                ts1 = ABS(cl_dtime(j) - cl_dtime(j+1))
                IF (ts1 > 1.05*ts) THEN
                  cl_LCF(j)   = IBSET(cl_LCF(j),  3)
                  cl_LCF(j+1) = IBSET(cl_LCF(j+1),3)
                ENDIF
              ENDDO
            ELSE                                        ! Setting occultation
              DO j=2,n_cl
                ts1 = ABS(cl_dtime(j) - cl_dtime(j-1))
                IF (ts1 > 1.05*ts) THEN
                  cl_LCF(j)   = IBSET(cl_LCF(j),  3)
                  cl_LCF(j-1) = IBSET(cl_LCF(j-1),3)
                ENDIF
              ENDDO
            ENDIF

          ENDIF

!-------------------------------------------------------------------------------
! 8. Raw sampling data
!-------------------------------------------------------------------------------

          CALL ncdf_getvar('have_rs_data', have_rs)
          IF (have_rs > 0) THEN

            ro_data%PCD = IBSET(ro_data%PCD, PCD_open_loop)

            CALL ncdf_getsize('rs_dtime', n_rs)
            WRITE(nstr, '(i6)') n_rs
            CALL message(msg_diag,"Reading "//nstr//"raw sampling data points")

            ! 8.1 Time

            ALLOCATE(rs_dtime(n_rs))
            CALL ncdf_getvar('rs_dtime', rs_dtime)

            ! 8.2 Position and velocity

            CALL ncdf_getsize('rs_r_gns', n_xyz, dim=1)
            ALLOCATE(rs_r_gns(n_xyz, n_rs))
            CALL ncdf_getvar('rs_r_gns', rs_r_gns)
            ALLOCATE(rs_v_gns(n_xyz, n_rs))
            CALL ncdf_getvar('rs_v_gns', rs_v_gns)
            ALLOCATE(rs_r_leo(n_xyz, n_rs))
            CALL ncdf_getvar('rs_r_rec', rs_r_leo)
            ALLOCATE(rs_v_leo(n_xyz, n_rs))
            CALL ncdf_getvar('rs_v_rec', rs_v_leo)

            ! 8.3 Phase and amplitude

            ! Navigation bits
            ALLOCATE(rs_navbit_int(n_rs))
            ALLOCATE(rs_navbit_ext(n_rs))
            CALL ncdf_getvar('rs_navbits_internal', rs_navbit_int)
            CALL ncdf_getvar('rs_have_navbits_external', have_nb)
            IF (have_nb > 0) THEN
              CALL message(msg_diag, "Reading external navigation bits from file")
              CALL ncdf_getvar('rs_navbits_external', rs_navbit_ext)
            ELSE
              CALL message(msg_diag, "No external navigation bits in file")
              rs_navbit_ext(:) = rs_navbit_int(:)
            ENDIF
            ALLOCATE(rs_i_ca_uncorr(n_rs))
            CALL ncdf_getvar('rs_i_ca_uncorr', rs_i_ca_uncorr)
            ALLOCATE(rs_q_ca_uncorr(n_rs))
            CALL ncdf_getvar('rs_q_ca_uncorr', rs_q_ca_uncorr)
            ALLOCATE(rs_exphase_l1_nco(n_rs))
            CALL ncdf_getvar('rs_exphase_l1_nco', rs_exphase_l1_nco)
            ALLOCATE(rs_snr_ca(n_rs))
            CALL ncdf_getvar('rs_snr_ca', rs_snr_ca)

            ! 8.4 Compute RS phase

            ALLOCATE(rs_phase_l1_iq(n_rs))
            ALLOCATE(rs_exphase_l1(n_rs))
            DO j=1,n_rs
              rs_phase_l1_iq(j) = ATAN2(rs_q_ca_uncorr(j), rs_i_ca_uncorr(j)+tiny(1.0_wp))
            ENDDO

            CALL Accumulate_Phase(rs_phase_l1_iq)

            rs_exphase_l1(:) = rs_exphase_l1_nco(:) - &
               (c_light/(2.0_wp*pi*f_L1))*rs_phase_l1_iq(:)

            ! 8.5 RS closed loop flag

            ALLOCATE(rs_lcf(n_rs))
            rs_lcf(:) = 0

            ts = MINVAL(ABS(rs_dtime(2:n_rs) - rs_dtime(1:n_rs-1)))

            IF (BTEST(ro_data%PCD, PCD_rising)) THEN    ! Rising occultation
              DO j=n_rs-1,1,-1
                ts1 = ABS(rs_dtime(j) - rs_dtime(j+1))
                IF (ts1 > 1.05*ts) THEN
                  rs_LCF(j)   = IBSET(rs_LCF(j),  3)
                  rs_LCF(j+1) = IBSET(rs_LCF(j+1),3)
                ENDIF
              ENDDO
            ELSE                                        ! Setting occultation
              DO j=2,n_rs
                ts1 = ABS(rs_dtime(j) - rs_dtime(j-1))
                IF (ts1 > 1.05*ts) THEN
                  rs_LCF(j)   = IBSET(rs_LCF(j),  3)
                  rs_LCF(j-1) = IBSET(rs_LCF(j-1),3)
                ENDIF
              ENDDO
            ENDIF

            rs_LCF(:) = IBSET(rs_LCF(:), 0)        ! Open loop mode

            WHERE (NINT(rs_navbit_ext(:)) == 1)    ! External navbit
              rs_LCF(:) = IBSET(rs_LCF(:), 1)
            ENDWHERE

            rs_LCF(:) = IBSET(rs_LCF(:), 2)        ! Navbit quality OK

            WHERE (NINT(rs_navbit_int(:)) == 1)    ! Alternative navbit
              rs_LCF(:) = IBSET(rs_LCF(:), 4)
            ENDWHERE

            rs_LCF(:) = IBSET(rs_LCF(:), 5)        ! Alternative navbit quality


            ! 8.6 Setting flag for duplicated CL record

            WHERE((rs_dtime(1) < cl_dtime(:)).AND.(cl_dtime(:) < rs_dtime(n_rs)))
              cl_LCF(:) = IBSET(cl_LCF(:),6)
            ENDWHERE

          ENDIF

          CALL ncdf_close(ifiles(k))

        ELSE

          CALL message(msg_fatal,"Unable to read input netCDF file " // ifiles(k))
        ENDIF

!-------------------------------------------------------------------------------
! 7. Combine CL and RS data
!-------------------------------------------------------------------------------

        ! 7.1 Set number of output data

        n = n_cl + n_rs
        WRITE(nstr, '(i6)') n
        CALL message(msg_diag, "Read " // TRIM(nstr) // " data points")

        ! 7.2 Initialise ro_data structure variables

        CALL ropp_io_init(ro_data%Lev1a, n)
        ALLOCATE(lcf(n))

        ! 7.3 Set output variables

        IF (BTEST(ro_data%PCD, PCD_rising)) THEN    ! Rising occultation

          DO j=1,n_cl
            ro_data%lev1a%dtime(n_rs+j)    = cl_dtime(j)
            ro_data%lev1a%r_gns(n_rs+j,:)  = cl_r_gns(:,j)
            ro_data%lev1a%v_gns(n_rs+j,:)  = cl_v_gns(:,j)
            ro_data%lev1a%r_leo(n_rs+j,:)  = cl_r_leo(:,j)
            ro_data%lev1a%v_leo(n_rs+j,:)  = cl_v_leo(:,j)
            ro_data%lev1a%snr_L1ca(n_rs+j) = cl_snr_ca(j)
            ro_data%lev1a%snr_L1p(n_rs+j)  = cl_snr_p1(j)
            ro_data%lev1a%snr_L2p(n_rs+j)  = cl_snr_p2(j)
            ro_data%lev1a%phase_L1(n_rs+j) = cl_exphase_ca(j)
            ro_data%lev1a%phase_L2(n_rs+j) = cl_exphase_p2(j)
            lcf(n_rs+j) = cl_LCF(j)
          ENDDO

          IF (n_rs /= 0) THEN
            DO j=1,n_rs
              ro_data%lev1a%dtime(j)    = rs_dtime(j)
              ro_data%lev1a%r_gns(j,:)  = rs_r_gns(:,j)
              ro_data%lev1a%v_gns(j,:)  = rs_v_gns(:,j)
              ro_data%lev1a%r_leo(j,:)  = rs_r_leo(:,j)
              ro_data%lev1a%v_leo(j,:)  = rs_v_leo(:,j)
              ro_data%lev1a%snr_L1ca(j) = rs_snr_ca(j)
              ro_data%lev1a%snr_L1p(j)  = ropp_MDFV
              ro_data%lev1a%snr_L2p(j)  = ropp_MDFV
              ro_data%lev1a%phase_L1(j) = rs_exphase_l1(j)
              ro_data%lev1a%phase_L2(j) = ropp_MDFV
              lcf(j) = rs_LCF(j)
            ENDDO
          ENDIF

          ts = MINVAL(ABS(cl_dtime(2:n_cl) - cl_dtime(1:n_cl-1)))

          IF (rs_dtime(n_rs) < cl_dtime(1)-1.5*ts) THEN
            LCF(n_rs:n_rs+1) = IBSET(LCF(n_rs:n_rs+1),3)
          ENDIF

        ELSE                                        ! Setting occultation

          DO j=1,n_cl
            ro_data%lev1a%dtime(j)    = cl_dtime(j)
            ro_data%lev1a%r_gns(j,:)  = cl_r_gns(:,j)
            ro_data%lev1a%v_gns(j,:)  = cl_v_gns(:,j)
            ro_data%lev1a%r_leo(j,:)  = cl_r_leo(:,j)
            ro_data%lev1a%v_leo(j,:)  = cl_v_leo(:,j)
            ro_data%lev1a%snr_L1ca(j) = cl_snr_ca(j)
            ro_data%lev1a%snr_L1p(j)  = cl_snr_p1(j)
            ro_data%lev1a%snr_L2p(j)  = cl_snr_p2(j)
            ro_data%lev1a%phase_L1(j) = cl_exphase_ca(j)
            ro_data%lev1a%phase_L2(j) = cl_exphase_p2(j)
            lcf(j) = cl_LCF(j)
          ENDDO

          IF (n_rs /= 0) THEN
            DO j=1,n_rs
              ro_data%lev1a%dtime(n_cl+j)    = rs_dtime(j)
              ro_data%lev1a%r_gns(n_cl+j,:)  = rs_r_gns(:,j)
              ro_data%lev1a%v_gns(n_cl+j,:)  = rs_v_gns(:,j)
              ro_data%lev1a%r_leo(n_cl+j,:)  = rs_r_leo(:,j)
              ro_data%lev1a%v_leo(n_cl+j,:)  = rs_v_leo(:,j)
              ro_data%lev1a%snr_L1ca(n_cl+j) = rs_snr_ca(j)
              ro_data%lev1a%snr_L1p(n_cl+j)  = ropp_MDFV
              ro_data%lev1a%snr_L2p(n_cl+j)  = ropp_MDFV
              ro_data%lev1a%phase_L1(n_cl+j) = rs_exphase_l1(j)
              ro_data%lev1a%phase_L2(n_cl+j) = ropp_MDFV
              lcf(n_cl+j) = rs_LCF(j)
            ENDDO

            ts = MINVAL(ABS(cl_dtime(2:n_cl) - cl_dtime(1:n_cl-1)))

            IF (cl_dtime(n_cl) < rs_dtime(1)-1.5*ts) THEN
              LCF(n_cl:n_cl+1) = IBSET(LCF(n_cl:n_cl+1),3)
            ENDIF
          ENDIF

        ENDIF

!-------------------------------------------------------------------------------
! 8. Missing/invalid data checks
!-------------------------------------------------------------------------------

        DO j=1,n
          IF (ropp_pp_isnan(ro_data%Lev1a%phase_L1(j))) THEN
            ro_data%Lev1a%phase_L1(j) = ropp_MDFV     ! Check for NaN
            LCF(j) = IBSET(LCF(j), 3)
          ENDIF
          IF (ropp_pp_isnan(ro_data%Lev1a%phase_L2(j))) THEN
            ro_data%Lev1a%phase_L2(j) = ropp_MDFV     ! Check for NaN
          ENDIF
          IF (ropp_pp_isnan(ro_data%Lev1a%snr_L1ca(j))) THEN
            ro_data%Lev1a%snr_L1ca(j) = ropp_MDFV     ! Check for NaN
            LCF(j) = IBSET(LCF(j), 3)
          ENDIF
          IF (ropp_pp_isnan(ro_data%Lev1a%snr_L1p(j))) THEN
            ro_data%Lev1a%snr_L1p(j) = ropp_MDFV     ! Check for NaN
          ENDIF
          IF (ropp_pp_isnan(ro_data%Lev1a%snr_L2p(j))) THEN
            ro_data%Lev1a%snr_L2p(j) = ropp_MDFV     ! Check for NaN
          ENDIF
        ENDDO

!-------------------------------------------------------------------------------
! 9. Write data
!-------------------------------------------------------------------------------

        ro_data%Lev1a%reference_frame%r_gns = "ECI"
        ro_data%Lev1a%reference_frame%r_leo = "ECI"

        ro_data%Lev1a%range%phase = (/ MIN(MINVAL(ro_data%Lev1a%phase_L1),   &
           ro_data%Lev1a%range%phase(1)),  &
           MAX(MAXVAL(ro_data%Lev1a%phase_L1),ro_data%Lev1a%range%phase(2)) /)

        ! 7.7 Add lost carrier information to file

        CALL ropp_io_addvar_rodataD1d(ro_data,   &
                                      name     = "open_loop_lcf",        &
                                      long_name= "OpenLoop Phase Model", &
                                      units    = "",                     &
                                      range    = (/-1000000.0_wp, 1000000.0_wp/),&
                                      DATA     = REAL(LCF,wp) )

!-------------------------------------------------------------------------------
! 8. Write data
!-------------------------------------------------------------------------------

        CALL message(msg_info, "Writing to output file " // TRIM(ofile) // "\n")
        CALL ropp_io_write(ro_data,ofile, ranchk=ranchk, output_precision='double')

!-------------------------------------------------------------------------------
! 9. Clean up
!-------------------------------------------------------------------------------

        CALL ropp_io_free(ro_data)
        DEALLOCATE(LCF)
        IF (have_cl > 0) THEN
         DEALLOCATE(cl_dtime)
         DEALLOCATE(cl_r_gns)
         DEALLOCATE(cl_v_gns)
         DEALLOCATE(cl_r_leo)
         DEALLOCATE(cl_v_leo)
         DEALLOCATE(cl_exphase_ca)
         DEALLOCATE(cl_exphase_p1)
         DEALLOCATE(cl_exphase_p2)
         DEALLOCATE(cl_snr_ca)
         DEALLOCATE(cl_snr_p1)
         DEALLOCATE(cl_snr_p2)
         DEALLOCATE(cl_i_p2)
         DEALLOCATE(cl_q_p2)
         DEALLOCATE(cl_exphase_l2_nco)
         DEALLOCATE(cl_lcf)
       ENDIF
       IF (have_rs > 0) THEN
         DEALLOCATE(rs_dtime)
         DEALLOCATE(rs_r_gns)
         DEALLOCATE(rs_v_gns)
         DEALLOCATE(rs_r_leo)
         DEALLOCATE(rs_v_leo)
         DEALLOCATE(rs_navbit_int)
         DEALLOCATE(rs_navbit_ext)
         DEALLOCATE(rs_i_ca_uncorr)
         DEALLOCATE(rs_q_ca_uncorr)
         DEALLOCATE(rs_snr_ca)
         DEALLOCATE(rs_exphase_l1_nco)
         DEALLOCATE(rs_phase_l1_iq)
         DEALLOCATE(rs_exphase_l1)
         DEALLOCATE(rs_lcf)
       ENDIF

     END DO
   END DO
 CONTAINS

!-------------------------------------------------------------------------------
! 10. Accumulate phase
!-------------------------------------------------------------------------------

    SUBROUTINE Accumulate_Phase(Ph, Sign)   ! (Array of (accumulated) phase, dir)

! Method:
!   Sign = 0 or no Sign:
!      Adding +-2*Pi where phase jumps from
!      +-Pi to -+Pi,
!   Sign > 0:
!      Adding +2*Pi where phase jumps from
!      - to +
!   Sign < 0
!      Adding -2*Pi where phase jumps from
!      + to -

    ! 15.1 Declarations

    USE typesizes, ONLY: wp => EightByteReal
    IMPLICIT NONE

    REAL(wp), DIMENSION(:), INTENT(inout) :: Ph   ! Phase --> accumulated phase
    INTEGER, OPTIONAL,      INTENT(in)    :: Sign ! Phase change sign

    INTEGER  :: i     ! Array index
    INTEGER  :: PSign ! Phase change sign

    ! 15.2 Determine phase change sign

    IF (.NOT. PRESENT(Sign)) THEN
      PSign = 0
    ELSE
      PSign = Sign
    ENDIF

    ! 15.3 Accumulate phase

    IF (PSign == 0) THEN
      DO i=2,SIZE(Ph)
        Ph(i) = Ph(i-1) + MODULO(Ph(i)-Ph(i-1)+pi, 2*pi) - pi
      ENDDO
    ELSEIF (PSign > 0) THEN
      DO i=2,SIZE(Ph)
        Ph(i) = Ph(i-1) + MODULO(Ph(i)-Ph(i-1), 2*pi)
      ENDDO
    ELSEIF (PSign < 0) THEN
      DO i=2,SIZE(Ph)
        Ph(i) = Ph(i-1) + MODULO(Ph(i)-Ph(i-1)+2*pi, 2*pi) - 2*pi
      ENDDO
    ENDIF

  END SUBROUTINE Accumulate_Phase


!-------------------------------------------------------------------------------
! 11. Usage information
!-------------------------------------------------------------------------------

  SUBROUTINE usage()
    PRINT *, 'Purpose:'
    PRINT *, '  Convert EUMETSAT format CL and RS GRAS data'
    PRINT *, '  to ROPP format netCDF'
    PRINT *, 'Usage:'
    PRINT *, '  > ropp_pp_grasrs2ropp [<options>] <input_file(s)>'
    PRINT *, 'Options:'
    PRINT *, '  -o <output_file>  name of ROPP netCDF output file'
    PRINT *, '                    (default: ropp_pp_gras.nc)'
    PRINT *, '  -d                output additional diagnostics'
    PRINT *, '  -h                this help'
    PRINT *, '  -v                version information'
    PRINT *, ''
  END SUBROUTINE usage

!-------------------------------------------------------------------------------
! 12. Version information
!-------------------------------------------------------------------------------

  SUBROUTINE version_info()
    CHARACTER (LEN=40) :: version
    version = ropp_pp_version()
    PRINT *, 'ropp_pp_grasrs2ropp - pre-processor conversion tool:'
    PRINT *, 'Convert GRAS raw sampling data to ROPP format.'
    PRINT *, ''
    PRINT *, 'This program is part of ROPP (PP) Release ' // TRIM(version)
    PRINT *, ''
  END SUBROUTINE version_info

END PROGRAM ropp_pp_grasrs2ropp

