! $Id: ropp_fm_state2roprof.f90 3551 2013-02-25 09:51:28Z idculv $

SUBROUTINE ropp_fm_state1d2roprof(x, ro_data)

!****s* Copying/ropp_fm_state1d2roprof *
!
! NAME
!    ropp_fm_state2roprof - Copy elements of a state vector to an ROprof
!                              structure
!
! SYNOPSIS
!    type(<some state vector type>) :: x 
!    type(ROprof)                   :: ro_data
!       ...
!    call ropp_fm_state2roprof(x, ro_data)
! 
! DESCRIPTION
!    This subroutine copies Level 2b, c and d (if applicable) data as
!    contained in a state vector to a radio occultation profile data
!    structure.
!
! INPUTS
!   x    State vector structure.
!
! OUTPUT
!   ro_data  Radio occultation profile data.
!
! NOTES
!   Data is copied into the ROprof data structure without unit conversion; 
!   thus, the units in the ROprof data structure for Level 2b,c, and (if
!   applicable) d must be set to the units used by the state vector
!   variables. This can be accomplished with the ropp_fm_set_units()
!   subroutine.
!
! SEE ALSO
!   State1dFM
!   ropp_fm_roprof2state
!   ropp_fm_set_units
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
  USE ropp_utils
  USE ropp_io
  USE ropp_io_types, ONLY: ROprof
  USE ropp_fm
  USE ropp_fm_types, ONLY: State1dFM
  USE ropp_fm_copy,  not_this => ropp_fm_state1d2roprof

  IMPLICIT NONE

  TYPE(State1dFM),          INTENT(in)    :: x           ! State vector
  TYPE(ROprof),             INTENT(inout) :: ro_data     ! RO data type
  REAL(wp), DIMENSION(x%n_lev)            :: temp_sigma  ! Temporary storage
  REAL(wp), DIMENSION(x%n_lev)            :: press_sigma ! Temporary storage
  REAL(wp), DIMENSION(x%n_lev)            :: geop_sigma  ! Temporary storage
  INTEGER                                 :: i, j, n     ! Indices

  CHARACTER(len = 256)                    :: routine

!-------------------------------------------------------------------------------
! 2. Error handling
!-------------------------------------------------------------------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_fm_state2roprof')

!-------------------------------------------------------------------------------
! 3. Ensure correct units
!-------------------------------------------------------------------------------

  CALL ropp_fm_set_units(ro_data)

!-------------------------------------------------------------------------------
! 4. Copy geolocation
!-------------------------------------------------------------------------------

  ro_data%georef%lon = x%lon
  ro_data%georef%lat = x%lat

!-------------------------------------------------------------------------------
! 5. Copy level coefficients (if exist)
!-------------------------------------------------------------------------------
  
  IF(ASSOCIATED(x%ak))THEN
     CALL ropp_io_init(ro_data%Lev2d, SIZE(x%ak))
     ro_data%Lev2d%level_coeff_a = x%ak
     ro_data%Lev2d%level_coeff_b = x%bk
     ro_data%Lev2d%level_type="HYBRID ECMWF" 
  ELSE 
     ro_data%Lev2d%level_type="METOFFICE" 
  ENDIF
  
!-------------------------------------------------------------------------------
! 6. Copy surface parameters
!-------------------------------------------------------------------------------

  CALL ropp_io_init(ro_data%Lev2c, 1)

  ro_data%Lev2c%geop_sfc = x%geop_sfc

!-------------------------------------------------------------------------------
! 7. Copy meteorological parameters
!-------------------------------------------------------------------------------

  n = 2*x%n_lev + 1 ! Number of elements in the state vector

! 7.1 Save error information

  temp_sigma(:)  = ropp_MDFV
  press_sigma(:) = ropp_MDFV
  geop_sigma(:)  = ropp_MDFV
  IF(ASSOCIATED(ro_data%Lev2b%temp_sigma))   &
     temp_sigma  = ro_data%Lev2b%temp_sigma
  IF(ASSOCIATED(ro_data%Lev2b%press_sigma))  &
     press_sigma = ro_data%Lev2b%press_sigma
  IF(ASSOCIATED(ro_data%Lev2b%geop_sigma))   &
     geop_sigma  = ro_data%Lev2b%geop_sigma 

! 7.2 Set level numbers
  
  CALL ropp_io_init(ro_data%Lev2b, x%n_lev)

! 7.3 Temperature

  ro_data%Lev2b%temp = x%temp

! 7.4 Humidity

  ro_data%Lev2b%shum = x%shum
    
  !! Check for negative values

  WHERE ( ro_data%Lev2b%shum < 0.0 ) ro_data%Lev2b%shum = 1.0e-7_wp

! 7.5 Pressure

  ! 7.5.1 ECMWF-type hybrid levels

  IF ( INDEX(ro_data%Lev2d%level_type,'ECMWF') > 0 ) THEN
     IF (x%use_logp) THEN
        ro_data%Lev2c%press_sfc = EXP(x%state(2*x%n_lev+1)) * 100.0_wp
     ELSE
        ro_data%Lev2c%press_sfc = x%state(2*x%n_lev+1)
     ENDIF
  ENDIF
  ro_data%Lev2b%press = x%pres

  ! 7.5.2 Met Office-type geopotential height levels

  IF ( INDEX(ro_data%Lev2d%level_type,'METOFFICE') > 0 ) THEN
     IF (x%use_logp) THEN
        ro_data%Lev2c%press_sfc = EXP(x%state(1)) * 100.0_wp
        ro_data%Lev2b%press = EXP(x%state(2:x%n_lev+1)) * 100.0_wp
     ELSE
        ro_data%Lev2c%press_sfc = x%state(1)     
        ro_data%Lev2b%press = x%state(2:x%n_lev+1)
     ENDIF
  ENDIF
    
! 7.6 Geopotential height

  ro_data%Lev2b%geop = x%geop

! 7.7 Quality confidence values
  
  IF(x%state_ok)THEN
     ro_data%Lev2b%meteo_qual(:) = 100.0_wp
     ro_data%Lev2c%press_sfc_qual = 100.0_wp
  ELSE
     ro_data%Lev2b%meteo_qual(:) = 0.0_wp
     ro_data%Lev2c%press_sfc_qual = 0.0_wp
  ENDIF

!-------------------------------------------------------------------------------
! 8. Error estimates: ECMWF-type hybrid levels
!-------------------------------------------------------------------------------

  IF ( INDEX(ro_data%Lev2d%level_type,'HYBRID') > 0 ) THEN
     
     IF (ASSOCIATED(x%cov%d) .AND. SIZE(x%cov%d) == n*(n+1)/2) THEN

! 8.1 Temperature

        DO i = 1, x%n_lev
           ro_data%Lev2b%temp_sigma(i) = SQRT(x%cov%d(i + i*(i-1)/2))
        END DO
        
! 8.2 Humidity

        IF (x%use_logq) THEN
           DO i = 1, x%n_lev
              j = x%n_lev + i
              ro_data%Lev2b%shum_sigma(i) = SQRT(x%cov%d(j + j*(j-1)/2)) *   &
                                             ro_data%Lev2b%shum(i)
           ENDDO
        ELSE
           DO i = 1, x%n_lev
              j = x%n_lev + i
              ro_data%Lev2b%shum_sigma(i) = SQRT(x%cov%d(j + j*(j-1)/2))
           ENDDO
        ENDIF
        
! 8.3 Surface pressure

        n = 2*x%n_lev + 1
        IF (x%use_logp) THEN
           ro_data%Lev2c%press_sfc_sigma = SQRT(x%cov%d(n + n*(n-1)/2)) *    &
                                            ro_data%Lev2c%press_sfc
        ELSE
           ro_data%Lev2c%press_sfc_sigma = SQRT(x%cov%d(n + n*(n-1)/2))
        ENDIF
        
! 8.4 Pressure
        
        ro_data%Lev2b%press_sigma = press_sigma
                
! 8.5 Geopotential height

        ro_data%Lev2b%geop_sigma = geop_sigma

     ENDIF

  ENDIF
  
!-------------------------------------------------------------------------------
! 9. Error estimates: MetOffice-type geopotential height levels
!-------------------------------------------------------------------------------

  IF ( INDEX(ro_data%Lev2d%level_type,'METOFFICE') > 0 ) THEN 

     IF (ASSOCIATED(x%cov%d) .AND. SIZE(x%cov%d) == n*(n+1)/2) THEN

! 9.1 Surface pressure

        IF (x%use_logp) THEN
           ro_data%Lev2c%press_sfc_sigma = SQRT(x%cov%d(1)) *      &
                                            ro_data%lev2c%press_sfc
        ELSE
           ro_data%Lev2c%press_sfc_sigma = SQRT(x%cov%d(1))
        ENDIF

! 9.2 Pressure

        IF (x%use_logp) THEN
           DO i = 2, x%n_lev+1
              ro_data%Lev2b%press_sigma(i-1) = SQRT(x%cov%d(i + i*(i-1)/2)) *  &
                                                ro_data%Lev2b%press(i-1)
           END DO
        ELSE
           DO i = 2, x%n_lev+1
              ro_data%Lev2b%press_sigma(i-1) = SQRT(x%cov%d(i + i*(i-1)/2))
           END DO
        ENDIF

! 9.3 Humidity

        IF (x%use_logq) THEN
           DO i = 1, x%n_lev
              j = x%n_lev + 1 + i
              ro_data%Lev2b%shum_sigma(i) = SQRT(x%cov%d(j + j*(j-1)/2)) *    &
                                             ro_data%Lev2b%shum(i)
           END DO
        ELSE
           DO i = 1, x%n_lev
              j = x%n_lev + 1 + i
              ro_data%Lev2b%shum_sigma(i) = SQRT(x%cov%d(j + j*(j-1)/2))
           END DO
        ENDIF
        
! 9.4 Temperature

        ro_data%Lev2b%temp_sigma = temp_sigma
    
! 9.5 Geopotential height

        ro_data%Lev2b%geop_sigma = geop_sigma

     ENDIF
  ENDIF

!-------------------------------------------------------------------------------
! 10. Clean up
!-------------------------------------------------------------------------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_fm_state1d2roprof


!****s* Copying/ropp_fm_state2d2roprof *
!
! NAME
!    ropp_fm_state2roprof - Copy elements of a state vector to an ROprof2d
!                              structure
!
! SYNOPSIS
!    type(<some state vector type>) :: x 
!    type(ROprof)                   :: ro_data
!       ...
!    call ropp_fm_state2roprof(x, ro_data)
! 
! DESCRIPTION
!    This subroutine copies Level 2b, c and d (if applicable) data as
!    contained in a state vector to a radio occultation profile data
!    structure.
!
! INPUTS
!   x    State vector structure.
!
! OUTPUT
!   ro_data  Radio occultation profile data.
!
! NOTES
!   Data is copied into the ROprof data structure without unit conversion; 
!   thus, the units in the ROprof data structure for Level 2b,c, and (if
!   applicable) d must be set to the units used by the state vector
!   variables. This can be accomplished with the ropp_fm_set_units()
!   subroutine.
!
! SEE ALSO
!   State1dFM
!   ropp_fm_roprof2state
!   ropp_fm_set_units
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


SUBROUTINE ropp_fm_state2d2roprof(x, ro_data)

! 1. Declarations
! ---------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_utils
  USE ropp_io
  USE ropp_io_types, ONLY: ROprof2d
  USE ropp_fm
  USE ropp_fm_types, ONLY: State2dFM
  USE ropp_fm_copy,  not_this => ropp_fm_state2d2roprof

  IMPLICIT NONE

  TYPE(State2dFM),          INTENT(in)    :: x        ! state vector
  TYPE(ROprof2d),           INTENT(inout) :: ro_data  ! RO data type
  INTEGER,  DIMENSION(2)                  :: ndim

  CHARACTER(len = 256)                    :: routine

! 2. Error handling
! -----------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_fm_state2d2roprof')

! 3. Ensure correct units
! -----------------------

  call ropp_fm_set_units(ro_data) 

! 4. Copy geolocation
! -------------------

!!  ro_data%georef%lon(:) = x%lon(:)   ???
!!  ro_data%georef%lat(:) = x%lat(:)   ???

! 5. Copy level coefficients (if exist)
! --------------------------
  
  IF(ASSOCIATED(x%ak))THEN
    CALL ropp_io_init(ro_data%Lev2d, SIZE(x%ak))
     ro_data%Lev2d%level_coeff_a = x%ak
     ro_data%Lev2d%level_coeff_b = x%bk
     ro_data%Lev2d%level_type="HYBRID ECMWF" 
  ELSE 
     ro_data%Lev2d%level_type="METOFFICE" 
  ENDIF
  
! 6. Copy surface parameters
! --------------------------

  ndim(1)=x%n_lev
  ndim(2)=x%n_horiz

  CALL ropp_io_init(ro_data%Lev2c, ndim)

  ro_data%Lev2c%geop_sfc(:) = x%geop_sfc(:)
  ro_data%Lev2c%lat_2d(:) = x%lat(:)
  ro_data%Lev2c%lon_2d(:) = x%lon(:)
  ro_data%Lev2c%dtheta = x%dtheta
  

! 7. Copy meteorological parameters
! ---------------------------------

 ! 7.1 Set level numbers
  
  CALL ropp_io_init(ro_data%Lev2b, ndim)

! 7.2 Temperature

  ro_data%Lev2b%temp(:,:) = x%temp(:,:)

! 7.2 Humidity

  ro_data%Lev2b%shum(:,:) = x%shum(:,:)

! 7.3 Pressure

  ! 7.3.1 ECMWF-type hybrid levels

  IF ( INDEX(ro_data%Lev2d%level_type,'ECMWF') > 0 ) THEN
     ro_data%Lev2c%press_sfc(:) = x%pres_sfc(:)
     ro_data%Lev2b%press(:,:) = x%pres(:,:)  
  ENDIF

  ! 7.3.2 Met Office-type geopotential height levels

!!  if ( INDEX(ro_data%Lev2d%level_type,'METOFFICE') > 0 ) then
!!     ro_data%Lev2c%press_sfc = x%state(1)     
!!     ro_data%Lev2b%press = x%state(2:x%n_lev+1)
!!  endif
    
! 7.4 Geopotential height

  ro_data%Lev2b%geop(:,:) = x%geop(:,:)

! 7.5 Quality confidence values
  
  IF(x%state_ok)THEN
     ro_data%Lev2b%meteo_qual(:,:) = 100.0_wp
     ro_data%Lev2c%press_sfc_qual = 100.0_wp
  ELSE
     ro_data%Lev2b%meteo_qual(:,:) = 0.0_wp
     ro_data%Lev2c%press_sfc_qual = 0.0_wp
  ENDIF


! 10. Clean up
! ------------

  CALL message_set_routine(routine)

END SUBROUTINE ropp_fm_state2d2roprof
