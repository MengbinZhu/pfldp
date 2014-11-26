! $Id: ropp_1dvar_solve.f90 3551 2013-02-25 09:51:28Z idculv $

!****p* 1DVar/ropp_1dvar_solve *
!
! NAME
!    ropp_1dvar_solve - Solve the 1DVar for background data.
!
! SYNOPSIS
!    ropp_1dvar_solve(obs, bg, state, config, diag)
!
! DESCRIPTION
!   Minimisation of the cost function is achieved by an iterative method which
!   computes the cost function value and updates the state vector on each of
!   n = 1, ..., n_iter iterations until convergence to a solution is achieved.
!   The state vector is updated using the ROPP-specific minimiser minROPP. This
!   implements a quasi-Newton method using a BFGS algorithm.
!
!
! INPUTS
!    obs         - Observation vector
!    bg          - Background vector
!    state       - First guess state vector
!    config      - Configuration options
!
! OUTPUT
!    state       - Modified state vector with 1dVar solution
!    diag        - Output diagnostics
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
! 1. Bending angle
!-------------------------------------------------------------------------------

SUBROUTINE ropp_1dvar_solve_bangle(obs, bg, state, config, diag)

! 1.1 Declarations
! ----------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_utils
  USE ropp_fm
  USE ropp_1dvar, not_this => ropp_1dvar_solve_bangle
  USE matrix

  IMPLICIT NONE

  TYPE(Obs1dBangle), INTENT(inout)    :: obs        ! Observation vector
  TYPE(State1dFM),   INTENT(inout)    :: bg         ! Background vector
  TYPE(State1dFM),   INTENT(inout)    :: state      ! State vector
  TYPE(VarConfig),   INTENT(in)       :: config     ! Configuration options
  TYPE(VarDiag),     INTENT(inout)    :: diag       ! Output diagnostics

  TYPE(State1dFM)                     :: control    ! Control vector
  TYPE(matrix_sq)                     :: precon     ! Preconditioning matrix
  REAL(wp)                            :: J          ! Cost function value
  REAL(wp)                            :: dJ         ! Change of cost fn
  REAL(wp)                            :: J_optimal  ! Ideal cost function
  REAL(wp)                            :: eps_grad   ! Convergence factor
  REAL(wp)                            :: gconv      ! Convergence criterion
  REAL(wp), DIMENSION(:), ALLOCATABLE :: delta_x    ! Difference solution-bg
  REAL(wp), DIMENSION(:), ALLOCATABLE :: J_grad     ! Cost gradient
  REAL(wp), DIMENSION(:), ALLOCATABLE :: J_dir      ! Minimiser direction
  INTEGER                             :: m_indic    ! Minimiser flag
  INTEGER                             :: c_indic    ! Cost flag
  INTEGER                             :: n_iter     ! Number of iterations
  INTEGER                             :: n_obs      ! Number of observations
  CHARACTER(len =  25)                :: J_str
  CHARACTER(len =   5)                :: niter_str
  CHARACTER(len = 256)                :: routine


! 1.2 Message handling
! --------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_1dvar_solve')

! 1.3 Useful things
! -----------------

  n_obs   = SIZE(obs % bangle)
  n_iter   = config % minropp % n_iter
  eps_grad = config % minropp % eps_grad

  ALLOCATE(J_grad(SIZE(state%state)))
  ALLOCATE(J_dir(SIZE(state%state)))

! 1.4 Preconditioning
! -------------------

  control = state

  IF (config % use_precond) THEN
     CALL message(msg_info, &
          "Using background error covariance matrix for preconditioning.\n")
!     ALLOCATE(cov_d(SIZE(state%state), SIZE(state%state)))
!     call matrix_pp2full(bg%cov, cov_d)
!     do i=1,SIZE(state%state)
!       do k=1,SIZE(state%state)
!         if (i /= k) cov_d(i,k) = 0.0_wp
!       enddo
!     enddo
!     CALL matrix_sqrt(cov_d, precon)
     CALL matrix_sqrt(bg % cov, precon)
     CALL ropp_state2control(precon, state % state, control % state)
  ENDIF

! 1.5 Initial value of cost function, its gradient and expected optimal value
! ----------------------------------------------------------------------------

  c_indic = 1

  CALL ropp_1dvar_cost(obs, bg, control, precon, J, J_grad, config, c_indic)

  IF (J > 10000.0_wp) THEN
    WRITE(J_str, '(A,F16.3)') 'J = ', J
    CALL message(msg_error, "Initial cost function too high." // J_str // &
                            "Not processing further. \n")
    diag % J = J
    diag % n_iter = config%minropp%n_iter
    state%state_ok = .false.
    RETURN
  ENDIF
  diag % J_init = J

  J_optimal = 0.5_wp * REAL(n_obs, wp)
  dJ = MAX(J - J_optimal, 0.1_wp*J)
  gconv = eps_grad*dsqrt(DOT_PRODUCT(J_grad,J_grad))   ! convergence criterion

! 1.6 Minimisation loop
! ---------------------

  m_indic = 1

  DO

     ! 1.6.1 Call minimiser
     CALL ropp_1dvar_minropp(control%state, J_grad, J_dir, dJ, gconv, &
                             n_iter, m_indic, config%minropp%n_updates, &
                             config%minropp%n_iter)

     IF (m_indic == 0) EXIT

     ! 1.6.2 Calculate cost function and it's gradient

     CALL ropp_1dvar_cost(obs, bg, control, precon, J, J_grad, config, c_indic)

     IF (c_indic == 0) m_indic = 0

  ENDDO


  ! 1.6.3 Output

  WRITE(niter_str,  '(i5)') n_iter
  niter_str  = ADJUSTL(niter_str)
  CALL message(msg_info, &
             'Minimization of cost function successfully finished ' // &
             '(according to additional\n   convergence criteria). ' // &
             'Number of required iterations: ' // TRIM(niter_str) // '.\n')

! 1.7 Copy solution back to state
! -------------------------------

  state = control

  IF (config % use_precond) THEN
     CALL ropp_control2state(precon, control % state, state % state)
  ENDIF

! 1.8 Update state vector variables - model-dependent
! ---------------------------------

  IF(ASSOCIATED(state%ak))THEN
     CALL ropp_fm_state2state_ecmwf(state)
  ELSE
     CALL ropp_fm_state2state_meto(state)
  ENDIF

! 1.9 Diagnostic data
! --------------------

  diag % J        = J
  IF (REAL(COUNT(obs % weights > 0.0_wp), wp) > 0) THEN
    diag % J_scaled = 2.0_wp * J / REAL(COUNT(obs % weights > 0.0_wp), wp)
  ENDIF
  diag % n_iter   = n_iter

  ALLOCATE (delta_x(size(state%state)))
  ALLOCATE (diag%J_bgr(size(state%state)))

  delta_x = state%state - bg%state
  diag % J_bgr = 0.5_wp * delta_x * matrix_solve(bg%cov, delta_x)

! 1.10 Clean up
! -------------

  DEALLOCATE(J_grad)
  DEALLOCATE(J_dir)
  DEALLOCATE(delta_x)

  CALL message_set_routine(routine)

END SUBROUTINE ropp_1dvar_solve_bangle


!-------------------------------------------------------------------------------
! 2. Refractivity
!-------------------------------------------------------------------------------

SUBROUTINE ropp_1dvar_solve_refrac(obs, bg, state, config, diag)

! 2.1 Declarations
! ----------------

  USE typesizes, ONLY: wp => EightByteReal
  USE ropp_utils
  USE ropp_fm
  USE ropp_1dvar, not_this => ropp_1dvar_solve_refrac
  USE matrix

  IMPLICIT NONE

  TYPE(Obs1dRefrac), INTENT(inout)    :: obs        ! Observation vector
  TYPE(State1dFM),   INTENT(inout)    :: bg         ! Background vector
  TYPE(State1dFM),   INTENT(inout)    :: state      ! State vector
  TYPE(VarConfig),   INTENT(in)       :: config     ! Configuration options
  TYPE(VarDiag),     INTENT(inout)    :: diag       ! Output diagnostics

  TYPE(State1dFM)                     :: control    ! Control vector
  TYPE(matrix_sq)                     :: precon     ! Preconditioning matrix
  REAL(wp)                            :: J          ! Cost function value
  REAL(wp)                            :: dJ         ! Change of cost fn
  REAL(wp)                            :: J_optimal  ! Ideal cost function
  REAL(wp)                            :: eps_grad   ! Convergence factor
  REAL(wp)                            :: gconv      ! Convergence criterion
  REAL(wp), DIMENSION(:), ALLOCATABLE :: delta_x    ! Difference solution-bg
  REAL(wp), DIMENSION(:), ALLOCATABLE :: J_grad     ! Cost gradient
  REAL(wp), DIMENSION(:), ALLOCATABLE :: J_dir      ! Minimiser direction
  INTEGER                             :: m_indic    ! Minimiser flag
  INTEGER                             :: c_indic    ! Cost flag
  INTEGER                             :: n_iter     ! Number of iterations
  INTEGER                             :: n_obs      ! Number of observations
  CHARACTER(len =  25)                :: J_str
  CHARACTER(len =   5)                :: niter_str
  CHARACTER(len = 256)                :: routine

! 2.2 Message handling
! --------------------

  CALL message_get_routine(routine)
  CALL message_set_routine('ropp_1dvar_solve')

! 2.3 Useful things
! -----------------

  n_obs   = SIZE(obs % refrac)
  n_iter   = config % minropp % n_iter
  eps_grad = config % minropp % eps_grad

  ALLOCATE(J_grad(SIZE(state%state)))
  ALLOCATE(J_dir(SIZE(state%state)))

! 2.4 Preconditioning
! -------------------

  control = state

  IF (config % use_precond) THEN
     CALL message(msg_info, &
          "Using background error covariance matrix for preconditioning.\n")
!     ALLOCATE(cov_d(SIZE(state%state), SIZE(state%state)))
!     call matrix_pp2full(bg%cov, cov_d)
!     do i=1,SIZE(state%state)
!       do k=1,SIZE(state%state)
!         if (i /= k) cov_d(i,k) = 0.0_wp
!       enddo
!     enddo
!     CALL matrix_sqrt(cov_d, precon)
     CALL matrix_sqrt(bg % cov, precon)
     CALL ropp_state2control(precon, state % state, control % state)
  ENDIF

! 2.5 Initial value of cost function, its gradient and expected optimal value
! ---------------------------------------------------------------------------

  c_indic = 1
  CALL ropp_1dvar_cost(obs, bg, control, precon, J, J_grad, config, c_indic)

  IF (J > 10000.0_wp) THEN
    WRITE(J_str, '(A,F16.3)') 'J = ', J
    CALL message(msg_error, "Initial cost function too high." // J_str // &
                            "Not processing further. \n")
    diag % J = J
    diag % n_iter = config%minropp%n_iter
    state%state_ok = .false.
    RETURN
  ENDIF
  diag % J_init = J

  J_optimal = 0.5_wp * REAL(n_obs, wp)
  dJ = MAX(J - J_optimal, 0.1_wp*J)
  gconv = eps_grad*dsqrt(DOT_PRODUCT(J_grad,J_grad))   ! convergence criterion

! 2.6 Minimisation loop
! ---------------------

  m_indic = 1

  DO

     ! 2.6.1 Call minimiser

    CALL ropp_1dvar_minropp(control%state, J_grad, J_dir, dJ, gconv, &
                              n_iter, m_indic, config%minropp%n_updates, &
                              config%minropp%n_iter)

     IF (m_indic == 0) EXIT

     ! 2.6.2 Calculate cost function and it's gradient

     CALL ropp_1dvar_cost(obs, bg, control, precon, J, J_grad, config, c_indic)

     IF (c_indic == 0) m_indic = 0

  ENDDO

  ! 2.6.3 Output

  WRITE(niter_str,  '(i5)') n_iter
  niter_str  = ADJUSTL(niter_str)
  CALL message(msg_info, &
       'Minimization of cost function successfully finished ' // &
       '(according to additional\n   convergence criteria). ' // &
       'Number of required iterations: ' // TRIM(niter_str) // '.\n')

! 2.7 Copy solution back to state
! -------------------------------

  state = control

  IF (config % use_precond) THEN
     CALL ropp_control2state(precon, control % state, state % state)
  ENDIF

! 2.8 Update state vector variables - model-dependent
! ---------------------------------

  IF(ASSOCIATED(state%ak))THEN
     CALL ropp_fm_state2state_ecmwf(state)
  ELSE
     CALL ropp_fm_state2state_meto(state)
  ENDIF

! 2.9 Diagnostic data
! --------------------

  diag % J        = J
  IF (  REAL(COUNT(obs % weights > 0.0_wp), wp) > 0) THEN
    diag % J_scaled = 2.0_wp * J / REAL(COUNT(obs % weights > 0.0_wp), wp)
  ENDIF
  diag % n_iter   = n_iter

  ALLOCATE (delta_x(size(state%state)))
  ALLOCATE (diag%J_bgr(size(state%state)))

  delta_x = (state%state - bg%state)
  diag % J_bgr = 0.5_wp * delta_x * matrix_solve(bg%cov, delta_x)

! 2.10 Clean up
! -------------

  DEALLOCATE(delta_x)
  DEALLOCATE(J_grad)
  DEALLOCATE(J_dir)

  CALL message_set_routine(routine)

END SUBROUTINE ropp_1dvar_solve_refrac
