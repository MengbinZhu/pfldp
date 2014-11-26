# Makefile.am for ropp_pp/common
# =================================

# 1. Common (for several PP implementations) routines
# ------------------------------------------------------

EXTRA_DIST = ropp_pp_constants.f90              \
             ropp_pp_copy.f90                   \
             ropp_pp_diag2roprof.f90            \
             ropp_pp.f90                        \
             ropp_pp_fft.f90                    \
             ropp_pp_filter.f90                 \
             ropp_pp_fourier_filter.f90         \
             ropp_pp_interpolate_trajectory.f90 \
             ropp_pp_interpol.f90               \
             ropp_pp_interpol_log.f90           \
             ropp_pp_monotonous.f90             \
             ropp_pp_read_config.f90            \
             ropp_pp_refrac_bg.f90              \
             ropp_pp_satellite_velocities.f90   \
             ropp_pp_sliding_polynomial.f90     \
             ropp_pp_spline.f90                 \
             ropp_pp_tdry.f90                   \
             ropp_pp_types.f90                  \
             ropp_pp_utils.f90                  \
             ropp_pp_version.f90

# 2. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@
