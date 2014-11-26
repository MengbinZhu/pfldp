# Makefile.am for ropp_1dvar/common
# =================================

# 1. Common (for several 1DVar implementations) routines
# ------------------------------------------------------

EXTRA_DIST = ropp_1dvar.f90 ropp_1dvar_types.f90 \
             ropp_1dvar_read_config.f90          \
	     ropp_1dvar_covar_bg.f90             \
	     ropp_1dvar_covar_bangle.f90         \
	     ropp_1dvar_covar_refrac.f90         \
             ropp_1dvar_solve.f90                \
             ropp_1dvar_cost.f90	         \
             ropp_1dvar_diagnostics.f90          \
             ropp_1dvar_diag2roprof.f90          \
	     ropp_1dvar.f90			 \
             ropp_1dvar_levmarq.f90	         \
	     ropp_1dvar_minropp.f90              \
             ropp_1dvar_version.f90



# 2. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@
