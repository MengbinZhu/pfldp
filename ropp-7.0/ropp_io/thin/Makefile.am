# Makefile.am for ropp_io/thin
# ============================

# 1. The THIN part of the ropp_io library
# ---------------------------------------

EXTRA_DIST = ropp_io_thin.f90           \
             ropp_io_thin_fixed.f90     \
             ropp_io_thin_select.f90    \
             ropp_io_thin_sg.f90        \
             ropp_io_thin_skip.f90      

# 2. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@
