# Makefile.am for ropp_io/ropp
# ============================

# 1. The ROPP part of the ropp_io library
# ---------------------------------------

EXTRA_DIST = ropp_io_types.f90          \
             ropp_io.f90                \
             ropp_io_addvar.f90         \
             ropp_io_ascend.f90         \
	     ropp_io_assign.f90         \
             ropp_io_init.f90           \
             ropp_io_free.f90           \
             ropp_io_isinrange.f90      \
             ropp_io_nrec.f90           \
             ropp_io_occid.f90          \
             ropp_io_rangecheck.f90     \
             ropp_io_read.f90           \
             ropp_io_read_ncdf_get.f90  \
             ropp_io_shrink.f90         \
             ropp_io_vlist_print.f90    \
             ropp_io_vlist_size.f90     \
	     ropp_io_vlist_read.f90     \
             ropp_io_write.f90          \
             ropp_io_write_ncdf_def.f90 \
             ropp_io_write_ncdf_put.f90 

# 2. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@
