# Makefile.am for ropp_fm/common
# ==============================

# 1. Common (for several forward models) routines of ropp_fm
# ----------------------------------------------------------

EXTRA_DIST = ropp_fm_constants.f90         \
             ropp_fm_interpol.f90          \
             ropp_fm_interpol_ad.f90       \
             ropp_fm_interpol_log.f90      \
             ropp_fm_interpol_log_ad.f90   \
             ropp_fm_interpol_log_tl.f90   \
             ropp_fm_interpol_tl.f90       \
             ropp_fm_roprof2state.f90      \
             ropp_fm_state2roprof.f90      \
             ropp_fm_state2state.f90       \
             ropp_fm_roprof2obs.f90        \
             ropp_fm_obs2roprof.f90        \
             ropp_fm_obs2obs.f90           \
             ropp_fm_set_units.f90         \
             ropp_fm_compress.f90          \
	     ropp_fm_compress_ad.f90       \
             ropp_fm_compress_tl.f90       \
             ropp_fm_compress_2d.f90       \
	     ropp_fm_compress_2d_ad.f90    \
             ropp_fm_compress_2d_tl.f90    \
             ropp_fm_version.f90
	     
# 2. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@
