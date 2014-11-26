# Makefile.am for ropp_fm/model_ecmwf
# ===================================

# 1. Common (for several forward models) routines of ropp_fm
# ----------------------------------------------------------

EXTRA_DIST = ropp_fm_state2state_ecmwf.f90         \
	     ropp_fm_state2state_ecmwf_ad.f90      \
	     ropp_fm_state2state_ecmwf_tl.f90      
             
# 2. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@
