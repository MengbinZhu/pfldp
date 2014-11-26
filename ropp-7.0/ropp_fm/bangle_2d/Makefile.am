# Makefile.am for ropp_fm/bangle_2d
# =================================

# 1. 1D forward model for refractivity
# ------------------------------------

EXTRA_DIST = ropp_fm_alpha2drk.f90         \
             ropp_fm_alpha2drk_ad.f90      \
             ropp_fm_alpha2drk_tl.f90      \
	     ropp_fm_gpspderivs.f90        \
	     ropp_fm_gpspderivs_ad.f90     \
	     ropp_fm_gpspderivs_tl.f90     \
             ropp_fm_bangle_2d.f90         \
             ropp_fm_bangle_2d_ad.f90      \
             ropp_fm_bangle_2d_tl.f90      \
             ropp_fm_2d_plane.f90    


# 2. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@
