# Makefile.am for ropp_fm/refrac_1d
# =================================

# 1. 1D forward model for refractivity
# ------------------------------------

EXTRA_DIST = ropp_fm_refrac_1d.f90      \
             ropp_fm_refrac_1d_ad.f90   \
             ropp_fm_refrac_1d_tl.f90   \
             ropp_fm_refrac_1d_grad.f90


# 2. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@
