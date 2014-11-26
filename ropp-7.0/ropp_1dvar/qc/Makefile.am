# Makefile.am for ropp_1dvar/qc
# =============================

# 1. ECMWF based 1DVar
# --------------------

EXTRA_DIST = ropp_qc.f90 ropp_qc_genqc.f90 ropp_qc_omb.f90 \
             ropp_qc_bgqc.f90 ropp_qc_pge.f90 ropp_qc_cutoff.f90

# 2. Rules for compilation
# ------------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@
