## Process the file with automake to create Makefile.in.

SUFFIXES = .m4

# 1. Extra files for the distribution
# -----------------------------------

EXTRA_DIST = unitconvert.f90 ut_convert.f90 unitconvert_aux.m4   \
		unitconvert.m4 ut_convert.m4                     \
		ut_convert_scode.m4 ut_convert_acode.m4          \
		ut_convert_sitfc.m4 ut_convert_aitfc.m4

# 2. Files to be cleaned
# ----------------------

# CLEANFILES =

# 3. Rules for compilation
# ------------------------

all: unitconvert.f90 ut_convert.f90

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@

# Dependencies
# ------------

unitconvert.f90 :: unitconvert.m4 ut_convert_sitfc.m4 ut_convert_aitfc.m4
ut_convert.f90  :: ut_convert.m4 ut_convert_scode.m4 ut_convert_acode.m4
