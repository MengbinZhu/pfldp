# Makefile.am for the ropp_pp data directory
# ==========================================

# 1. Extra files for the distribution
# -----------------------------------

EGM_FILES  = egm96.dat    corrcoef.dat
GZP_FILES  = egm96.dat.gz corrcoef.dat.gz

EXTRA_DIST = README                   \
             ropp_pp_test.nc          \
             ropp_pp_tph_test.nc      \
             ropp_pp_grasrs_test.nc   \
             MSIS_coeff.nc            \
             $(GZP_FILES)

# 2. Files to be cleaned
# ----------------------

# CLEANFILES =

# 3. Rules for compilation
# ------------------------

all:		unzip_files

install:	all

dist:		zip_files

egm96.dat.gz:		dist

corrcoef.dat.gz:	dist

## Unzip data on package build
unzip_files:;
	@for file in $(GZP_FILES); do \
	  if test -e $$file; then     \
	    echo "Uncompressing $$file..."; \
	    gunzip -f $$file;         \
	  fi; \
	done

## Zip data on distro build
zip_files:;
	@for file in $(EGM_FILES); do \
	  if test -e $$file; then     \
	    echo "Compressing $$file..."; \
	    gzip -f $$file;           \
	  fi; \
	done

