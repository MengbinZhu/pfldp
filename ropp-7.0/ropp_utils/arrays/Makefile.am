# Makefile.am for the ropp_utils/arrays directory
# ===============================================

SUFFIXES = .m4

# Things to put into the distribution
# -----------------------------------

noinst_SCRIPTS = arrays.f90 copy_alloc.f90 callocate.f90 reallocate.f90 \
  preallocate.f90 reverse.f90

EXTRA_DIST = arrays.f90 arrays.m4 auxiliary.m4 iminloc.f90 imaxloc.f90    \
  callocate.f90 callocate.m4 callocate_itfc.m4 callocate_code.m4          \
  isinrange.f90 isinrange.m4 isinrange_sitfc.m4 isinrange_scode.m4        \
  isinrange_aitfc.m4 isinrange_acode.m4                                   \
  reallocate.f90 reallocate.m4 reallocate_itfc.m4 reallocate_code.m4      \
  preallocate.f90 preallocate.m4 preallocate_itfc.m4 preallocate_code.m4  \
  reverse.f90 reverse.m4 reverse_itfc.m4 reverse_code.m4 locate.f90       \
  blend.f90 sort.f90 uniq.f90 where.f90 setminus.f90 nruns.f90 getrun.f90 \
  cross_product.f90 outer_product.f90 outer_and.f90 l2norm.f90            \
  unit_vector.f90 swap.f90                                                \
  copy_alloc.f90 copy_alloc.m4 copy_alloc_itfc.m4 copy_alloc_code.m4

# Rules for compilation
# ---------------------

.m4.f90:
	$(M4) $(M4_OPTS) $< > $@

all: arrays.f90 copy_alloc.f90 callocate.f90 isinrange.f90 reallocate.f90 preallocate.f90 reverse.f90

arrays.f90: arrays.m4 auxiliary.m4 copy_alloc_itfc.m4 reallocate_itfc.m4 preallocate_itfc.m4 reverse_itfc.m4
	$(M4) $(M4_OPTS) arrays.m4 > $@

copy_alloc.f90: copy_alloc.m4 auxiliary.m4 copy_alloc_code.m4
	$(M4) $(M4_OPTS) copy_alloc.m4 > $@

callocate.f90: callocate.m4 auxiliary.m4 callocate_code.m4
	$(M4) $(M4_OPTS) callocate.m4 > $@

isinrange.f90: isinrange.m4 auxiliary.m4 isinrange_sitfc.m4 isinrange_scode.m4 isinrange_aitfc.m4 isinrange_acode.m4
	$(M4) $(M4_OPTS) isinrange.m4 > $@

reallocate.f90: reallocate.m4 auxiliary.m4 reallocate_code.m4
	$(M4) $(M4_OPTS) reallocate.m4 > $@

preallocate.f90: preallocate.m4 auxiliary.m4 preallocate_code.m4
	$(M4) $(M4_OPTS) preallocate.m4 > $@

reverse.f90: reverse.m4 auxiliary.m4 reverse_code.m4
	$(M4) $(M4_OPTS) reverse.m4 > $@

# Dependencies
# ------------
