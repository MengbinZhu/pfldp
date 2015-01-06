AM_FCFLAGS = -I $(top_builddir)/src

TESTS_ENVIRONMENT = $(srcdir)/runtest.sh
EXTRA_DIST = runtest.sh
TESTS = \
	hsl_ea20ds

check_PROGRAMS = \
	hsl_ea20ds

hsl_ea20ds_SOURCES = hsl_ea20ds.f90
hsl_ea20ds_LDADD = $(top_builddir)/src/libhsl_ea20.a $(LAPACK_LIBS) $(BLAS_LIBS) $(METIS_LIBS) $(FCLIBS)
EXTRA_DIST += hsl_ea20ds.output
