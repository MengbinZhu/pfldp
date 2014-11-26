# Makefile.am for ropp_io/tests
# =============================

# 1. Useful settings
# ------------------

SUFFIXES = .sh .pl

# 2. The t_netcdf program
# -----------------------

noinst_PROGRAMS   = t_netcdf
t_netcdf_SOURCES  = t_netcdf.f90

# 3. The nml_diff program
# -----------------------

noinst_PROGRAMS  += nml_diff
nml_diff_SOURCES  = nml_diff.f90

# 4. The nc_diff program
# ----------------------

noinst_PROGRAMS   += nc_diff
nc_diff_SOURCES  = nc_diff.f90

# 5. Some scripts for testing
# ---------------------------

noinst_SCRIPTS = t_ropp2ropp t_ucar2ropp t_gfz2ropp t_bgrasc2ropp

if HAVE_HDF5
   noinst_SCRIPTS += t_eum2ropp
endif

# 6. Bufr programs
# ----------------

if HAVE_ECMWF_BUFR
   noinst_SCRIPTS += t_roppbufr
if HAVE_HDF5
   noinst_SCRIPTS += t_eumbufr
endif
endif

if HAVE_METO_BUFR
   noinst_SCRIPTS += t_roppbufr
endif

# 7. Grib programs
# ----------------

if HAVE_GRIB
   noinst_SCRIPTS += t_grib2bgrasc
endif

# 8. Libraries to link in
# -----------------------

LDADD = ../build/libropp_io.a

# 9. Files to be cleaned
# ----------------------

CLEANFILES     = *.stb $(MODULES)
DISTCLEANFILES = t_ropp2ropp t_roppbufr t_gfz2ropp t_ucar2ropp \
                 t_grib2bgrasc t_bgrasc2ropp \
                 t_eum2ropp t_eumbufr \
                 *.nc *.bufr *.bnc *.dat *.cdl t_ropp2ropp.log

# 10. Stuff to go into the distribution
# -------------------------------------

EXTRA_DIST = t_ropp2ropp.sh t_roppbufr.sh t_gfz2ropp.sh t_ucar2ropp.sh \
             t_grib2bgrasc.sh t_bgrasc2ropp.sh \
             t_eum2ropp.sh t_eumbufr.sh \
             nml_diff.f90 nc_diff.f90 example_test.log

# 11. Compilation rules for scripts
# ---------------------------------

editsh = sed \
	-e 's,@''etcdir''@,$(etcdir),g' \
	-e 's,@''datadir''@,$(datadir),g' \
	-e 's,@''pkgdatadir''@,$(pkgdatadir),g' \
	-e 's,@''defaultcenter''@,$(defaultcenter),g' \
	-e 's,@''M4''@,$(M4),g' \
	-e 's,@''AWK''@,$(AWK),g' \
	-e 's,@''SHELL''@,$(SHELL),g' \
	-e 's,@''VERSION''@,$(VERSION),g' \
	-e 's,@''PACKAGE''@,$(PACKAGE),g'

editpl = sed \
	-e 's,@''etcdir''@,$(etcdir),g' \
	-e 's,@''datadir''@,$(datadir),g' \
	-e 's,@''pkgdatadir''@,$(pkgdatadir),g' \
	-e 's,@''perldir''@,$(perldir),g' \
	-e 's,@''defaultcenter''@,$(defaultcenter),g' \
	-e 's,@''PERL''@,$(PERL),g' \
	-e 's,@''VERSION''@,$(VERSION),g' \
	-e 's,@''PACKAGE''@,$(PACKAGE),g'

.sh:
	@rm -f $@ $@.tmp
	@$(editsh) $< >$@.tmp
	@chmod +x $@.tmp
	@mv $@.tmp $@

.pl:
	@rm -f $@ $@.tmp
	@$(editpl) $< >$@.tmp
	@chmod +x $@.tmp
	@mv $@.tmp $@

# 12. Dependencies
# ----------------

t_netcdf.$(OBJEXT) : t_netcdf.f90

nml_diff.$(OBJEXT) : nml_diff.f90

nc_diff.$(OBJEXT) : nc_diff.f90

# 13. Additional things to do
# ---------------------------

test: test_netcdf test_ropp test_bufr test_gfz test_ucar test_grib test_bgrasc test_eum

test_netcdf: ./t_netcdf
	@ echo ""
	@ echo "Testing netCDF Fortran 90 write interfaces..."
	@ echo ""
	@ ./t_netcdf
	@ echo ""

test_ropp: ./t_ropp2ropp
	@ echo ""
	@ echo "Testing ROPP conversion tool..."
	@ echo ""
	@ ./t_ropp2ropp -r -n -m -t
	@ echo ""

test_bufr:	test_ecbufr test_mobufr test_eumbufr

if HAVE_ECMWF_BUFR
test_ecbufr: ./t_roppbufr
	@ echo ""
	@ echo "Testing BUFR encode/decode [ECMWF library]..."
	@ echo ""
	@ ./t_roppbufr
	@ echo ""
if HAVE_HDF5
test_eumbufr: ./t_eumbufr
	@ echo ""
	@ echo "Testing BUFR encode/decode [EUM nc4 files]..."
	@ echo ""
	@ ./t_eumbufr
	@ echo ""
else
test_eumbufr:;	@ echo ""
endif
else
test_ecbufr:;	@ echo ""
test_eumbufr:;	@ echo ""
endif

if HAVE_METO_BUFR
test_mobufr: ./t_roppbufr
	@ echo ""
	@ echo "Testing BUFR encode/decode [MetDB library]..."
	@ echo ""
	@ ./t_roppbufr
	@ echo ""
else
test_mobufr:; @ echo ""
endif

test_gfz: ./t_gfz2ropp
	@ echo ""
	@ echo "Testing GFZ-to-netCDF tool..."
	@ echo ""
	@ ./t_gfz2ropp
	@ echo ""

test_ucar: ./t_ucar2ropp
	@ echo ""
	@ echo "Testing UCAR-to-ROPP netCDF tool..."
	@ echo ""
	@ ./t_ucar2ropp
	@ echo ""

if HAVE_GRIB
test_grib: ./t_grib2bgrasc
	@ echo ""
	@ echo "Testing GRIB decode [ECMWF library]..."
	@ echo ""
	@ ./t_grib2bgrasc
	@ echo ""
else
test_grib:; @ echo ""
endif

test_bgrasc: ./t_bgrasc2ropp

	@ echo ""
	@ echo "Testing BGRASC-to-ROPP netCDF tool..."
	@ echo ""
	@ ./t_bgrasc2ropp
	@ echo ""

if HAVE_HDF5
test_eum: ./t_eum2ropp
	@ echo ""
	@ echo "Testing EUM nc4 conversion tool..."
	@ echo ""
	@ ./t_eum2ropp
	@ echo ""
else
test_eum:;	@ echo ""
endif
