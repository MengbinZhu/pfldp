# Makefile.am for ropp_io/tools
# =============================

# 1. Useful settings
# ------------------

SUFFIXES = .sh .pl

# 2a. The test2ropp program
# ----------------------------

bin_PROGRAMS      = test2ropp
test2ropp_SOURCES = test2ropp.f90
man_MANS          = test2ropp.1

# 2b. The ropp2ropp program
# ----------------------------

bin_PROGRAMS     += ropp2ropp
ropp2ropp_SOURCES = ropp2ropp.f90
man_MANS         += ropp2ropp.1

# 2c The gfz2ropp program
# ----------------------------

bin_PROGRAMS    += gfz2ropp
gfz2ropp_SOURCES = gfz2ropp.f90
man_MANS        += gfz2ropp.1

# 2d The ucar2ropp program
# ----------------------------

bin_PROGRAMS     += ucar2ropp
ucar2ropp_SOURCES = ucar2ropp.f90
man_MANS         += ucar2ropp.1

# 2e The bgrasc2ropp program
# ----------------------------

bin_PROGRAMS     += bgrasc2ropp
bgrasc2ropp_SOURCES = bgrasc2ropp.f90
man_MANS         += bgrasc2ropp.1

# 2f The grib2bgrasc program
# ----------------------------

if HAVE_GRIB
bin_PROGRAMS     += grib2bgrasc
grib2bgrasc_SOURCES = grib2bgrasc.f90
man_MANS         += grib2bgrasc.1
endif

# 2g The eum2ropp program
# ----------------------------

if HAVE_HDF5
bin_PROGRAMS     += eum2ropp
eum2ropp_SOURCES  = eum2ropp.f90
man_MANS         += eum2ropp.1
endif


# 2. Bufr programs
# ----------------

if HAVE_ECMWF_BUFR
bin_PROGRAMS     += bufr2ropp ropp2bufr
bufr2ropp_SOURCES = bufr2ropp_ec.f90
ropp2bufr_SOURCES = ropp2bufr_ec.f90
man_MANS         += bufr2ropp.1 ropp2bufr.1
if HAVE_HDF5
bin_PROGRAMS     += eum2bufr
eum2bufr_SOURCES  = eum2bufr_ec.f90
man_MANS         += eum2bufr.1
endif
endif

if HAVE_METO_BUFR
bin_PROGRAMS     += bufr2ropp ropp2bufr
bufr2ropp_SOURCES = bufr2ropp_mo.f90
ropp2bufr_SOURCES = ropp2bufr_mo.f90
endif

# 4. Libraries to link in
# -----------------------

LDADD = ../build/libropp_io.a

# 5. Files to be cleaned
# ----------------------

CLEANFILES     = *.stb *.mod
DISTCLEANFILES = *.dat *.nc *.cdl *.bfr *.bufr

# 6. Stuff to go into the distribution
# ------------------------------------

EXTRA_DIST = test2ropp.1 ropp2ropp.1 ropp2bufr.1 bufr2ropp.1 gfz2ropp.1 ucar2ropp.1 bgrasc2ropp.1 grib2bgrasc.1 eum2ropp.1 eum2bufr.1

# 8. Compilation rules for scripts (currently not needed, but maybe later)
# ------------------------------------------------------------------------

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
	rm -f $@ $@.tmp
	$(editsh) $< >$@.tmp
	chmod +x $@.tmp
	mv $@.tmp $@

.pl:
	rm -f $@ $@.tmp
	$(editpl) $< >$@.tmp
	chmod +x $@.tmp
	mv $@.tmp $@

# 10. Additional things to install
# --------------------------------
