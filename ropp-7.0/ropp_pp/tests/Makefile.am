# Makefile.am for ropp_pp/tests
# =============================

# 1. Useful settings
# ------------------

SUFFIXES = .sh .pl

# 2. The t_ropp_pp program
# -----------------------

if HAVE_ROPP_IO
noinst_PROGRAMS = ropp_test_abel
ropp_test_abel_SOURCES = ropp_test_abel.f90
endif

# 3. Scripts for testing
# ----------------------

noinst_SCRIPTS = t_ropp_pp

# 5. Libraries to link in
# -----------------------

LDADD = ../build/libropp_pp.a

# 6. Files to be cleaned
# ----------------------

CLEANFILES     = *.stb $(MODULES)
DISTCLEANFILES = t_ropp_pp ropp_test_abel *.cdl  *.nc  

# 7. Stuff to go into the distribution
# ------------------------------------

EXTRA_DIST = t_ropp_pp.sh it_pp_01.pro ncdf_attget.pro ncdf_getvar.pro  \
	     it_pp_spectra_dt.pro it_pp_spectra_ep.pro open_jpgfile.pro \
	     close_jpgfile.pro \
	     ropp_pp_comp1-example.jpg \
	     ropp_pp_comp2-example.jpg \
	     ropp_pp_spectra_dt-example.eps \
	     ropp_pp_spectra_ep-example.eps \
	     ropp_pp_spectra_dt-example.jpg \
	     ropp_pp_spectra_ep-example.jpg

# 8. Compilation rules for scripts
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
	rm -f $@ $@.tmp
	$(editsh) $< >$@.tmp
	chmod +x $@.tmp
	mv $@.tmp $@

.pl:
	rm -f $@ $@.tmp
	$(editpl) $< >$@.tmp
	chmod +x $@.tmp
	mv $@.tmp $@

# 9. Dependencies
# ---------------

ropp_test_abel.$(OBJEXT) : ropp_test_abel.f90

# 10. Additional things to do
# ---------------------------

test: test_ropp_pp test_ropp_abel

if HAVE_ROPP_IO
test_ropp_pp: ./t_ropp_pp
	@ echo ""
	@ echo "Testing ROPP PP tool..."
	@ echo ""
	@ ./t_ropp_pp 
	@ echo ""

test_ropp_abel: ./ropp_test_abel
	@ echo ""
	@ echo "Testing ROPP PP Abel..."
	@ echo ""
	@ ./ropp_test_abel
	@ echo ""	

else
test_ropp_pp:
	@ echo ""
	@ echo "***********   ROPP_PP TESTING   ***********************"
	@ echo ""
	@ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	@ echo "The ropp_pp test routine requires ropp_io and netcdf to be installed first. "
	@ echo "See ROPP release notes for installation instructions."
	@ echo ""
	@ echo "NOTE: "
	@ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	@ echo "  ROPP_PP TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_PP TO "
	@ echo "  THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	@ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	@ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	@ echo ""

test_ropp_abel:
	@ echo ""
	@ echo "***********   ROPP_PP TESTING   ***********************"
	@ echo ""
	@ echo "Configure detects that the ROPP_IO library has not been correctly installed."	
	@ echo "The ropp_pp test routine requires ropp_io and netcdf to be installed first. "
	@ echo "See ROPP release notes for installation instructions."
	@ echo ""
	@ echo "NOTE: "
	@ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	@ echo "  ROPP_PP TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_PP TO "
	@ echo "  THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	@ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	@ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	@ echo ""

endif

