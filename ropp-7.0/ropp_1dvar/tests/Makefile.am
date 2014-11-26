# Makefile.am for ropp_1dvar/tests
# ================================

# 1. Useful settings
# ------------------

SUFFIXES = .sh .pl

SUBDIRS = 

# 2. Some scripts for testing
# ---------------------------

noinst_SCRIPTS = test_1dvar.sh

CLEANFILES     = *.stb $(MODULES)
DISTCLEANFILES = *.dat *.nc *_ba.jpg *_ref.jpg

# 3. Stuff to go into the distribution
# ------------------------------------

EXTRA_DIST = test_1dvar.sh close_jpgfile.pro klegend_d.pro   \
             plot_1dvar.pro ncdf_getvar.pro open_jpgfile.pro \
             example_tests.log example_tests.jpg test_1dvar_MO.sh  \
             test_1dvar_GRAS.sh ropp_1dvar_comp_01_ref_example.jpg \
             ropp_1dvar_comp_04_ba_example.jpg ropp_1dvar_comp_04_ref_example.jpg \
             ropp_1dvar_comp_05_ba_example.jpg ropp_1dvar_comp_05_ref_example.jpg \
             ropp_1dvar_comp_04comp_ba_example.jpg ropp_1dvar_comp_04comp_ref_example.jpg \
             ropp_1dvar_comp_05comp_ba_example.jpg ropp_1dvar_comp_05comp_ref_example.jpg \
             ropp_1dvar_comp_01comp_ref_example.jpg

# 4. Compilation rules for scripts (currently not needed, but maybe later)
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

# 10. Additional things to do
# ---------------------------

test: test_1dvar

if HAVE_ROPP_IO
test_1dvar: ./test_1dvar.sh
	@ echo ""
	@ echo "Testing basic 1D-Var functionality..."
	@ echo ""
	@ ./test_1dvar.sh
	@ echo ""
	@ echo "Testing 1dVar using GRAS observations with co-located ECMWF background"
	@ echo ""
	@ ./test_1dvar_GRAS.sh
	@ echo ""
	@ echo "Testing 1dVar using GRAS observations with co-located Met Office background"
	@ echo ""
	@ ./test_1dvar_MO.sh
	@ echo ""
else

test_1dvar:
	@ echo ""
	@ echo "***********   ROPP_1DVAR TESTING   ***********************"
	@ echo ""
	@ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	@ echo "The ropp_1dvar test routine requires ropp_io and netcdf to be installed first. "
	@ echo "See ROPP release notes for installation instructions."
	@ echo ""
	@ echo "NOTE: "	
	@ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	@ echo "  ROPP_1DVAR TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_1DVAR"
	@ echo "  TO THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	@ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	@ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	@ echo ""
endif

