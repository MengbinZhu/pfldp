# Makefile.am for ropp_fm/tools
# =============================

# 1. Useful settings
# ------------------

SUFFIXES = .sh .pl

# 2. The ropp2ropp program
# ----------------------------

noinst_SCRIPTS = test_fm_GRAS.sh test_fm_2D.sh

if HAVE_ROPP_IO
bin_PROGRAMS     = t_fascod t_fascod_tl t_fascod_ad t_twodop t_twodtl t_twodad
t_fascod_SOURCES = t_fascod.f90
t_fascod_tl_SOURCES = t_fascod_tl.f90
t_fascod_ad_SOURCES = t_fascod_ad.f90
t_twodop_SOURCES = t_twodop.f90
t_twodtl_SOURCES = t_twodtl.f90
t_twodad_SOURCES = t_twodad.f90
endif

# 3. Libraries to link in
# -----------------------

LDADD = ../build/libropp_fm.a

# 4. Files to be cleaned
# ----------------------

if CM_FC_UC_MODNAMES
   MODULES = 
else
   MODULES = 
endif

CLEANFILES     = *.stb $(MODULES)
DISTCLEANFILES = *.dat *.nc *.cdl

# 5. Stuff to go into the distribution
# ------------------------------------

EXTRA_DIST = example_tests.log test_fm_GRAS.sh test_fm_2D.sh plot_fm.pro plot_fm_twod.pro close_jpgfile.pro ncdf_getvar.pro open_jpgfile.pro \
             ropp_fm_bgr20090401_single-example.jpg ECMWF_2D_test_output-example.jpg \
             ropp_fm_bgr20090401_comp_single-example.jpg ECMWF_2D_test_output_comp-example.jpg


# 6. Compilation rules for scripts (currently not needed, but maybe later)
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


# 7. Dependencies
# ---------------

t_fascod.$(OBJEXT) : t_fascod.f90
t_fascod_tl.$(OBJEXT) : t_fascod_tl.f90
t_fascod_tl.$(OBJEXT) : t_fascod_ad.f90
t_twodop.$(OBJEXT) : t_twodop.f90
t_twodtl.$(OBJEXT) : t_twodtl.f90
t_twodad.$(OBJEXT) : t_twodad.f90

# 8. Additional things to install
# -------------------------------

test: test_fm 

if HAVE_ROPP_IO
test_fm: 
	@ echo ""
	@ echo "Testing ropp_fm routine on 1D reference dataset..."
	@ echo ""
	@ ./t_fascod
	@ ./t_fascod_tl
	@ ./t_fascod_ad
	@ echo ""
	@ echo "Testing ropp_fm routine with -comp option on 1D reference dataset..."
	@ echo ""
	@ ./t_fascod -comp
	@ ./t_fascod_tl -comp
	@ ./t_fascod_ad -comp
	@ echo ""
	@ echo "Testing ropp_fm routine on 2D reference dataset..."
	@ ./t_twodop
	@ ./t_twodtl
	@ ./t_twodad
	@ echo ""
	@ echo "Testing ropp_fm routine with -comp option on 2D reference dataset..."
	@ ./t_twodop -comp
	@ ./t_twodtl -comp
	@ ./t_twodad -comp
	@ echo ""
	@ echo "Testing ropp_fm with 1D ECMWF background dataset..."
	@ ./test_fm_GRAS.sh
	@ echo ""
	@ echo "Testing ropp_fm with 2D ECMWF background dataset..."
	@ ./test_fm_2D.sh
	@ echo ""
else
test_fm: 
	 @ echo ""
	 @ echo "***********   ROPP_FM TESTING   ***********************"
	 @ echo ""
	 @ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	 @ echo "The ropp_fm test routine requires ropp_io and netcdf to be installed first. "
	 @ echo "See ROPP release notes for installation instructions."
	 @ echo ""
	 @ echo "NOTE: "
	 @ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	 @ echo "  ROPP_FM TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_FM TO "
	 @ echo "  THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	 @ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN " 
	 @ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details." 
	 @ echo ""
endif

