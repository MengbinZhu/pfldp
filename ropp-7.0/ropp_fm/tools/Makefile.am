# Makefile.am for ropp_fm/tools
# =============================

# 1. Useful settings
# ------------------

SUFFIXES = .sh .pl

# 2. The ropp2ropp program
# ----------------------------


bin_PROGRAMS = ropp_fm_bg2ro_1d ropp_fm_bg2ro_2d

if HAVE_ROPP_IO
ropp_fm_bg2ro_1d_SOURCES = ropp_fm_bg2ro_1d.f90
ropp_fm_bg2ro_2d_SOURCES = ropp_fm_bg2ro_2d.f90
man_MANS = ropp_fm_bg2ro_1d.1 ropp_fm_bg2ro_2d.1
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

EXTRA_DIST = ropp_fm_bg2ro_1d.1 ropp_fm_bg2ro_2d.1

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

ropp_fm_bg2ro_1d.$(OBJEXT) : ropp_fm_bg2ro_1d.f90
ropp_fm_bg2ro_2d.$(OBJEXT) : ropp_fm_bg2ro_2d.f90

# 8. Additional things to install
# -------------------------------

if HAVE_ROPP_IO

else
ropp_fm_bg2ro_1d:
	 @ echo ""
	 @ echo "***********   ROPP_FM_BG2RO_1D TOOL   ***********************"
	 @ echo ""
	 @ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	 @ echo "The ropp_fm stand-alone tool requires ropp_io and netcdf to be installed first. "
	 @ echo "See ROPP Release Notes for installation instructions."
	 @ echo ""
	 @ echo "NOTE: "
	 @ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	 @ echo "  ROPP_FM TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_FM TO "
	 @ echo "  THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	 @ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	 @ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	 @ echo ""
ropp_fm_bg2ro_2d:
	 @ echo ""
	 @ echo "***********   ROPP_FM_BG2RO_2D TOOL   ***********************"
	 @ echo ""
	 @ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	 @ echo "The ropp_fm stand-alone tool requires ropp_io and netcdf to be installed first. "
	 @ echo "See ROPP Release Notes for installation instructions."
	 @ echo ""
	 @ echo "NOTE: "
	 @ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	 @ echo "  ROPP_FM TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_FM TO "
	 @ echo "  THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	 @ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	 @ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	 @ echo ""
endif
