# Makefile.am for ropp_pp/tools
# =============================

# 1. Useful settings
# ------------------

SUFFIXES = .sh .pl

# 2. The ropp2ropp program
# ----------------------------

bin_PROGRAMS        = ropp_pp_abel_tool    \
	              ropp_pp_occ_tool     \
	              ropp_pp_invert_tool  \
	              ropp_pp_spectra_tool \
	              ropp_pp_grasrs2ropp  \
	              ropp_pp_tph_tool

if HAVE_ROPP_IO
ropp_pp_abel_tool_SOURCES = ropp_pp_abel_tool.f90
ropp_pp_occ_tool_SOURCES = ropp_pp_occ_tool.f90
ropp_pp_invert_tool_SOURCES = ropp_pp_invert_tool.f90
ropp_pp_spectra_tool_SOURCES = ropp_pp_spectra_tool.f90
ropp_pp_grasrs2ropp_SOURCES = ropp_pp_grasrs2ropp.f90
ropp_pp_tph_tool_SOURCES = ropp_pp_tph_tool.f90
man_MANS = ropp_pp_abel_tool.1 ropp_pp_occ_tool.1 ropp_pp_invert_tool.1 \
           ropp_pp_spectra_tool.1 ropp_pp_grasrs2ropp.1 ropp_pp_tph_tool.1
endif


# 3. Libraries to link in
# -----------------------

LDADD = ../build/libropp_pp.a

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

EXTRA_DIST = ropp_pp_abel_tool.1 ropp_pp_occ_tool.1 ropp_pp_invert_tool.1 \
             ropp_pp_spectra_tool.1 ropp_pp_grasrs2ropp.1 ropp_pp_tph_tool.1


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

ropp_pp_abel_tool.$(OBJEXT)    : ropp_pp_abel_tool.f90
ropp_pp_occ_tool.$(OBJEXT)     : ropp_pp_occ_tool.f90
ropp_pp_invert_tool.$(OBJEXT)  : ropp_pp_invert_tool.f90
ropp_pp_spectra_tool.$(OBJEXT) : ropp_pp_spectra_tool.f90
ropp_pp_grasrs2ropp.$(OBJEXT)  : ropp_pp_grasrs2ropp.f90
ropp_pp_tph_tool.$(OBJEXT)     : ropp_pp_tph_tool.f90

# 8. Additional things to install
# -------------------------------

if HAVE_ROPP_IO

else
ropp_pp_abel_tool:
	 @ echo ""
	 @ echo "***********   ROPP_PP ABEL TOOL   ***********************"
	 @ echo ""
	 @ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	 @ echo "The ropp_pp stand-alone tool requires ropp_io and netcdf to be installed first. "
	 @ echo "See ROPP Release Notes for installation instructions."
	 @ echo ""
	 @ echo "NOTE: "
	 @ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	 @ echo "  ROPP_PP TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_PP TO "
	 @ echo "  THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	 @ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	 @ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	 @ echo ""

ropp_pp_occ_tool:
	 @ echo ""
	 @ echo "***********   ROPP_PP OCC TOOL   ***********************"
	 @ echo ""
	 @ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	 @ echo "The ropp_pp stand-alone tool requires ropp_io and netcdf to be installed first. "
	 @ echo "See ROPP Release Notes for installation instructions."
	 @ echo ""
	 @ echo "NOTE: "
	 @ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	 @ echo "  ROPP_PP TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_PP TO "
	 @ echo "  THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	 @ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	 @ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	 @ echo ""

ropp_pp_invert_tool:
	 @ echo ""
	 @ echo "***********   ROPP_PP INVERT TOOL   ***********************"
	 @ echo ""
	 @ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	 @ echo "The ropp_pp stand-alone tool requires ropp_io and netcdf to be installed first. "
	 @ echo "See ROPP Release Notes for installation instructions."
	 @ echo ""
	 @ echo "NOTE: "
	 @ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	 @ echo "  ROPP_PP TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_PP TO "
	 @ echo "  THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	 @ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	 @ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	 @ echo ""

ropp_pp_spectra_tool:
	 @ echo ""
	 @ echo "***********   ROPP_PP SPECTRA TOOL   ***********************"
	 @ echo ""
	 @ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	 @ echo "The ropp_pp stand-alone tool requires ropp_io and netcdf to be installed first. "
	 @ echo "See ROPP Release Notes for installation instructions."
	 @ echo ""
	 @ echo "NOTE: "
	 @ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	 @ echo "  ROPP_PP TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_PP TO "
	 @ echo "  THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	 @ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	 @ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	 @ echo ""

ropp_pp_grasrs2ropp:
	 @ echo ""
	 @ echo "***********   ROPP_PP GRASRS2ROPP TOOL   ***********************"
	 @ echo ""
	 @ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	 @ echo "The ropp_pp stand-alone tool requires ropp_io and netcdf to be installed first. "
	 @ echo "See ROPP Release Notes for installation instructions."
	 @ echo ""
	 @ echo "NOTE: "
	 @ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	 @ echo "  ROPP_PP TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_PP TO "
	 @ echo "  THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	 @ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	 @ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	 @ echo ""

ropp_pp_tph_tool:
	 @ echo ""
	 @ echo "***********   ROPP_PP TPH TOOL   ***********************"
	 @ echo ""
	 @ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	 @ echo "The ropp_pp stand-alone tool requires ropp_io and netcdf to be installed first. "
	 @ echo "See ROPP Release Notes for installation instructions."
	 @ echo ""
	 @ echo "NOTE: "
	 @ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	 @ echo "  ROPP_PP TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_PP TO "
	 @ echo "  THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	 @ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	 @ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	 @ echo ""

endif
