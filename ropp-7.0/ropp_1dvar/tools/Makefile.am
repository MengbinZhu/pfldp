# Makefile.am for ropp_1dvar/tools
# ================================

# 1. Main 1DVars
# ---------------------

SUFFIXES = .sh .pl

# 2. ropp_1dvar programs

bin_PROGRAMS = ropp_1dvar_refrac ropp_1dvar_bangle

if HAVE_ROPP_IO
ropp_1dvar_refrac_SOURCES = ropp_1dvar_refrac.f90
ropp_1dvar_bangle_SOURCES = ropp_1dvar_bangle.f90
man_MANS = ropp_1dvar_refrac.1 ropp_1dvar_bangle.1
endif

# 3. Libraries to link in
# -----------------------

LDADD = ../build/libropp_1dvar.a

# 4. Files to be cleaned
# ----------------------

MODULES = 

CLEANFILES     = *.stb $(MODULES)
DISTCLEANFILES = *.dat *.nc *.cdl

# 5. Stuff to go into the distribution
# ------------------------------------

EXTRA_DIST = ropp_1dvar_refrac.1 ropp_1dvar_bangle.1

# 6. Rules for compilation
# ------------------------

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

#.m4.f90:
#	$(M4) $(M4_OPTS) $< > $@

# 3. Dependencies
# ---------------

ropp_1dvar_refrac.$(OBJEXT) : ropp_1dvar_refrac.f90

ropp_1dvar_bangle.$(OBJEXT) : ropp_1dvar_bangle.f90


if HAVE_ROPP_IO

else
ropp_1dvar_refrac:
	@ echo ""
	@ echo "***********   ROPP_1DVAR_REFRAC TOOL   ***********************"
	@ echo ""
	@ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	@ echo "The ropp_1dvar stand-alone tool requires ropp_io and netcdf to be installed first. "
	@ echo "See ROPP release notes for installation instructions."
	@ echo ""
	@ echo "NOTE: "	
	@ echo "  ROPP_IO AND NETCDF LIBRARIES ARE ONLY REQUIRED FOR STAND-ALONE"
	@ echo "  ROPP_1DVAR TOOLS AND TEST ROUTINES. USERS WISHING TO LINK ROPP_1DVAR"
	@ echo "  TO THEIR OWN APPLICATIONS NOT REQUIRING INPUT/OUTPUT FUNCTIONS DO"
	@ echo "  NOT NEED TO INSTALL THESE ADDITIONAL LIBRARIES OTHER THAN TO RUN "
	@ echo "  STAND-ALONE TOOLS AND TESTING. See ROPP User Guide for more details."
	@ echo ""

ropp_1dvar_bangle:
	@ echo ""
	@ echo "***********   ROPP_1DVAR_BANGLE TOOL   ***********************"
	@ echo ""
	@ echo "Configure detects that the ROPP_IO library has not been correctly installed."
	@ echo "The ropp_1dvar stand-alone tool requires ropp_io and netcdf to be installed first. "
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
