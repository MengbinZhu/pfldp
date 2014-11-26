# Makefile.am for ropp_io/bufr
# =============================

# 1. Useful settings
# ------------------

SUFFIXES = .sh .pl


# 5. Files to be cleaned
# ----------------------

CLEANFILES     = *.stb *.mod
DISTCLEANFILES = *.dat *.nc *.cdl *.bfr *.bufr

# 6. Stuff to go into the distribution
# ------------------------------------

EXTRA_DIST = roppbufrcodes.nl

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

if HAVE_ECMWF_BUFR
install-data-local:
	@if test x$$BUFR_TABLES = x ; then                                         \
	   echo "" ;                                                               \
	   echo "WARNING: required run-time files can not be installed because"  ; \
	   echo "         the environment variable BUFR_TABLES is not set."      ; \
	   echo "" ;                                                               \
	   echo "         Please manually install bufr/roppbufrcodes.nl in the"  ; \
	   echo "         run-time directory for your ECMWF BUFR library."       ; \
	   echo "" ;                                                               \
	   echo "         In order to run and/or test the BUFR programs, the" ;    \
	   echo "         environment variable BUFR_TABLES must also be set"  ;    \
	   echo "         properly." ;                                             \
	   echo "" ;                                                               \
	else                                                                       \
	   echo $(INSTALL_DATA) roppbufrcodes.nl $(BUFR_TABLES)/roppbufrcodes.nl ; \
	   $(INSTALL_DATA) roppbufrcodes.nl $(BUFR_TABLES)/roppbufrcodes.nl ;      \
	fi ;

uninstall-local:
	if test -f $(BUFR_TABLES)/roppbufrcodes.nl ; then \
	   rm -f $(BUFR_TABLES)/roppbufrcodes.nl ;        \
	fi ;
endif

if HAVE_METO_BUFR
install-data-local:
	@if test x$$BUFR_LIBRARY = x ; then                                        \
	   echo "" ;                                                               \
	   echo "WARNING: required run-time files can not be installed because"  ; \
	   echo "         the environment variable BUFR_LIBRARY is not set."     ; \
	   echo "" ;                                                               \
	   echo "         Please manually install bufr/roppbufrcodes.nl in the"  ; \
	   echo "         run-time directory for your Met Office BUFR library."  ; \
	   echo "" ;                                                               \
	   echo "         In order to run and/or test the bufr programs, the" ;    \
	   echo "         environment variable BUFR_LIBRARY must also be set" ;    \
	   echo "         properly." ;                                             \
	   echo "" ;                                                               \
	else                                                                       \
	   echo $(INSTALL_DATA) roppbufrcodes.nl $(BUFR_LIBRARY)/roppbufrcodes.nl ;\
	   $(INSTALL_DATA) roppbufrcodes.nl $(BUFR_LIBRARY)/roppbufrcodes.nl ;     \
	fi ;

uninstall-local:
	if test -f $(BUFR_LIBRARY)/roppbufrcodes.nl ; then \
	   rm -f $(BUFR_LIBRARY)/roppbufrcodes.nl ;        \
	fi ;
endif
