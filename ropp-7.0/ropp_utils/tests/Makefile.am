# Makefile.am for ropp_io/tests
# =============================

# 1. Useful settings
# ------------------

SUFFIXES = .sh .pl

# 2. The t_version program
# -----------------------

noinst_PROGRAMS   = t_version
t_version_SOURCES  = t_version.f90

# 3. Libraries to link in
# -----------------------

LDADD = ../build/libropp_utils.a

# 4. Files to be cleaned
# ----------------------

CLEANFILES     = *.stb $(MODULES)
DISTCLEANFILES = t_version

# 5. Stuff to go into the distribution
# ------------------------------------

EXTRA_DIST =

# 6. Compilation rules for scripts
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


# 7. Dependencies
# ---------------

t_version.$(OBJEXT) : t_version.f90

# 8. Additional things to do
# ---------------------------

test: test_version

test_version: ./t_version
	@ echo ""
	@ echo "Testing ROPP UTILS version..."
	@ ./t_version
