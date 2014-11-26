# $Id: Makefile.am 3086 2011-10-14 08:54:50Z idculv $
#
# Makefile.am for ropp_io
# =======================

# 1. Useful settings
# ------------------

ACLOCAL_AMFLAGS = -I m4

# 2. Subdirectories
# -----------------

SUBDIRS = common ncdf build data doc ropp thin bufr tools tests contrib

# 3. Extra bits for the distribution
# -----------------------------------

EXTRA_DIST = AUTHORS COPYRIGHT README README.cygwin README.unix \
             ChangeLog MANIFEST

# 4. Generate ChangeLog/REVISION files automatically with make dist
# -----------------------------------------------------------------

dist-hook:
	if test -n "$(SVN2CL)"; then \
		$(SVN2CL) --include-rev --group-by-day -o $(distdir)/ChangeLog; \
	fi; \
        cp $(distdir)/ChangeLog $(top_srcdir)/ChangeLog; \
	if test -e ".svn/entries"; then \
		grep revision ".svn/entries"| tail -n1 | \
                sed s?/\>?? | sed s/" "//g | sed s/\"//g > $(distdir)/REVISION; \
	fi; \
        cp $(distdir)/REVISION $(top_srcdir)/REVISION



