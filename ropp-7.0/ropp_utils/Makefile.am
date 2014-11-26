# $Id: Makefile.am 3696 2013-06-17 08:48:37Z idculv $
#
# Top level Makefile.am for ropp_utils
# ====================================

# 1. Useful settings
# ------------------

ACLOCAL_AMFLAGS = -I m4

# 2. Subdirectories
# -----------------

SUBDIRS = common arrays compilers datetime doc geodesy \
          coordinates ropp_messages unitconvert build tests

# 3. Extra bits for the distribution
# -----------------------------------

EXTRA_DIST = AUTHORS COPYRIGHT README README.cygwin README.unix \
             ChangeLog REVISION MANIFEST

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

