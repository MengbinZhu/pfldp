# $Id: $
#
# Makefile.am for ropp_pp
# =======================

# 1. Useful settings
# ------------------

ACLOCAL_AMFLAGS = -I m4

# 2. Subdirectories
# -----------------

SUBDIRS = abel_invert bangle build common config data doc icorr \
          msis_bangle preprocess tph tests tools 

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
	fi; 
	if test -e "data/egm96.dat"; then \
		gzip -f data/egm96.dat;     \
	fi; \
	if test -e "data/corrcoef.dat"; then \
	gzip -f data/corrcoef.dat;           \
	fi;


