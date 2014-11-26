# Makefile.am for ropp_utils reference documentation
# ==================================================

# 1. Subdirectories
# -----------------

# SUBDIRS =

# 1.a Test programs
# -----------------

# pkgdata_DATA =

# noinst_DATA  =

# 2. Files to clean
# -----------------

CLEANFILES           = html/* *.ps *.dvi *.pdf *.aux \
                       *.idx *.ilg *.ind *.log *.toc
MAINTAINERCLEANFILES = rm_utils.tex

# 3. Files for distribution
# -------------------------

EXTRA_DIST = robodoc.rc             \
             rm_intro.tex           \
             rm_utils.tex           \
             rm_copyright.tex       \
             romsaf_ropp_utils_rm.tex \
             romsaf_ropp_utils_rm.dvi \
             romsaf_ropp_utils_rm.pdf

# 4. Rules
# --------

all:

html:     html/index.html

refman:   romsaf_ropp_utils_rm.pdf

html/index.html: ../arrays/*.f90 ../common/*.f90 ../compilers/*.f90 \
                 ../coordinates/*.f90 ../datetime/*.f90 ../geodesy/*.f90 \
                 ../ropp_messages/*.f90 ../unitconvert/*.f90
	mkdir -p mywork html
	(cd mywork; ln -s ../../arrays/*.f90 .)
	(cd mywork; ln -s ../../common/*.f90 .)
	(cd mywork; ln -s ../../compilers/*.f90 .)
	(cd mywork; ln -s ../../datetime/*.f90 .)
	(cd mywork; ln -s ../../geodesy/*.f90 .)
	(cd mywork; ln -s ../../coordinates/*.f90 .)
	(cd mywork; ln -s ../../unitconvert/*.f90 .)
	(cd mywork; ln -s ../../ropp_messages/*.f90 .)
	$(ROBODOC) --src ./mywork --doc ./html --multidoc --index --toc --html
	(cd html ; cat index.html | grep -v masterindex.html | grep -v robo_sourcefiles.html > tmp.html ; mv tmp.html index.html )
	(cd html ; rm -f masterindex.html robo_sourcefiles.html )
	rm -fr mywork

rm_utils.tex : ../arrays/*.f90 ../common/*.f90 ../compilers/*.f90 \
               ../coordinates/*.f90 ../datetime/*.f90 ../geodesy/*.f90 \
               ../ropp_messages/*.f90 ../unitconvert/*.f90
	mkdir mywork
	(cd mywork; ln -s ../../arrays/*.f90 .)
	(cd mywork; ln -s ../../common/*.f90 .)
	(cd mywork; ln -s ../../compilers/*.f90 .)
	(cd mywork; ln -s ../../datetime/*.f90 .)
	(cd mywork; ln -s ../../geodesy/*.f90 .)
	(cd mywork; ln -s ../../coordinates/*.f90 .)
	(cd mywork; ln -s ../../unitconvert/*.f90 .)
	(cd mywork; ln -s ../../ropp_messages/*.f90 .)
	$(ROBODOC) --src ./mywork --doc ./rm_utils --headless --footless --singledoc --sections --latex
	(sed -e 's/verbatim/Verbatim/' \
		rm_utils.tex > tmp.tex ; mv tmp.tex rm_utils.tex )
	rm -fr mywork

romsaf_ropp_utils_rm.dvi: romsaf_ropp_utils_rm.tex rm_intro.tex rm_utils.tex rm_copyright.tex
	latex romsaf_ropp_utils_rm
	bibtex romsaf_ropp_utils_rm
	sed s/'{SAF}'/'{ROM SAF}'/g <romsaf_ropp_utils_rm.bbl >romsaf_ropp_utils_rm.bbl_new
	sed s/'{ROM SAF}, G.,'/'{ROM SAF},'/g <romsaf_ropp_utils_rm.bbl_new >romsaf_ropp_utils_rm.bbl2_new
	sed s/'{ROM SAF}, R.,'/'{ROM SAF},'/g <romsaf_ropp_utils_rm.bbl2_new >romsaf_ropp_utils_rm.bbl
	rm romsaf_ropp_utils_rm.bbl_new romsaf_ropp_utils_rm.bbl2_new
	latex romsaf_ropp_utils_rm
	makeindex romsaf_ropp_utils_rm
	latex romsaf_ropp_utils_rm

romsaf_ropp_utils_rm.pdf: romsaf_ropp_utils_rm.dvi
	dvips -Pcmz -Pamz -ta4 $< -o romsaf_ropp_utils_rm.ps
	ps2pdf romsaf_ropp_utils_rm.ps
	rm -f romsaf_ropp_uils_rm.ps
#	dvipdfm -o romsaf_ropp_utils_rm.pdf romsaf_ropp_utils_rm
