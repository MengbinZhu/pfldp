# Makefile.am for ropp_pp reference documentation
# ===============================================

# 1. Test programs
# ----------------

# pkgdata_DATA =

# noinst_DATA  =

# 2. Files to clean
# -----------------

CLEANFILES           = html/* *.ps *.dvi *.pdf *.aux \
                       *.idx *.ilg *.ind *.log *.toc
MAINTAINERCLEANFILES = rm_pp.tex

# 3. Files for distribution
# -------------------------

EXTRA_DIST = robodoc.rc           \
             rm_intro.tex         \
             rm_pp.tex            \
             rm_copyright.tex     \
             romsaf_ropp_pp_rm.tex  \
             romsaf_ropp_pp_rm.dvi  \
             romsaf_ropp_pp_rm.pdf

# 4. Rules
# --------

all:

html:     html/index.html

refman:   romsaf_ropp_pp_rm.pdf

html/index.html: ../common/*.f90 ../bangle/*.f90 ../abel_invert/*.f90 \
                 ../icorr/*.f90 ../msis_bangle/*.f90 ../preprocess/*.f90 \
                 ../tools/*.f90
	mkdir mywork
	(cd mywork; ln -s ../../common/*.f90 .)
	(cd mywork; ln -s ../../bangle/*.f90 .)
	(cd mywork; ln -s ../../abel_invert/*.f90 .)
	(cd mywork; ln -s ../../icorr/*.f90 .)
	(cd mywork; ln -s ../../msis_bangle/*.f90 .)
	(cd mywork; ln -s ../../preprocess/*.f90 .)
	(cd mywork; ln -s ../../tools/*.f90 .)
	$(ROBODOC) --src ./mywork --doc ./html --multidoc --index --toc --html
	(cd html ; cat index.html | grep -v masterindex.html | grep -v robo_sourcefiles.html > tmp.html ; mv tmp.html index.html )
	(cd html ; rm -f masterindex.html robo_sourcefiles.html )
	rm -fr mywork

rm_pp.tex :  ../common/*.f90 ../bangle/*.f90 ../abel_invert/*.f90 \
             ../icorr/*.f90 ../msis_bangle/*.f90 ../preprocess/*.f90 \
             ../tools/*.f90
	mkdir mywork
	(cd mywork; ln -s ../../common/*.f90 .)
	(cd mywork; ln -s ../../bangle/*.f90 .)
	(cd mywork; ln -s ../../abel_invert/*.f90 .)
	(cd mywork; ln -s ../../icorr/*.f90 .)
	(cd mywork; ln -s ../../msis_bangle/*.f90 .)
	(cd mywork; ln -s ../../preprocess/*.f90 .)
	(cd mywork; ln -s ../../tools/*.f90 .)
	$(ROBODOC) --src ./mywork --doc ./rm_pp --headless --footless --singledoc --sections --latex
	(sed -e 's/verbatim/Verbatim/' \
	     rm_pp.tex > tmp.tex ; mv tmp.tex rm_pp.tex )
	rm -fr mywork

romsaf_ropp_pp_rm.dvi: romsaf_ropp_pp_rm.tex rm_intro.tex rm_pp.tex rm_copyright.tex
	latex romsaf_ropp_pp_rm
	latex romsaf_ropp_pp_rm
	bibtex romsaf_ropp_pp_rm
	sed s/'{SAF}'/'{ROM SAF}'/g <romsaf_ropp_pp_rm.bbl >romsaf_ropp_pp_rm.bbl_new
	sed s/'{ROM SAF}, G.,'/'{ROM SAF},'/g <romsaf_ropp_pp_rm.bbl_new >romsaf_ropp_pp_rm.bbl2_new
	sed s/'{ROM SAF}, R.,'/'{ROM SAF},'/g <romsaf_ropp_pp_rm.bbl2_new >romsaf_ropp_pp_rm.bbl
	rm romsaf_ropp_pp_rm.bbl_new romsaf_ropp_pp_rm.bbl2_new
	makeindex romsaf_ropp_pp_rm
	latex romsaf_ropp_pp_rm

romsaf_ropp_pp_rm.pdf: romsaf_ropp_pp_rm.dvi
	dvips -Pcmz -Pamz -ta4 $< -o romsaf_ropp_pp_rm.ps
	ps2pdf romsaf_ropp_pp_rm.ps
	rm -f romsaf_ropp_pp_rm.ps
#	dvipdfm -o romsaf_ropp_pp_rm.pdf romsaf_ropp_pp_rm
