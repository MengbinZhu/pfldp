# Makefile.am for ropp_fm reference documentation
# ===============================================

# 1. Test programs
# ----------------

# pkgdata_DATA =

# noinst_DATA  =

# 2. Files to clean
# -----------------

CLEANFILES           = html/* *.ps *.dvi *.pdf *.aux \
                       *.idx *.ilg *.ind *.log *.toc
MAINTAINERCLEANFILES = rm_fm.tex

# 3. Files for distribution
# -------------------------

EXTRA_DIST = robodoc.rc           \
             rm_intro.tex         \
             rm_fm.tex            \
             rm_copyright.tex     \
             romsaf_ropp_fm_rm.tex  \
             romsaf_ropp_fm_rm.dvi  \
             romsaf_ropp_fm_rm.pdf

# 4. Rules
# --------

all:

html:     html/index.html

refman:   romsaf_ropp_fm_rm.pdf

html/index.html: ../common/*.f90 ../math/matrix/*.f90 ../bangle_1d/*.f90 \
                 ../refrac_1d/*.f90 ../bangle_2d/*.f90 ../model_ecmwf/*.f90 \
                 ../model_meto/*.f90 ../tools/*.f90
	mkdir -p mywork html
	(cd mywork; ln -s ../../common/*.f90 .)
	(cd mywork; ln -s ../../math/matrix/*.f90 .)
	(cd mywork; ln -s ../../bangle_1d/*.f90 .)
	(cd mywork; ln -s ../../bangle_2d/*.f90 .)
	(cd mywork; ln -s ../../refrac_1d/*.f90 .)
	(cd mywork; ln -s ../../model_ecmwf/*.f90 .)
	(cd mywork; ln -s ../../model_meto/*.f90 .)
	(cd mywork; ln -s ../../tools/*.f90 .)
	$(ROBODOC) --src ./mywork --doc ./html --multidoc --index --toc --html
	(cd html ; cat index.html | grep -v masterindex.html | grep -v robo_sourcefiles.html > tmp.html ; mv tmp.html index.html )
	(cd html ; rm -f masterindex.html robo_sourcefiles.html )
	rm -fr mywork

rm_fm.tex : ../common/*.f90 ../math/matrix/*.f90 ../bangle_1d/*.f90 \
            ../refrac_1d/*.f90 ../bangle_2d/*.f90 ../model_ecmwf/*.f90 \
            ../model_meto/*.f90 ../tools/*.f90
	mkdir mywork
	(cd mywork; ln -s ../../common/*.f90 .)
	(cd mywork; ln -s ../../math/matrix/*.f90 .)
	(cd mywork; ln -s ../../bangle_1d/*.f90 .)
	(cd mywork; ln -s ../../bangle_2d/*.f90 .)
	(cd mywork; ln -s ../../refrac_1d/*.f90 .)
	(cd mywork; ln -s ../../model_ecmwf/*.f90 .)
	(cd mywork; ln -s ../../model_meto/*.f90 .)
	(cd mywork; ln -s ../../tools/*.f90 .)
	$(ROBODOC) --src ./mywork --doc ./rm_fm --headless --footless --singledoc --sections --latex
	(sed -e 's/verbatim/Verbatim/' \
	     rm_fm.tex > tmp.tex ; mv tmp.tex rm_fm.tex )
	rm -fr mywork

romsaf_ropp_fm_rm.dvi: romsaf_ropp_fm_rm.tex rm_intro.tex rm_fm.tex rm_copyright.tex
	latex romsaf_ropp_fm_rm
	bibtex romsaf_ropp_fm_rm
	sed s/'{SAF}'/'{ROM SAF}'/g <romsaf_ropp_fm_rm.bbl >romsaf_ropp_fm_rm.bbl_new
	sed s/'{ROM SAF}, G.,'/'{ROM SAF},'/g <romsaf_ropp_fm_rm.bbl_new >romsaf_ropp_fm_rm.bbl2_new
	sed s/'{ROM SAF}, R.,'/'{ROM SAF},'/g <romsaf_ropp_fm_rm.bbl2_new >romsaf_ropp_fm_rm.bbl
	rm romsaf_ropp_fm_rm.bbl_new romsaf_ropp_fm_rm.bbl2_new
	latex romsaf_ropp_fm_rm
	makeindex romsaf_ropp_fm_rm
	latex romsaf_ropp_fm_rm

romsaf_ropp_fm_rm.pdf: romsaf_ropp_fm_rm.dvi
	dvips -Pcmz -Pamz -ta4 $< -o romsaf_ropp_fm_rm.ps
	ps2pdf romsaf_ropp_fm_rm.ps
	rm -f romsaf_ropp_fm_rm.ps
#	dvipdfm -o romsaf_ropp_fm_rm.pdf romsaf_ropp_fm_rm
