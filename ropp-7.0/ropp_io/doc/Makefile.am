# Makefile.am for ropp_io documentation
# =====================================

# 1. Test programs
# ----------------

# pkgdata_DATA =

# noinst_DATA  =

# 2. Files to clean
# -----------------

CLEANFILES           = html/* *.ps *.dvi *.pdf *.aux \
                       *.idx *.ilg *.ind *.log *.toc
MAINTAINERCLEANFILES = rm_io.tex

# 3. Files for distribution
# -------------------------

EXTRA_DIST = robodoc.rc           \
             rm_intro.tex         \
             rm_io.tex            \
             rm_copyright.tex     \
             romsaf_ropp_io_rm.tex  \
             romsaf_ropp_io_rm.dvi  \
             romsaf_ropp_io_rm.pdf

# 4. Rules
# --------

all:

html:     html/index.html

refman:   romsaf_ropp_io_rm.pdf

html/index.html: ../ropp/*.f90 ../common/*.f90 ../thin/*.f90 \
                 ../ncdf/*.f90 ../bufr/*.f90 ../tools/*.f90
	mkdir -p mywork html
	(cd mywork; ln -s ../../ropp/*.f90 .)
	(cd mywork; ln -s ../../common/*.f90 .)
	(cd mywork; ln -s ../../thin/*.f90 .)
	(cd mywork; ln -s ../../ncdf/*.f90 .)
	(cd mywork; rm -f ncdf.f90)
	(cd mywork; ln -s ../../bufr/*.f90 .)
	(cd mywork; ln -s ../../tools/*.f90 .)
	$(ROBODOC) --src ./mywork --doc ./html --multidoc --index --toc --html
	(cd html ; cat index.html | grep -v masterindex.html | grep -v robo_sourcefiles.html > tmp.html ; mv tmp.html index.html )
	(cd html ; rm -f masterindex.html robo_sourcefiles.html )
	rm -fr mywork

rm_io.tex : ../ropp/*.f90 ../common/*.f90 ../thin/*.f90 \
            ../ncdf/*.f90 ../bufr/*.f90 ../tools/*.f90
	mkdir mywork
	(cd mywork; ln -s ../../ropp/*.f90 .)
	(cd mywork; ln -s ../../common/*.f90 .)
	(cd mywork; ln -s ../../thin/*.f90 .)
	(cd mywork; ln -s ../../ncdf/*.f90 .)
	(cd mywork; rm -f ncdf.f90)
	(cd mywork; ln -s ../../bufr/*.f90 .)
	(cd mywork; ln -s ../../tools/*.f90 .)
	$(ROBODOC) --src ./mywork --doc ./rm_io --headless --footless --singledoc --sections --latex
	(sed -e 's/verbatim/Verbatim/' \
	     rm_io.tex > tmp.tex ; mv tmp.tex rm_io.tex )
	rm -fr mywork

romsaf_ropp_io_rm.dvi: romsaf_ropp_io_rm.tex rm_intro.tex rm_io.tex rm_copyright.tex
	latex romsaf_ropp_io_rm
	bibtex romsaf_ropp_io_rm
	sed s/'{SAF}'/'{ROM SAF}'/g <romsaf_ropp_io_rm.bbl >romsaf_ropp_io_rm.bbl_new
	sed s/'{ROM SAF}, G.,'/'{ROM SAF},'/g <romsaf_ropp_io_rm.bbl_new >romsaf_ropp_io_rm.bbl2_new
	sed s/'{ROM SAF}, R.,'/'{ROM SAF},'/g <romsaf_ropp_io_rm.bbl2_new >romsaf_ropp_io_rm.bbl
	rm romsaf_ropp_io_rm.bbl_new romsaf_ropp_io_rm.bbl2_new
	latex romsaf_ropp_io_rm
	makeindex romsaf_ropp_io_rm
	latex romsaf_ropp_io_rm

romsaf_ropp_io_rm.pdf: romsaf_ropp_io_rm.dvi
	dvips -Pcmz -Pamz -ta4 $< -o romsaf_ropp_io_rm.ps
	ps2pdf romsaf_ropp_io_rm.ps
	rm -f romsaf_ropp_io_rm.ps
#	dvipdfm -o romsaf_ropp_io_rm.pdf romsaf_ropp_io_rm
