# $Id: Makefile 34 2011-08-18 09:01:03Z frdo $
#
#****a* BUFR/Makefile *
#
########################################################################
#
# NAME
#   Makefile
#
# SYNOPSIS
#   Make file to add extra routines to MetDB BUFR object library &
#   build sample application/test programs
#
#  > make                         - add extra code to <prefix>/lib/libbufr.a
#                                   and build apps locally
#  > make install                 - install files to target <prefix> tree
#                                   & BUFR tables to BUFR_LIBRARY
#  > make test                    - run test apps
#
#  > make FF=<f90> FFLAGS=<flags> - use compiler <f90> [ifort]
#                                   with compiler flags <flags>
#  > make OBJ=<obj> EXE=<exe>     - use <obj> extention for object files [.o]
#                                   use <exe> extension for exec. files [none]
#  > make PREFIX=<prefix> install - install in <prefix>/lib
#
#  > make FF=<nag> NAGIF=<nagif>  - build using NAG compiler <nag> (normally
#                                   f95 or nagfor) with NAG system interface
#                                   source code file <nagif> (normally
#                                   nag_interfaces.f90)
#
# AUTHOR
#   Dave Offiler
#
########################################################################
#****

FF       = ifort

PREFIX   = $(HOME)

INCDIR   = $(PREFIX)/include

LIBDIR   = $(PREFIX)/lib

BINDIR   = $(PREFIX)/bin

MANDIR   = $(PREFIX)/man

HTMDIR   = $(HOME)/public_html/bufr

BUFRLIB  = libmetdbbufr.a

BUFR_LIBRARY = $(PREFIX)/data/bufr/

TESTFILE = ../metdb/TestMessage.bufr

MAKEFILE = Makefile

NAGIF    =

OBJ     = .o
EXE     =

SRCS    = bufrutils.f90   \
          gtshdrs.f90     \
          sec1tables.f90

OBJS    = $(SRCS:.f90=$(OBJ))

HTMDOC  = bufrdoc

TABLEB  = TABLEB
TABLED  = TABLED
CODEFIG = CODEFIG
DATACAT = DATACATEGORY
MASTERT = MASTERTABLE
ORIGCEN = ORIGCENTRE

FFLAGS  = -O2 -I$(INCDIR)
LFLAGS  = $(BUFRLIB)

INSTALL = cp -uvp
MKDIR   = mkdir -vp
ROBODOC = robodoc

.SUFFIXES:
.SUFFIXES: .f90 $(OBJ)

all:		decbufr$(EXE)     \
		testtables$(EXE)


$(OBJS):	$(SRCS) $(MAKEFILE)

$(BUFRLIB):	$(OBJS) ../metdb/libbufr.a
		@$(INSTALL) ../metdb/libbufr.a ./$(BUFRLIB)
		ar -srv $@ $(OBJS)

decbufr$(EXE):	decbufr.f90 $(BUFRLIB) $(NAGIF)
		$(FF) $(FFLAGS) -o $@ decbufr.f90 $(NAGIF) $(LFLAGS)

testtables$(EXE):	testtables.f90 $(BUFRLIB) $(NAGIF)
		$(FF) $(FFLAGS) -o $@ testtables.f90 $(NAGIF) $(LFLAGS)

.f90$(OBJ):
		$(FF) -c -o $@ $(FFLAGS) $<

install_mod:;	@$(MKDIR) $(INCDIR)
		@$(INSTALL) *.mod $(INCDIR)

install_lib:;	@$(MKDIR) $(LIBDIR)
		@$(INSTALL) $(BUFRLIB) $(LIBDIR)

install_bin:;	@$(MKDIR) $(BINDIR)
		@$(INSTALL) decbufr$(EXE) testtables$(EXE) $(BINDIR)

install_data:;	@$(MKDIR) $(BUFR_LIBRARY)
		@if [ -s ${TABLEB} ];  then \
                  $(INSTALL) ${TABLEB}  $(BUFR_LIBRARY); else \
                  $(INSTALL) ../metdb/${TABLEB}  $(BUFR_LIBRARY); fi
		@if [ -s ${TABLED} ];  then \
                  $(INSTALL) ${TABLED}  $(BUFR_LIBRARY); else \
                  $(INSTALL) ../metdb/${TABLED}  $(BUFR_LIBRARY); fi
		@if [ -s ${CODEFIG} ]; then \
                  $(INSTALL) ${CODEFIG} $(BUFR_LIBRARY); else \
                  $(INSTALL) ../metdb/${CODEFIG}  $(BUFR_LIBRARY); fi
		@if [ -f ${DATACAT} ]; then \
                  $(INSTALL) ${DATACAT} $(BUFR_LIBRARY); fi
		@if [ -f ${MASTERT} ]; then \
                  $(INSTALL) ${MASTERT} $(BUFR_LIBRARY); fi
		@if [ -f ${ORIGCEN} ]; then \
                  $(INSTALL) ${ORIGCEN} $(BUFR_LIBRARY); fi

install_man:;	@$(MKDIR) $(MANDIR)/man1
		@$(INSTALL) decbufr.1 $(MANDIR)/man1

htmldoc:	install_doc

install_doc:;	@if [ -n "$(ROBODOC)" ]; then \
                 echo; echo ">> ROBOdoc-umenting in $(HTMDIR)..."; \
                 $(MKDIR)   $(HTMDIR); \
                 $(ROBODOC) --src ./ --nodesc --ignore_case_when_linking \
                            --doc $(HTMDOC) --html --singledoc --toc \
                            --rc  robodoc.rc; \
                 $(INSTALL) $(HTMDOC).* $(HTMDIR); \
                fi

install:	install_mod install_lib  install_bin \
                install_data install_man install_doc

test:;		@export BUFR_LIBRARY=$(BUFR_LIBRARY); \
                 echo "---------------------------------------------"; \
		 $(BINDIR)/testtables${EXE} < testtables.dat; \
		 echo "---------------------------------------------"; \
		 $(BINDIR)/decbufr${EXE} $(TESTFILE) -d; \
		 echo "---------------------------------------------"

clean:;		@rm -f *$(OBJ) *.mod $(BUFRLIB)       \
                       decbufr$(EXE) testtables$(EXE) \
                       $(HTMDOC).*                    \
                       > /dev/null

uninstall_mod:;	@rm -f $(INCDIR)/bufrutils.mod  \
                       $(INCDIR)/BUFRUTILS.mod  \
                       $(INCDIR)/gtshdrs.mod    \
                       $(INCDIR)/GTSHDRS.mod    \
                       $(INCDIR)/sec1tables.mod \
                       $(INCDIR)/SEC1TABLES.mod \
                       > /dev/null 2>&1

uninstall_lib:;	@rm -f $(LIBDIR)/$(BUFRLIB) > /dev/null 2>&1

uninstall_bin:;	@rm -f $(BINDIR)/decbufr$(EXE)    \
                       $(BINDIR)/testtables$(EXE) \
                       > /dev/null 2>&1

uninstall_data:;	@cd $(BUFR_LIBRARY) ; \
		 rm -f $(TABLEB)  $(TABLED)  $(CODEFIG) \
                       $(MASTERT) $(DATACAT) $(ORIGCEN) \
                       > /dev/null 2>&1

uninstall_man:;	@rm -f $(MANDIR)/man1/decbufr.1 > /dev/null 2>&1

uninstall_doc:;	@rm -f $(HTMDIR)/$(HTMDOC).* > /dev/null 2>&1

uninstall:	uninstall_mod  uninstall_lib uninstall_bin \
                uninstall_data uninstall_man uninstall_doc

