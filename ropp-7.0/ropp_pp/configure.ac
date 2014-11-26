#
# $Id: configure.ac $
#
# configure.ac for ropp_pp
# ========================

# 1. Initialisation
# -----------------

AC_INIT(ropp_pp, 7.0, romsaf@metoffice.gov.uk)
AC_CONFIG_SRCDIR(common/ropp_pp.f90)

AC_PREREQ(2.59)

AM_INIT_AUTOMAKE(1.9.5 no-dependencies foreign)

AC_CANONICAL_HOST

# 2. Check for programs
# ---------------------

AC_PROG_CC()
AC_PROG_CC_STDC
AM_PROG_CC_C_O
CM_PROG_CC_VENDOR

AC_PROG_FC()
CM_PROG_FC_VENDOR

AC_PROG_RANLIB
AC_PATH_PROG(ROBODOC, robodoc)
AC_PATH_PROG(SVN2CL, svn2cl)

# 3. Check if compiling works
# ---------------------------

AC_LANG(C)
AC_C_CONST

AC_LANG(Fortran)
AC_FC_SRCEXT(f90)
AC_FC_FREEFORM()

# 4. Compiler specific settings
# -----------------------------

CM_COMPILER_DEFAULTS
CM_PROG_FC_BACKSLASH_ESCAPE

AM_CONDITIONAL(FC_BACKSLASH_ESCAPE, test x$FC_BACKSLASH_ESCAPE = xyes)
AM_CONDITIONAL(FC_IS_NAG, test x$CM_FC_VENDOR = xNAG)

case $host in
   *-sun-solaris*)
        case $M4 in
           /usr/ccs/bin/m4)
              M4_OPTS='-U len'
              ;;
           *)
              ;;
        esac
        ;;
   *)
        ;;
esac

AC_SUBST(M4_OPTS)

# 5. Check for C header files
# ---------------------------

AC_LANG(C)
AC_HEADER_STDC

# 6. Check for Fortran 90 module files
# ------------------------------------

AC_LANG(Fortran)

CM_CHECK_MODULE(ropp_utils)
AM_CONDITIONAL(HAVE_ROPP_UTILS, test x$HAVE_MODULE_ropp_utils = xyes)
if test x$HAVE_MODULE_ropp_utils = xno ; then
   AC_MSG_WARN([])
   AC_MSG_WARN([PACKAGE ROPP_UTILS NOT FOUND])
   AC_MSG_WARN([THIS PACKAGE REQUIRES ROPP_UTILS TO BE INSTALLED FIRST.])
   AC_MSG_WARN([*** NOTE:                                            ***])
   AC_MSG_WARN([*** Users wishing to install ROPP_PP must first have ***])
   AC_MSG_WARN([*** the ROPP_UTILS package installed before building ***])
   AC_MSG_WARN([*** this package. See ROPP Release Notes or ROPP     ***])
   AC_MSG_WARN([*** User Guide for further details.                  ***])
   AC_MSG_WARN([])
   AC_MSG_ERROR([Module ROPP_UTILS not found])
fi

CM_CHECK_MODULE(typesizes)
AM_CONDITIONAL(NO_TYPESIZES, test x$HAVE_MODULE_typesizes = xno)
if test x$HAVE_MODULE_typesizes = xno ; then
   AC_MSG_WARN([])
   AC_MSG_WARN([PACKAGE TYPESIZES NOT FOUND])
   AC_MSG_WARN([THIS PACKAGE REQUIRES TYPESIZES TO BE INSTALLED FIRST.])
   AC_MSG_WARN([TypeSizes is included in the ROPP_UTILS module. Check ])
   AC_MSG_WARN([that you have correctly installed ROPP_UTILS before   ])
   AC_MSG_WARN([building this package. See ROPP User Guide for details])
   AC_MSG_WARN([])
   AC_MSG_ERROR([Module TYPESIZES not found])
fi

CM_CHECK_MODULE(ropp_io)
AM_CONDITIONAL(HAVE_ROPP_IO, test x$HAVE_MODULE_ropp_io = xyes)
if test x$HAVE_MODULE_ropp_io = xno ; then
   AC_MSG_WARN([])
   AC_MSG_WARN([PACKAGE ROPP_IO NOT FOUND])
   AC_MSG_WARN([WILL NOT COMPILE STAND-ALONE PROGRAMS REQUIRING ROPP_IO.])
   AC_MSG_WARN([(Stand alone tool: ropp_pp_tool; Tests: ropp_test_abel).])
   AC_MSG_WARN([Making ropp_pp module with no ropp_io dependencies.])
   AC_MSG_WARN([Users may link the ropp_pp library or the subroutines ])
   AC_MSG_WARN([provided in this module to their own applications.])
   AC_MSG_WARN([])
fi

# 7. Check for libraries
# ----------------------

FCFLAGS_orig="$FCFLAGS" ; FCFLAGS="$AM_FCFLAGS $FCFLAGS"

AC_LANG(Fortran)

CM_REQUIRE_LIB(ropp_utils, message, -lropp_utils)

## 7.1 Required libraries for linking stand-alone executables
if test x$HAVE_MODULE_ropp_io = xyes ; then
   CM_CHECK_LIB(hdf5, nf_inq_ncid, -lnetcdff -lnetcdf -lhdf5hl_fortran -lhdf5_hl -lhdf5_fortran -lhdf5 -lz)
   AM_CONDITIONAL(HAVE_HDF5, test x$HAVE_LIBRARY_hdf5 = xyes)

   CM_CHECK_LIB(netcdf, nf_open, -lnetcdff -lnetcdf)
   if test ! x$HAVE_LIBRARY_netcdf = xyes ; then
      AC_LANG(Fortran)
      CM_REQUIRE_LIB(netcdff, nf_open, -lnetcdff)
   fi

   CM_CHECK_LIB(ncdf, is_netcdf, -lncdf)
   AM_CONDITIONAL(HAVE_NCDF, test x$HAVE_LIBRARY_ncdf = xyes)
   if test x$HAVE_LIBRARY_ncdf = xno ; then
    CM_CHECK_LIB(ropp_io, is_netcdf,            -lropp_io)
    CM_CHECK_LIB(ropp_io, ropp_io_init_l1atype, -lropp_io)
   else
    CM_CHECK_LIB(ropp_io, ropp_io_init_l1atype, -lropp_io)
    fi
fi
AM_CONDITIONAL(HAVE_NCDF, test x$HAVE_LIBRARY_ncdf = xyes)

CM_CHECK_LIB(netcdf, nf_open, -lnetcdff -lnetcdf)
AM_CONDITIONAL(HAVE_NETCDF, test x$HAVE_LIBRARY_netcdf = xyes)
if test x$HAVE_LIBRARY_netcdf = xno ; then
   AC_MSG_WARN([])
   AC_MSG_WARN([LIBRARY NETCDF NOT FOUND])
   AC_MSG_WARN([WILL NOT COMPILE ROPP_PP ROUTINES REQUIRING NETCDF.])
   AC_MSG_WARN([(MSIS-reading routine: ropp_pp_read_MSIS).])
   AC_MSG_WARN([Making ropp_pp module with no netcdf dependencies.])
   AC_MSG_WARN([Users may link the ropp_pp library or the subroutines ])
   AC_MSG_WARN([provided in this module to their own applications.])
   AC_MSG_WARN([])
fi

CM_CHECK_LIB(ropp_io, is_netcdf, -lropp_io)
AM_CONDITIONAL(HAVE_NCDF_IO, test x$HAVE_LIBRARY_ncdf_io = xyes)
if test x$HAVE_LIBRARY_ncdf_io = xno ; then
   AC_MSG_WARN([])
   AC_MSG_WARN([LIBRARY NCDF NOT FOUND])
   AC_MSG_WARN([WILL NOT COMPILE ROPP_PP ROUTINES REQUIRING NETCDF.])
   AC_MSG_WARN([(MSIS-reading routine: ropp_pp_read_MSIS).])
   AC_MSG_WARN([Making ropp_pp module with no netcdf dependencies.])
   AC_MSG_WARN([Users may link the ropp_pp library or the subroutines ])
   AC_MSG_WARN([provided in this module to their own applications.])
   AC_MSG_WARN([])
fi

# 8. Add directories to the include path(s)
# -----------------------------------------


CPPFLAGS="$CPPFLAGS -I../include -I../FFTW"
FCFLAGS="$FCFLAGS_orig"

CM_ENSURE(AM_FCFLAGS,$FCFLAGS_f90 $CM_FCFLAG_MODINC../build $AM_FCFLAGS)

# 9. Finish
# ---------

AC_CONFIG_FILES(Makefile              \
		abel_invert/Makefile  \
		bangle/Makefile       \
                build/Makefile        \
                common/Makefile       \
		config/Makefile       \
		data/Makefile         \
		doc/Makefile          \
		icorr/Makefile        \
		msis_bangle/Makefile  \
		preprocess/Makefile   \
		tph/Makefile          \
		tests/Makefile        \
                tools/Makefile)

AC_OUTPUT
