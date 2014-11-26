#
#  $Id: configure.ac 3696 2013-06-17 08:48:37Z idculv $
#
# configure.ac for ropp_io
# ========================

# 1. Initialisation
# -----------------

AC_INIT(ropp_io, 7.0, romsaf@metoffice.gov.uk)

AC_CONFIG_SRCDIR(ropp/ropp_io.f90)

AC_PREREQ(2.59)

AM_INIT_AUTOMAKE(1.9.5 no-dependencies foreign)

AC_CANONICAL_HOST

# 2. Check for programs
# ---------------------

AC_PROG_FC()
CM_PROG_FC_VENDOR

AC_PROG_RANLIB
AC_PATH_PROG(M4, m4)
AC_PATH_PROG(ROBODOC, robodoc)
AC_PATH_PROG(SVN2CL, svn2cl)

# 3. Check if compiling works
# ---------------------------

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

# 6. Check for Fortran 90 module files
# ------------------------------------

CM_CHECK_MODULE(netcdf)
AM_CONDITIONAL(HAVE_NETCDF, test x$HAVE_MODULE_netcdf = xyes)
if test x$HAVE_MODULE_netcdf = xno ; then
   AC_MSG_WARN([])
   AC_MSG_WARN([PACKAGE NETCDF NOT FOUND])
   AC_MSG_WARN([THIS PACKAGE REQUIRES NETCDF TO BE INSTALLED FIRST.])
   AC_MSG_WARN([*** NOTE:                                            ***])
   AC_MSG_WARN([*** Users wishing to install ROPP_IO must first have ***])
   AC_MSG_WARN([*** the NETCDF package installed before building     ***])
   AC_MSG_WARN([*** this package. See ROPP Release Notes or ROPP     ***])
   AC_MSG_WARN([*** User Guide for further details.                  ***])
   AC_MSG_WARN([])
   AC_MSG_ERROR([Module NETCDF not found])
fi

CM_CHECK_MODULE(ropp_utils)
AM_CONDITIONAL(HAVE_ROPP_UTILS, test x$HAVE_MODULE_ropp_utils = xyes)
if test x$HAVE_MODULE_ropp_utils = xno ; then
   AC_MSG_WARN([])
   AC_MSG_WARN([PACKAGE ROPP_UTILS NOT FOUND])
   AC_MSG_WARN([THIS PACKAGE REQUIRES ROPP_UTILS TO BE INSTALLED FIRST.])
   AC_MSG_WARN([*** NOTE:                                            ***])
   AC_MSG_WARN([*** Users wishing to install ROPP_IO must first have ***])
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

# 7. Check for libraries
# ----------------------

FCFLAGS_orig="$FCFLAGS" ; FCFLAGS="$AM_FCFLAGS $FCFLAGS"

# 7.1 BUFR (Optional; one of ECMWF libbufr.a or MetDB libmetbufr.a)
# -------------------------------------------------------------------

CM_CHECK_LIB(metdbbufr, enbufv4, -lmetdbbufr)
AM_CONDITIONAL(HAVE_METO_BUFR, test x$HAVE_LIBRARY_metdbbufr = xyes)
if test x$HAVE_LIBRARY_metdbbufr = xyes ; then
  if test x$BUFR_LIBRARY = x ; then
    AC_MSG_WARN([The BUFR_LIBRARY environment variable must be set
                 properly at runtime for the BUFR programs to work.])
  fi
fi
NO_ECMWF_LIBS="$LIBS"

CM_CHECK_LIB(bufr, bufren, -lbufr)
AM_CONDITIONAL(HAVE_ECMWF_BUFR, test x$HAVE_LIBRARY_bufr = xyes)
if test x$HAVE_LIBRARY_bufr = xyes ; then
  if test x$BUFR_TABLES = x ; then
    AC_MSG_WARN([The BUFR_TABLES environment variable must be set
                 properly at runtime for the BUFR programs to work.])
  fi
fi

if test x$HAVE_LIBRARY_bufr = xyes ; then
  if test x$HAVE_LIBRARY_metdbbufr = xyes ; then
    AC_MSG_NOTICE([Both MetDB & ECMWF BUFR libraries found; using MetDB in preference])
    AM_CONDITIONAL(HAVE_ECMWF_BUFR, test x$HAVE_LIBRARY_bufr = xno)
    AS_VAR_SET(HAVE_LIBRARY_bufr, no)
    LIBS="$NO_ECMWF_LIBS"
  fi
fi

if test x$HAVE_LIBRARY_bufr = xno ; then
  if test x$HAVE_LIBRARY_metdbbufr = xno ; then
    AC_MSG_WARN([])
    AC_MSG_WARN([LIBRARY BUFR (ECMWF or METDB) NOT FOUND])
    AC_MSG_WARN([THIS PACKAGE USES EITHER ECMWF OR METDB BUFR LIBRARY])
    AC_MSG_WARN([*** NOTE:                                            ***])
    AC_MSG_WARN([*** Users wishing to build the ROPP BUFR encoder or  ***])
    AC_MSG_WARN([*** decoder must first have either the ECMWF or      ***])
    AC_MSG_WARN([*** MetDB (Met Office) BUFR library installed        ***])
    AC_MSG_WARN([*** before building this package. See ROPP Release   ***])
    AC_MSG_WARN([*** Notes or ROPP  UserGuide for further details.    ***])
    AC_MSG_WARN([*** Build will proceed without BUFR support.         ***])
    AC_MSG_WARN([])
    AC_MSG_WARN([Library BUFR (ECMWF or METDB) not found])
  fi
fi

# 7.2 GRIB (Optional; ECMWF library)
# ----------------------------------

CM_CHECK_LIB(grib_api, grib_f_open_file, -lgrib_api_f90 -lgrib_api)
AM_CONDITIONAL(HAVE_GRIB, test x$HAVE_LIBRARY_grib_api = xyes)
if test x$HAVE_LIBRARY_grib_api = xno ; then
  AC_MSG_WARN([])
  AC_MSG_WARN([LIBRARY GRIB (ECMWF) NOT FOUND])
  AC_MSG_WARN([THIS PACKAGE MAY USE THE ECMWF GRIB LIBRARY])
  AC_MSG_WARN([*** NOTE:                                            ***])
  AC_MSG_WARN([*** Users wishing to build the GRIB file reader must ***])
  AC_MSG_WARN([*** install the ECMWF GRIB library first. See        ***])
  AC_MSG_WARN([*** ROPP Release Notes or ROPP User Guide            ***])
  AC_MSG_WARN([*** for further details.                             ***])
  AC_MSG_WARN([*** Build will proceed without GRIB support.         ***])
  AC_MSG_WARN([])
  AC_MSG_WARN([Library GRIB (ECMWF) not found])
fi

# 7.3 NC4/HDF5 (Optional)
# -----------------------

CM_CHECK_LIB(hdf5, nf_inq_ncid, -lnetcdff -lnetcdf -lhdf5hl_fortran -lhdf5_hl -lhdf5_fortran -lhdf5 -lz)
#CM_CHECK_LIB(hdf5, dummy_routine, -ldummy_lib)
AM_CONDITIONAL(HAVE_HDF5, test x$HAVE_LIBRARY_hdf5 = xyes)
if test x$HAVE_LIBRARY_hdf5 = xno ; then
  AC_MSG_WARN([])
  AC_MSG_WARN([LIBRARY NC4/HDF5 NOT FOUND])
  AC_MSG_WARN([THIS PACKAGE MAY USE THE NC4/HDF5 LIBRARY])
  AC_MSG_WARN([*** NOTE:                                            ***])
  AC_MSG_WARN([*** Users wishing to build readers for EUMETSAT      ***])
  AC_MSG_WARN([*** netcdf4 files must install the NC4/HDF5 library  ***])
  AC_MSG_WARN([*** first. See ROPP Release Notes or ROPP User       ***])
  AC_MSG_WARN([*** Guide for further details.                       ***])
  AC_MSG_WARN([*** Build will proceed without NC4/HDF5 support.     ***])
  AC_MSG_WARN([])
  AC_MSG_WARN([Library NC4/HDF5 not found])
fi

# 7.4 netCDF (separate base & Fortran API libraries)
# --------------------------------------------------

CM_CHECK_LIB(netcdf, nf_open, -lnetcdff -lnetcdf)
AM_CONDITIONAL(HAVE_NETCDF, test x$HAVE_LIBRARY_netcdf = xyes)
if test x$HAVE_LIBRARY_netcdf = xno ; then
   AC_MSG_WARN([])
   AC_MSG_WARN([LIBRARY NETCDF (Fortran) NOT FOUND])
   AC_MSG_WARN([THIS PACKAGE REQUIRES NETCDF TO BE INSTALLED FIRST.])
   AC_MSG_WARN([*** NOTE:                                            ***])
   AC_MSG_WARN([*** Users wishing to install ROPP_IO must first have ***])
   AC_MSG_WARN([*** the NETCDF package installed before building     ***])
   AC_MSG_WARN([*** this package. See ROPP Release Notes or ROPP     ***])
   AC_MSG_WARN([*** User Guide for further details.                  ***])
   AC_MSG_WARN([])
   AC_MSG_ERROR([Library NETCDF not found])
fi

# 7.5 ropp_utils
# --------------

CM_CHECK_LIB(ropp_utils, message, -lropp_utils)
AM_CONDITIONAL(HAVE_UTILS, test x$HAVE_LIBRARY_ropp_utils = xyes)
if test x$HAVE_LIBRARY_ropp_utils = xno ; then
   AC_MSG_WARN([])
   AC_MSG_WARN([LIBRARY ROPP_UTILS NOT FOUND])
   AC_MSG_WARN([THIS PACKAGE REQUIRES ROPP_UTILS TO BE INSTALLED FIRST.])
   AC_MSG_WARN([*** NOTE:                                            ***])
   AC_MSG_WARN([*** Users wishing to install ROPP_IO must install    ***])
   AC_MSG_WARN([*** the ROPP_UTILS package first. See ROPP Release   ***])
   AC_MSG_WARN([*** Notes or ROPP User Guide for further details.    ***])
   AC_MSG_WARN([])
   AC_MSG_ERROR([Library ROPP_UTILS not found])
fi

FCFLAGS="$FCFLAGS_orig"

# 8. Add directories to the include path(s)
# -----------------------------------------

CM_ENSURE(AM_FCFLAGS, $FCFLAGS_f90 $CM_FCFLAG_MODINC../build $AM_FCFLAGS)

# 9. Finish
# ---------

AC_CONFIG_FILES(Makefile              \
                build/Makefile        \
                common/Makefile       \
                contrib/Makefile      \
                contrib/idl/Makefile  \
                contrib/wave/Makefile \
                data/Makefile         \
                doc/Makefile          \
                ropp/Makefile         \
                thin/Makefile         \
                ncdf/Makefile         \
                bufr/Makefile         \
                tools/Makefile        \
                tests/Makefile)
AC_OUTPUT
