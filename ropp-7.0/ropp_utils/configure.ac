# Process this file with autoconf to produce a configure script

# 1. Initialisation
# -----------------

AC_INIT(ropp_utils, 7.0, romsaf@metoffice.gov.uk)
AC_CONFIG_SRCDIR(common/ropp_utils.f90)

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
CM_FC_SRCEXT(f90)
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
esac

AC_SUBST(M4_OPTS)

# 5. Check for Fortran 90 module files
# ------------------------------------

AC_LANG(Fortran)

CM_CHECK_MODULE(typesizes)
AM_CONDITIONAL(NO_TYPESIZES, test x$HAVE_MODULE_typesizes = xno)

# 6. Add directories to the include path(s)
# -----------------------------------------

CM_ENSURE(AM_FCFLAGS,$FCFLAGS_f90 $CM_FCFLAG_MODINC../build $AM_FCFLAGS)

# 7. Finish
# ---------

AC_CONFIG_FILES(Makefile                  \
                common/Makefile           \
                build/Makefile            \
                arrays/Makefile           \
                compilers/Makefile        \
                coordinates/Makefile      \
                datetime/Makefile         \
                doc/Makefile              \
                geodesy/Makefile          \
                ropp_messages/Makefile    \
                unitconvert/Makefile      \
                tests/Makefile)
AC_OUTPUT
