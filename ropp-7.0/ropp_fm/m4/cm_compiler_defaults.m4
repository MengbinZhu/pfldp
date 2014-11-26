# $Id: cm_compiler_defaults.m4 3086 2011-10-14 08:54:50Z idculv $

#****f* cm_macros/cm_compiler_defaults.m4 *
#
# NAME
#    cm_compiler_defaults.m4 - Setting default switches for various compilers.
#
# SYNOPSIS
#    Place the cm_compilers.m4 and cm_compiler_default.m4 files in a subdirectory
#    'm4' of the top level directory of the project, and add
#
#       ACLOCAL_AMFLAGS  = -I m4
#
#    to the toplevel Makefile.am.
#
#    To generate the initial aclocal.m4, run
#
#       aclocal -I m4
#
#    and the usual sequence of automake and autoconf thereafter.
# 
# DESCRIPTION
#    The file cm_compiler_defaults.m4 provides an autoconf / automake macro for
#    setting default switches for various compilers.
#
# NOTES
#    cm_compiler_defaults.m4 requires (at least) autoconf 2.59 and automake
#    1.9.5 or above. A properly patched version of automake 1.8.3 can also
#    be used. It also uses some macros from cm_tools.m4 and cm_compilers.m4.
#
# SEE ALSO
#    cm_compiler_defaults
#
#    cm_tools.m4
#    cm_compilers.m4
#    cm_fortran.m4
#    cm_libraries.m4
#
# AUTHOR
#    C. Marquardt, Darmstadt, Germany              <christian@marquardt.sc>
#
# COPYRIGHT
#
#    Copyright (c) 2005 Christian Marquardt        <christian@marquardt.sc>
#
#    All rights reserved.
#
#    Permission is hereby granted, free of charge, to any person obtaining
#    a copy of this software and associated documentation files (the
#    "Software"), to deal in the Software without restriction, including
#    without limitation the rights to use, copy, modify, merge, publish,
#    distribute, sublicense, and/or sell copies of the Software, and to
#    permit persons to whom the Software is furnished to do so, subject to
#    the following conditions:
#
#    The above copyright notice and this permission notice shall be
#    included in all copies or substantial portions of the Software as well
#    as in supporting documentation.
#
#    THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
#    EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
#    MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
#    NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE
#    LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
#    OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION
#    WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
#
#****

#****m* cm_compiler_defaults.m4/cm_compiler_defaults *
#
# NAME
#    CM_COMPILER_DEFAULTS - Set default switches for various compilers.
#
# SYNOPSIS
#    CM_COMPILER_DEFAULTS
# 
# DESCRIPTION
#    This macro generates default settings for various platforms and compilers
#    compilers. These default settings include C preprocessor flags for cfortran.h,
#    the inclusion of libraries to Fortran compilers (e.g., in order to get getarg()
#    and getenv() by default), and some automake conditionals and shell variables
#    useful for the development of automake and autoconf build environments.
#
#    For known Fortran compilers, the macro presets a shell variable to indicate
#    if the compiler users lowercase or uppercase module names; these can be used
#    in Makefile.am's via the CM_FC_UC_MODNAMES which is true if the compiler wants
#    uppercase module names. If the default settings do not work, the two command
#    line options --with-uppercase-modnames and --with-lowercase-modnames can be
#    used to overwrite the default settings.
#
#    On some platforms, preprocessing of Fortran source code with the native 
#    compilers is not straightforward, or can at least not easily be implemented
#    with the current versions of automake and autoconf. In particular, the native
#    AIX and HPUX compilers require specific targets in the generated Makefiles.
#    While the current version of the cm_macros does not transparently supports
#    these platforms, the automake macros CM_NATIVE_AIX and CM_NATIVE_HPUX can
#    at least be used to provided the specific targets in the Makefile.am's for
#    these platforms.
#
# OUTPUT
#    This macro defines the following automake conditionals:
#
#       CM_FC_UC_MODNAMES
#       CM_NATIVE_AIX
#       CM_NATIVE_HPUX
#
#    as well as the following Makefile variables (which are AC_SUBST'd):
#
#       AM_CPPFLAGS
#       AM_CFLAGS
#       AM_FFLAGS
#       AM_FCFLAGS
#       AM_LDFLAGS
#
#    In addition, the shell variable
#
#       CM_FCFLAG_MODINC
#
#    is set to a compiler switch (like '-I') which allows the specification
#    of directories where the FC compilers searches for Fortran 9x module
#    files.
#
# NOTES
#    The automake conditionals are intended to be used in Makefile.am's to
#    specify additional target on certain platforms (mainly to be used for
#    preprocessing, which may require explicit rules on HPUX or AIX), or to
#    be able to construct proper names of module files generated during
#    compilation in CLEANFILES.
#
#    The Makefile variables, if defined previously, will be overwritten.
#    The main purpose of this macro is to provide compiler and preprocessor
#    switches on various platforms on which I (Christian Marquardt) work 
#    regularly, in order to ensure certain behaviours. The latter include
#
#       - proper (C-) preprocessor settings for the use of cfortran.h
#       - distinction between C and Fortran function names by appending an
#         underscore to the Fortran names (on HPUX and IBM AIX)
#       - inclusion of several library functions (like getarg(), getenv()) 
#         which are usually available from every compiler, but may require
#         special command line options during compiling and linking
#
#    The functionality is achieved by setting the AM_* variables used by
#    automake; variables set by the user when running configure are not changed.
#
# EXAMPLE
#    (to be given...)
#
# SEE ALSO
#    cm_compilers.m4
#    cm_fortran.m4
#    cm_libraries.m4
#
#****

# 1. CM_COMPILER_DEFAULTS
# -----------------------

AC_DEFUN([CM_COMPILER_DEFAULTS],
[AC_REQUIRE([AC_CANONICAL_HOST])
 AC_REQUIRE([AC_PROG_FC])
 AC_REQUIRE([CM_PROG_FC_VENDOR])

 CM_FCFLAG_MODINC=-I
 CM_FC_UC_MODNAMES=no
 CM_NATIVE_HPUX=no
 CM_NATIVE_AIX=no

 case $host in
    *-*-linux*)
         AM_CFLAGS=""
	 case $CM_FC_VENDOR in
	    Intel)
               CM_VERSION_INTEL=8-11
               CM_FC_UC_MODNAMES=no
               AM_CPPFLAGS="-DNAGf90Fortran"
               AM_FCFLAGS="-Vaxlib -fp_port"
               CM_FCFLAG_MODINC=-I
               cm_fc_v_output=`$FC -V 2>&1`
               cm_fc_v_string=`echo $cm_fc_v_output | grep 'Version 7.\|Revision 3.'`
               if test x"$cm_fc_v_string" != x; then
                  CM_VERSION_INTEL="pre8"
                  ac_cv_uc_modnames=yes
                  CM_FC_UC_MODNAMES=yes
               fi
               cm_fc_v_string=`echo $cm_fc_v_output | grep 'NEC Fortran Itanium'`
               if test x"$cm_fc_v_string" != x; then
                  CM_VERSION_INTEL=i5+
                  AM_FCFLAGS="-Vaxlib"
               fi
               cm_fc_v_string=`echo $cm_fc_v_output | grep 'Compiler XE'`
               if test x"$cm_fc_v_string" != x; then
                  CM_VERSION_INTEL=12+
                  AM_FCFLAGS="-fp_port"
               fi
	       ;;
            Portland)
               CM_FCFLAG_MODINC=-I
               AM_CPPFLAGS="-DpgiFortran"
               AM_FCFLAGS=""
               ac_cv_uc_modnames=no
               CM_FC_UC_MODNAMES=no
	       ;;
	    Fujitsu)
               CM_FCFLAG_MODINC=-I
               AM_CPPFLAGS="-DNAGf90Fortran"
               AM_FCFLAGS=""
               ac_cv_uc_modnames=no
               CM_FC_UC_MODNAMES=no
	       ;;
	 esac      
	 ;;
    *-sun-solaris*)
	 case $CM_FC_VENDOR in
	    Fujitsu)
               CM_FCFLAG_MODINC=-I
               AM_CPPFLAGS="-DNAGf90Fortran"
               AM_FCFLAGS=""
               ac_cv_uc_modnames=no
               CM_FC_UC_MODNAMES=no
	       ;;
            *)
               CM_FCFLAG_MODINC=-M
               AM_CFLAGS=""
               AM_CPPFLAGS=""
	       AM_FCFLAGS=""
	       AM_LDFLAGS=
               ac_cv_uc_modnames=no
               CM_FC_UC_MODNAMES=no
	       ;;
         esac
         ;;
    *-hp-hpux*)
         if [ test x$CM_FC_VENDOR != xNAG ] && \
            [ test x$CM_FC_VENDOR != xGCC ] && \
            [ test x$CM_FC_VENDOR != xG95 ] ; then
            CM_FCFLAG_MODINC=-I
            AM_CFLAGS="-Ae"
            AM_CPPFLAGS="-Dextname -DhpuxFortran"
            AM_FFLAGS="+ppu +U77"
            AM_FCFLAGS="+ppu +U77"
            AM_LDFLAGS="+U77 $LDFLAGS -lm"
            CM_NATIVE_HPUX=yes
            ac_cv_uc_modnames=yes
            CM_FC_UC_MODNAMES=yes
         fi
         ;;
    *mips*)
         if test x$CM_FC_VENDOR != xNAG ; then
            CM_FCFLAG_MODINC=-I
            AM_CFLAGS=""
            AM_CPPFLAGS=""
            AM_FFLAGS="-u"
            AM_FCFLAGS="-u"
            ac_cv_uc_modnames=yes
            CM_FC_UC_MODNAMES=yes
         fi
         ;;
    *-ibm-aix*)
         if test x$CM_FC_VENDOR != xNAG ; then
            CM_FCFLAG_MODINC=-I
            AM_CFLAGS=""
            AM_CPPFLAGS=""
            AM_FFLAGS=""
            AM_FCFLAGS=""
            CM_NATIVE_AIX=yes
            ac_cv_uc_modnames=no
            CM_FC_UC_MODNAMES=no
         fi
         ;;
    *)
         CM_FCFLAG_MODINC=-I
         ac_cv_uc_modnames=no
         CM_FC_UC_MODNAMES=no
         ;;
 esac

 case $CM_FC_VENDOR in
    G95)
         CM_FCFLAG_MODINC=-I
         AM_CPPFLAGS="-Df2cFortran"
         AM_FCFLAGS=""
         ac_cv_uc_modnames=no
         CM_FC_UC_MODNAMES=no
	 ;;
    GCC)
         CM_FCFLAG_MODINC=-I
         AM_CPPFLAGS="-DpgiFortran"
         AM_FCFLAGS=""
         ac_cv_uc_modnames=no
         CM_FC_UC_MODNAMES=no
	 ;;
    NAG)
         CM_FCFLAG_MODINC=-I
         AM_CFLAGS=""
         AM_CPPFLAGS="-DNAGf90Fortran"
         AM_FFLAGS="-dusty"
         AM_FCFLAGS=""
         ac_cv_uc_modnames=no
         CM_FC_UC_MODNAMES=no
         ;;
 esac

 AC_ARG_WITH([uppercase-modnames],
             AS_HELP_STRING([--with-uppercase-modnames],
                            [Compiler uses uppercase module names]),dnl
             [ac_cv_uc_modnames=yes],dnl
             [:])
 AC_ARG_WITH([lowercase-modnames],
             AS_HELP_STRING([--with-lowercase-modnames],
                            [Compiler uses lowercase module names]),dnl
             [ac_cv_uc_modnames=no],dnl
             [:])
 AC_CACHE_CHECK([whether compiler uses upper case module names],
                [ac_cv_uc_modnames],dnl
                [ac_cv_uc_modnames=no])
 AS_IF([test x"$ac_cv_uc_modnames" = x"no"],dnl
       [CM_FC_UC_MODNAMES="no"],dnl
       [CM_FC_UC_MODNAMES="yes"])


 AM_CONDITIONAL(CM_NATIVE_HPUX,    test x$CM_NATIVE_HPUX = xyes)
 AM_CONDITIONAL(CM_NATIVE_AIX,     test x$CM_NATIVE_AIX = xyes)
 AM_CONDITIONAL(CM_FC_UC_MODNAMES, test x$CM_FC_UC_MODNAMES = xyes)

 AC_SUBST(AM_CPPFLAGS)
 AC_SUBST(AM_CCFLAGS)
 AC_SUBST(AM_FFLAGS)
 AC_SUBST(AM_FCFLAGS)
 AC_SUBST(AM_LDFLAGS)
])# CM_COMPILER_DEFAULTS
