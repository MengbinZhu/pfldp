# $Id: cm_libraries.m4 694 2005-09-26 06:51:03Z marq $

#****f* cm_macros/cm_libraries.m4 *
#
# NAME
#    cm_libraries.m4 - Autoconf / automake macros for libraries.
#
# SYNOPSIS
#    Place the cm_libraries.m4 file in a subdirectory 'm4' of the top level
#    directory of the project, and add
#
#       ACLOCAL_AMFLAGS  = -I m4
#
#    to the toplevel Makefile.am
#
#    To generate the initial aclocal.m4, run
#
#       aclocal -I m4
#
#    and the usual sequence of automake and autoconf thereafter.
# 
# DESCRIPTION
#    The file cm_libraries.m4 provides a small number of useful autoconf /
#    automake macros for working with (external) libraries.
#
# NOTES
#    cm_libraries.m4 requires (at least) autoconf 2.59 and a patched version
#    of automake 1.8.2.
#
# SEE ALSO
#    cm_check_lib
#    cm_require_lib
#    cm_with_atlas
#    cm_with_mkl
#
#    cm_fortran.m4
#    cm_compilers.m4
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


#****m* cm_libraries.m4/cm_check_lib *
#
# NAME
#    CM_CHECK_LIB - Check for the availability of a library
#
# SYNOPSIS
#    CM_CHECK_LIB(library, routine, default, [withname])
# 
# DESCRIPTION
#    This macro provides an alternative to the standard AC_CHECK_LIB.
#
# INPUTS
#    library     Name of the library (WITHOUT -l).
#    routine     Routine to check for.
#    default     Default set of options to be tried and added
#                   to LDFLAGS.
#    withname    Specifies the name of the --with-<name> option
#                   for the configure script (defaults to libray).
#
# OUTPUT
#    If the specified routine and library are available and can be used
#    in compilation, both a shell variable and a preprocessor variable
#    HAVE_LIBRARY_<library> are set to 'yes' and '1', respectively. In
#    this case, LDFLAGS is also prepended with the linker flags.
#
#    If the library is is not available for compilation, the respective
#    variables are set to 'no' and remain undefined; the LDFLAGS variable
#    is not changed.
#
#    Support is provided for autoheader and autoconf's config.h mechanism
#    (via AC_CONFIG_HEADERS).
#
# NOTES
#    If other libraries are required for the linking, these needed
#    to be tested before.
#
#    If the library is not found, the macro does NOT issue an error, and
#    does NOT abort the configuration; it is the user's responsibility
#    to deal with this situation explicitely. In this respect, CM_CHECK_LIB
#    behaves significantly different from AC_CHECK_LIB. If you want to
#    check for the availability of a mandatory library, use CM_REQUIRE_LIB
#    instead.
#
# EXAMPLE
#    To try to link against the Blas library, with a default linker
#    flag of -lblas, use
#
#       CM_CHECK_LIB(blas, saxpy, -lblas)
#
#    The user can specify alternative linker options when running
#    configure by using
#
#       --with-blas=<linker options>
#
#    If, depending on the compiler or other options given to configure,
#    a shell variable LDBLAS is defined which contains the required
#    linker flags, the use of
#
#       CM_CHECK_LIB(blas, saxpy, $LDBLAS)
#
#    is also possible. This allows to link against vendor provided
#    libraries containing (in this case) the blas routines if something
#    is known about their existence. A possible scenario is that 
#    configure determines the systems and compiler type and sets a
#    default LDBLAS for the system it is running on.
#
# SEE ALSO
#    cm_require_lib
#    cm_with_atlas
#
#    cm_fortran.m4
#    cm_compilers.m4
#
# BUGS
#    Error processing is close to non-existing; no diagnostics is
#    provided in case the attempt to link fails. If necessary, the
#    file config.log needs to be inspected for further hints.
#
#****

# CM_CHECK_LIB(library, routine, default, [withname])
# ---------------------------------------------------

AC_DEFUN([CM_CHECK_LIB],
[dnl
OLD_LIBS="$LIBS"
AC_ARG_WITH(m4_ifval([$4],[$4],[$1]), dnl
AS_HELP_STRING(m4_ifval([$4],--with-$4=LDFLAGS,--with-$1=LDFLAGS), dnl
              [lib$1.a requires LDFLAGS (defaults to $3)]), dnl
               m4_ifval([$4],
                    [LIBS="$with_$4 $LIBS"],
                    [LIBS="$with_$1 $LIBS"]),dnl
              [LIBS="$3 $LIBS"])
AS_VAR_PUSHDEF([ac_Lib], [HAVE_LIBRARY_$1])dnl
AH_TEMPLATE(AS_TR_CPP(HAVE_LIBRARY_[]$1),
            [Define to 1 if you have the <]$1[> library.])dnl
AC_CACHE_CHECK([for $2 of the $1 library], ac_Lib,
[AC_TRY_LINK_FUNC([$2],
                 [AS_VAR_SET(ac_Lib, yes)],
                 [AS_VAR_SET(ac_Lib, no)])
])
AS_IF([test AS_VAR_GET(ac_Lib) = yes],dnl
      [AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_LIBRARY_[]$1))],dnl
      [LIBS="$OLD_LIBS"]dnl
     )dnl
AS_VAR_POPDEF([ac_Lib])dnl
])


#****m* cm_libraries.m4/cm_require_lib *
#
# NAME
#    CM_REQUIRE_LIB - Check for the availability of a library
#
# SYNOPSIS
#    CM_REQUIRE_LIB(library, routine, default, [withname])
# 
# DESCRIPTION
#    This macro provides an alternative to the standard AC_CHECK_LIB.
#
# INPUTS
#    library     Name of the library (WITHOUT -l).
#    routine     Routine to check for.
#    default     Default set of options to be tried and added
#                   to LDFLAGS.
#    withname    Specifies the name of the --with-<name> option
#                   for the configure script (defaults to libray).
#
# OUTPUT
#    LDFLAGS is prepended with the linker flags if the attempt to
#    link was successful; an error occurs if not.
#
# NOTES
#    If other libraries are required for the linking, these needed
#    to be tested before.
#
# EXAMPLE
#    To try to link against the Blas library, with a default linker
#    flag of -lblas, use
#
#       CM_REQUIRE_LIB(blas, saxpy, -lblas)
#
#    The user can specify alternative linker options when running
#    configure by using
#
#       --with-blas=<linker options>
#
#    If, depending on the compiler or other options given to configure,
#    a shell variable LDBLAS is defined which contains the required
#    linker flags, the use of
#
#       CM_REQUIRE_LIB(blas, saxpy, $LDBLAS)
#
#    is also possible. This allows to link against vendor provided
#    libraries containing (in this case) the blas routines if something
#    is known about their existence. A possible scenario is that 
#    configure determines the systems and compiler type and sets a
#    default LDBLAS for the system it is running on.
#
# SEE ALSO
#    cm_check_lib
#    cm_with_atlas
#
#    cm_fortran.m4
#    cm_compilers.m4
#
# BUGS
#    Error processing is close to non-existing; no diagnostics is
#    provided in case the attempt to link fails. If necessary, the
#    file config.log needs to be inspected for further hints.
#
#****

# CM_REQUIRE_LIB(library, routine, default, [withname])
# -----------------------------------------------------

AC_DEFUN([CM_REQUIRE_LIB],
[dnl
AC_ARG_WITH(m4_ifval([$4],[$4],[$1]), dnl
AS_HELP_STRING(m4_ifval([$4],--with-$4=LDFLAGS,--with-$1=LDFLAGS), dnl
              [lib$1.a requires LDFLAGS (defaults to $3)]), dnl
               m4_ifval([$4],
                    [LIBS="$with_$4 $LIBS"],
                    [LIBS="$with_$1 $LIBS"]),dnl
              [LIBS="$3 $LIBS"])
AS_LITERAL_IF([$1],
              [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1_$2])],
              [AS_VAR_PUSHDEF([ac_Lib], [ac_cv_lib_$1''_$2])])dnl
AC_CACHE_CHECK([for $2 of the $1 library], ac_Lib,
[AC_TRY_LINK_FUNC([$2],
                 [AS_VAR_SET(ac_Lib, yes)],
                 [AS_VAR_SET(ac_Lib, no)])
])
AS_IF([test AS_VAR_GET(ac_Lib) = yes],,dnl
      [AC_MSG_ERROR([The $1 library is required for the compilation of this library / program.])])dnl
AS_VAR_POPDEF([ac_Lib])dnl
])


#****m* cm_libraries.m4/cm_with_atlas *
#
# NAME
#    CM_WITH_ATLAS - Use the Atlas library instead of BLAS (and LAPACK).
#
# SYNOPSIS
#    CM_WITH_ATLAS()
# 
# DESCRIPTION
#    This macro provides an option --with-atlas to configure. If the
#    option is used without arguments, the shell variable LDBLAS is
#    set to hold standard linker options (-lcblas -lf77blas -latlas)
#    to link against against Atlas as alternative to the BLAS. If
#    other linker options are given in the argument to --with-atlas,
#    LDBLAS will be set to those.
#
# OUTPUT
#    Setting of the shell variable LDBLAS if --with-atlas is used
#    when running configure.
#
# NOTES
#    If --with-atlas is specified at configure time, the constructed
#    Makefiles try to link all executables against the Atlas libraries
#    instead of the standard BLAS. Because Atlas contains some, but not
#    all, of LAPACK, applications using LAPACK as well might still need
#    the standard LAPACK library. It is assumed that the LAPACK library
#    available has been properly set up or merged with Atlas in order
#    to provide the optimised LAPACK routines provided by Atlas. For
#    details, see the Atlas documentation.
#
#    This macros attempts to link all executables against the Atlas
#    libraries instead of the standatd BLAS and LAPACK ones. It does
#    not aim at providing correct paths of these libraries to the
#    linker. Thus, if the Atlas libraries are installed in non-standard
#    places, the appropriate -L options must also be given as part of
#    the LDFLAGS environment variable when configure is run.
#
#    The macro is probably best used in conjunction with CM_CHECK_LIB.
#
# EXAMPLE
#    The following code in configure.ac
#
#       LDBLAS=-lblas
#       CM_WITH_ATLAS()
#       CM_CHECK_LIB(blas, saxpy, $LDBLAS)
#
#    will cause the following behaviour of configure:
#
#       configure               will try to find the function saxpy by
#                                  linking with -lblas
#
#       configure --with-atlas  will try to find the function saxpy by
#                                  linking with -lcblas -lf77blas -ltatlas
#
#    Other than the standard linker options can be specified by giving
#    an argument to --with-atlas, i.e.
#
#       configure --with-atlas="-l... -l... ..."
#
#    Note that arguments to the option --with-blas will overwrite all
#    settings specified via --with-atlas in this example.
#
# SEE ALSO
#    cm_check_lib
#
#    cm_fortran.m4
#    cm_compilers.m4
#
#****

# CM_WITH_ATLAS
# -------------

AC_DEFUN([CM_WITH_ATLAS],
[AC_ARG_WITH([atlas],
             AS_HELP_STRING([--with-atlas],
                            [atlas instead of blas]),dnl
             [ac_cv_use_atlas=$withval],dnl
             [ac_cv_use_atlas=no])
 AC_CACHE_CHECK([whether we are using atlas instead of blas],
                [ac_cv_use_atlas],dnl
                [ac_cv_use_atlas=no])
 AS_IF([test x"$ac_cv_use_atlas" = x"no"],,dnl
       [AS_IF([test x"$ac_cv_use_atlas" = x"yes"],dnl
              [LDBLAS="-lcblas -lf77blas -latlas"],dnl
              [LDBLAS="$ac_cv_use_atlas"])
       ])
])

#****m* cm_libraries.m4/cm_with_mkl *
#
# NAME
#    CM_WITH_MKL - Use Intel's Math Kernel Library (MKL) instead of
#                     BLAS and LAPACK.
#
# SYNOPSIS
#    CM_WITH_MKL()
# 
# DESCRIPTION
#    This macro provides an option --with-mkl to configure. If the
#    option is used without arguments, the shell variable LDBLAS is
#    set to hold standard linker options (-lcblas -lf77blas -latlas)
#    to link against against MKL as alternative to any standalone Blas
#    and/or Lapack library. The shell variable LDLAPACK is set to the
#    empty string. If other linker options are given in the argument
#    to --with-mkl, these will be added to LDBLAS.
#
# OUTPUT
#    Setting of the shell variable LDBLAS and LDLAPACK if --with-mkl
#    is used when running configure.
#
# NOTES
#    If --with-mkl is specified at configure time, the constructed
#    Makefiles try to link all executables against Intel's Math
#    Kernel libraries in order to replace calls to BLAS and LAPACK.
#    Other functionality provided by the MKL is not supported.
#
#    This macro does not aim at providing correct paths to the MKL. 
#    Thus, the proper path to the installed version of the MKL must be
#    given in form of the appropriate -L arguments within the LDFLAGS
#    environment variable when configure is run.
#
#    It is probably best used in conjunction with CM_CHECK_LIB, as shown
#    in the example below.
#
# EXAMPLE
#    The following code in configure.ac
#
#       LDBLAS=-lblas
#       CM_WITH_ATLAS()
#       CM_WITH_MKL()
#       CM_CHECK_LIB(blas, saxpy, $LDBLAS)
#
#    will cause the following behaviour of configure:
#
#       configure               will try to find the function saxpy by
#                                  linking with -lblas
#
#       configure --with-atlas  will try to find the function saxpy by
#                                  linking with -lcblas -lf77blas -ltatlas
#
#    Other than the standard linker options can be specified by giving
#    an argument to --with-atlas, i.e.
#
#       configure --with-atlas="-l... -l... ..."
#
#    Note that arguments to the option --with-blas will overwrite all
#    settings specified via --with-atlas in this example.
#
# SEE ALSO
#    cm_check_lib
#
#    cm_fortran.m4
#    cm_compilers.m4
#
#****

# CM_WITH_MKL
# -----------

AC_DEFUN([CM_WITH_MKL],
[AC_ARG_WITH([mkl],
             AS_HELP_STRING([--with-mkl],
                            [Intel Math Kernel Library instead of blas and lapack]),dnl
             [ac_cv_use_mkl=$withval],dnl
             [ac_cv_use_mkl=no])
 AC_CACHE_CHECK([whether we are using MKL instead of blas and lapack],
                [ac_cv_use_mkl],dnl
                [ac_cv_use_mkl=no])
 AS_IF([test x"$ac_cv_use_mkl" = x"no"],,dnl
       [AS_IF([test x"$ac_cv_use_mkl" = x"yes"],dnl
              [LDBLAS="-lmkl_lapack -lmkl_ia32 -lguide -lpthread"
               LDLAPACK=""],dnl
              [LDBLAS="$ac_cv_use_mkl"
               LDLAPACK=""])
       ])
])
