# $Id: cm_fortran.m4 1882 2008-10-27 15:45:52Z frhl $

#****f* cm_macros/cm_fortran.m4 *
#
# NAME
#    cm_fortran.m4 - Autoconf / automake macros for Fortran 9x.
#
# SYNOPSIS
#    Place the cm_fortran.m4 file in a subdirectory 'm4' of the top level
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
#    The file cm_fortran.m4 provides a small number of useful autoconf /
#    automake macros for working with Fortran 9x projects.
#
# NOTES
#    cm_fortran.m4 requires (at least) autoconf 2.59 and a patched version
#    of automake 1.8.2.
#
# SEE ALSO
#    cm_check_module
#    cm_require_module
#    cm_prog_fc_backslash_escape
#    cm_prog_f77_backslash_escape
#
#    cm_libraries.m4
#    cm_programs.m4
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

#****im* cm_fortran.m4/ac_lang_program *
#
# NAME
#    AC_LANG_PROGRAM - Redefinition of the standard macro for Fortran
#
# SYNOPSIS
#    AC_LANG_PROGRAM([PROLOGUE], [BODY])
# 
# DESCRIPTION
#    cm_fortran redefines the AC_LANG_PROGRAM macro of autoconf to enforce
#    Fortran 9x free form source code in testing for modules and libraries.
#
# INPUTS
#    PROLOGUE    Ignored.
#    BODY        Body of the program to be compiled.
#
# NOTES
#    The source code generated is
#
#       program main
#          [BODY]
#       end program main
#
# SEE ALSO
#    cm_require_module
#    cm_check_module
#
#    cm_compilers.m4
#    cm_libraries.m4
#
#****

# 1. AC_LANG_PROGRAM(Fortran)([PROLOGUE], [BODY])
# -----------------------------------------------

m4_undefine([AC_LANG_PROGRAM(Fortran)])
m4_define([AC_LANG_PROGRAM(Fortran)],
[m4_ifval([$1],
       [m4_warn([syntax], [$0: ignoring PROLOGUE: $1])])dnl
      program main
      $2
      end program main])


#****m* cm_fortran.m4/cm_require_module *
#
# NAME
#    CM_REQUIRE_MODULE - Check for the availability of a required module.
#
# SYNOPSIS
#    CM_REQUIRE_MODULE(modname)
# 
# DESCRIPTION
#    This macro tests if a required Fortran 9x module is available and can
#    be used for compilation.
#
# INPUTS
#    modname     Name of the required module (WITHOUT extension).
#
# OUTPUT
#    If compiling using the required module succeeds, both an environment and
#    a preprocessor variable HAVE_MODULE_<modname> will be set to 'yes' and 1,
#    respectively; ; an error occurs if not.
#
# NOTES
#    The language must be set to 'Fortran', and it's probably wise to set the
#    language extension to a reasonable suffix. The macro attempts to compile
#    the following source code:
#
#       program main
#          use <modname>
#       end program main
#
# EXAMPLE
#    To see if the netcdf module can be used, use
#
#       AC_LANG(Fortran)
#       AC_FC_SRCEXT(f90)
#
#       CM_REQUIRE_MODULE(netcdf)
#
# SEE ALSO
#    cm_check_module
#
#    cm_libraries.m4
#    cm_compilers.m4
#
# BUGS
#    Error processing is close to non-existing; no diagnostics is
#    provided in case the attempt to compile fails. If necessary,
#    the file config.log needs to be inspected for further hints.
#
#****

# 2. CM_REQUIRE_MODULE(MODULE)
# ----------------------------

AC_DEFUN([CM_REQUIRE_MODULE],
[AC_LANG_ASSERT(Fortran)dnl
 AS_VAR_PUSHDEF([ac_Module], [HAVE_MODULE_$1])dnl
 AC_CACHE_CHECK([for Fortran module $1],
                [ac_Module],
                [AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[[use $1]])],dnl
                 [AS_VAR_SET(ac_Module, yes)],
                 [AS_VAR_SET(ac_Module, no)])
                ])
 AS_IF([test AS_VAR_GET(ac_Module) = yes],,dnl
         [AC_MSG_ERROR([The $1 module is required for this library / program.])])[]dnl
 AS_VAR_POPDEF([ac_Module])dnl
])# CM_REQUIRE_MODULE


#****m* cm_fortran.m4/cm_check_module *
#
# NAME
#    CM_CHECK_MODULE - Check for the availability of a module.
#
# SYNOPSIS
#    CM_CHECK_MODULE(modname)
# 
# DESCRIPTION
#    This macro tests if a required Fortran 9x module is available and can
#    be used for compilation.
#
# INPUTS
#    modname     Name of the required module (WITHOUT extension).
#
# OUTPUT
#    If the specified module is available and can be used in compilation,
#    both a shell variable and a preprocessor variable HAVE_MODULE_<modname>
#    are set to 'yes' and '1', respectively. If the module is not available
#    for compilation, the respective variables are set to 'no' and remain
#    undefined, respectively.
#
#    Support is provided for autoheader and autoconf's config.h mechanism
#    (via AC_CONFIG_HEADERS).
#
# NOTES
#    The language must be set to 'Fortran', and it's probably wise to set the
#    language extension to a reasonable suffix. The macro attempts to compile
#    the following source code:
#
#       program main
#          use <modname>
#       end program main
#
# EXAMPLE
#    To see if the netcdf module can be used, use
#
#       AC_LANG(Fortran)
#       AC_FC_SRCEXT(f90)
#
#       CM_CHECK_MODULE(netcdf)
#
# SEE ALSO
#    cm_require_module
#
#    cm_libraries
#    cm_compilers
#
# BUGS
#    Error processing is close to non-existing; no diagnostics is
#    provided in case the attempt to compile fails. If necessary,
#    the file config.log needs to be inspected for further hints.
#
#****

# 3. CM_CHECK_MODULE(MODULE)
# --------------------------

AC_DEFUN([CM_CHECK_MODULE],
[AC_LANG_ASSERT(Fortran)dnl
 AS_VAR_PUSHDEF([ac_Module], [HAVE_MODULE_$1])dnl
 AH_TEMPLATE(AS_TR_CPP(HAVE_MODULE_[]$1),
             [Define to 1 if you have the <]$1[> fortran module.])dnl
 AC_CACHE_CHECK([for Fortran module $1],
                [ac_Module],
                [AC_COMPILE_IFELSE([AC_LANG_PROGRAM(,[[use $1]])],dnl
                 [AS_VAR_SET(ac_Module, yes)],
                 [AS_VAR_SET(ac_Module, no)],
                 )])
 AS_IF([test AS_VAR_GET(ac_Module) = yes],dnl
         [AC_DEFINE_UNQUOTED(AS_TR_CPP(HAVE_MODULE_[]$1))]dnl
      )dnl
 AS_VAR_POPDEF([ac_Module])dnl
])# CM_CHECK_MODULE


#****m* cm_fortran.m4/cm_prog_fc_backslash_escape *
#
# NAME
#    CM_PROG_FC_BACKSLASH_ESCAPE - Check whether the Fortran compiler escapes 
#                                  backslashes or not.
#
# SYNOPSIS
#    CM_PROG_BACKSLASH_ESCAPE
# 
# DESCRIPTION
#    This macro tests if a Fortran 9x compiler treats '\' as an escape
#    character (like C), or a regular character.
#
# OUTPUT
#    If the compiler treats backslashes in string variables as escape characters,
#    both a shell variable and a preprocessor variable FC_BACKSLASH_ESCAPE
#    are set to 'yes' and '1', respectively. If the compiler treats backslashes
#    as ordinary characters, the respective variables are set to 'no' and remain
#    undefined, respectively.
#
#    Support is provided for autoheader and autoconf's config.h mechanism
#    (via AC_CONFIG_HEADERS).
#
# NOTES
#    The language must be set to 'Fortran', and it's probably wise to set the
#    language extension to a reasonable suffix. The macro attempts to compile
#    the following source code (which follows Fortran 77 conventions):
#
#          program backslash
#          character string*3
#          string='\' '
#          end
#
#    This macro was written by Steven G. Johnson and submitted to the autoconf
#    mailing list in October 2004. I have slightly modified the output while
#    running, but did not add anything else to it (and hopefully have not
#    introduced any errors).
#
#    If you run into any problems with this macro, I have probably caused them;
#    please send me a bug report, but don't blame Steven.
#
# EXAMPLE
#    To check how the the current Fortran 9x compiler treats backslashes, use
#
#       AC_LANG(Fortran)
#       AC_FC_SRCEXT(f90)
#
#       CM_PROG_FC_BACKSLASH_ESCAPE
#
# SEE ALSO
#    cm_f77_backslash_escape
#
#    cm_libraries
#    cm_compilers
#
# BUGS
#    Error processing is close to non-existing; no diagnostics is
#    provided in case the attempt to compile fails. If necessary,
#    the file config.log needs to be inspected for further hints.
#
#****

# _CM_PROG_FC_BACKSLASH_ESCAPE
# ---------------------------------
#
# Find if a backslash `\' escapes in quoted strings.

AC_DEFUN([_CM_PROG_FC_BACKSLASH_ESCAPE],
[_AC_FORTRAN_ASSERT()dnl
AC_CACHE_CHECK([for Fortran whether backslash escapes], 
  [ac_cv_[]_AC_LANG_ABBREV[]_backslash_escape],
[AC_COMPILE_IFELSE([AC_LANG_SOURCE([[      program backslash
      character string*3

      string='\' '
      end
]])],
[ AS_TR_SH(ac_cv_[]_AC_LANG_ABBREV[]_backslash_escape)='yes' ],
[ AS_TR_SH(ac_cv_[]_AC_LANG_ABBREV[]_backslash_escape)='no' ])])

if test "z$AS_TR_SH(ac_cv_[]_AC_LANG_ABBREV[]_backslash_escape)" = "zyes"; then
   AC_DEFINE_UNQUOTED(AS_TR_CPP([]_AC_FC[]_backslash_escape), 
     $AS_TR_SH(ac_cv_[]_AC_LANG_ABBREV[]_backslash_escape),
     [Define to 1 if a backslash escapes in strings.])
fi

AS_TR_SH([]_AC_FC[]_BACKSLASH_ESCAPE)=$AS_TR_SH(ac_cv_[]_AC_LANG_ABBREV[]_backslash_escape)
])

# CM_PROG_FC_BACKSLASH_ESCAPE
# --------------------------------
AC_DEFUN([CM_PROG_FC_BACKSLASH_ESCAPE],
[AC_REQUIRE([AC_PROG_FC])dnl
AC_LANG_PUSH(Fortran)dnl
_CM_PROG_FC_BACKSLASH_ESCAPE
AC_LANG_POP(Fortran)dnl
])# CM_PROG_FC_BACKSLASH_ESCAPE


#****m* cm_fortran.m4/cm_prog_f77_backslash_escape *
#
# NAME
#    CM_PROG_F77_BACKSLASH_ESCAPE - Check whether the Fortran compiler escapes 
#                                   backslashes or not.
#
# SYNOPSIS
#    CM_PROG_BACKSLASH_ESCAPE
# 
# DESCRIPTION
#    This macro tests if a Fortran 77 compiler treats '\' as an escape
#    character (like C), or a regular character.
#
# OUTPUT
#    If the compiler treats backslashes in string variables as escape characters,
#    both a shell variable and a preprocessor variable FC_BACKSLASH_ESCAPE
#    are set to 'yes' and '1', respectively. If the compiler treats backslashes
#    as ordinary characters, the respective variables are set to 'no' and remain
#    undefined, respectively.
#
#    Support is provided for autoheader and autoconf's config.h mechanism
#    (via AC_CONFIG_HEADERS).
#
# NOTES
#    The language must be set to 'Fortran 77', and it's probably wise to set the
#    language extension to a reasonable suffix. The macro attempts to compile
#    the following source code (which follows Fortran 77 conventions):
#
#          program backslash
#          character string*3
#          string='\' '
#          end
#
#    This macro was written by Steven G. Johnson and submitted to the autoconf
#    mailing list in October 2004. I have slightly modified the output while
#    running, but did not add anything else to it (and hopefully have not
#    introduced any errors).
#
#    If you run into any problems with this macro, I have probably caused them;
#    please send me a bug report, but don't blame Steven.
#
# EXAMPLE
#    To check how the the current Fortran 77 compiler treats backslashes, use
#
#       AC_LANG(Fortran)
#       AC_FC_SRCEXT(f90)
#
#       CM_PROG_F77_BACKSLASH_ESCAPE
#
# SEE ALSO
#    cm_fc_backslash_escape
#
#    cm_libraries
#    cm_compilers
#
# BUGS
#    Error processing is close to non-existing; no diagnostics is
#    provided in case the attempt to compile fails. If necessary,
#    the file config.log needs to be inspected for further hints.
#
#****

# CM_PROG_F77_BACKSLASH_ESCAPE
# --------------------------------
AC_DEFUN([CM_PROG_F77_BACKSLASH_ESCAPE],
[AC_REQUIRE([AC_PROG_F77])dnl
AC_LANG_PUSH(Fortran 77)dnl
_CM_PROG_FC_BACKSLASH_ESCAPE
AC_LANG_POP(Fortran 77)dnl
])# CM_PROG_F77_BACKSLASH_ESCAPE
