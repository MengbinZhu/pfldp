# $Id: $

#****f* cm_macros/cm_tools.m4 *
#
# NAME
#    cm_tools.m4 - Generic tools for automake and autoconf.
#
# SYNOPSIS
#    Place cm_tools.m4 (and the other files in the cm_tools/m4 directory)
#    into a subdirectory 'm4' of the top level directory of the project,
#    and add
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
#    The file cm_tools.m4 provides useful autoconf / automake macro for
#    for various tasks.
#
# NOTES
#    cm_tools.m4 requires (at least) autoconf 2.59 and automake 1.9.5 or
#    above.
#
# SEE ALSO
#    cm_ensure
#
#    cm_compiler_defaults.m4
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

#****m* cm_tools.m4/cm_ensure *
#
# NAME
#    CM_ENSURE - Ensure that a variable contains a given string and gets AC_SUBST'd.
#
# SYNOPSIS
#    CM_ENSURE(VARIABLE, STRING)
# 
# DESCRIPTION
#    This macro generates ensures that (shell) variables will be AC_SUBST'd and
#    contain a given substring. This can be used to e.g., enforce certain
#    compiler options.
#
# INPUT
#
# OUTPUT
#    This macro will ensure that STRING is contained in VARIABLE; if VARIABLE
#    does notain it, STRING will be added to VARIABLE. The latter will also be
#    declared to be AC_SUBST'd.
#
# NOTES
#    The second argument is optional; if no STRING is given, VARIABLE will only
#    be AC_SUBST'd.
#
#    This macro is copied from Unidata's acsite.m4 for udunits.
#
# EXAMPLE
#    (to be given...)
#
# SEE ALSO
#    cm_compiler_defaults.m4
#    cm_compilers.m4
#    cm_fortran.m4
#    cm_libraries.m4
#
#****

# 1. CM_ENSURE
# ------------

AC_DEFUN([CM_ENSURE], [dnl
ifelse($2, , [dnl
  $1=${$1-}
], [dnl
  for arg in $2; do
    case "$$1" in
      *$arg*[)]
        ;;
      *[)]
        $1="${$1-} $arg"
        ;;
    esac
  done
])dnl
AC_SUBST($1)dnl
]dnl
)
