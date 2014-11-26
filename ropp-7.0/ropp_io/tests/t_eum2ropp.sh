#!@SHELL@
#
# $Id: t_eum2ropp.sh 2905 2011-06-22 11:25:15Z idculv $
#
#****s* Tools/t_eum2ropp *
#
# NAME
#    t_eum2ropp - Test the eum2ropp program.
#
# SYNOPSIS
#    t_eum2ropp.sh
#
# DESCRIPTION
#    This shell script undertakes some tests of the eum2ropp program. 
#    It uses example eum format nc4 data located in the ../data directory, 
#    and creates "standard" ropp nc3 output.  The latter can then be compared 
#    with master versions of these files (originally also created by this 
#    script) using the program nc_diff, in this directory.
#
# NOTES
#   1) Assumes PWD = ropp_io/tests
#   2) Requires the fortran executable nc_diff, in this same directory
#
# REFERENCES
#   See the ROPP User Guide (SAF/ROM/METO/UG/ROPP/002)
#
#****

if test -s nc_diff >/dev/null 2>&1; then
  echo " "
else
  echo "*** nc_diff not found in this directory - test NOT PERFORMED"
  exit
fi

if [ "$(echo $(basename $(dirname $PWD)) |cut -c1-7)"/"$(basename $PWD)" != "ropp_io/tests" ] ; then
  echo "*** Not in ropp_io*/tests subdirectory - test NOT PERFORMED"
  exit
fi

#-----------------------------------------------------------------------------

EXEC=../tools/eum2ropp

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# 1. Convert an EUM netCDF4 file to ROPP & compare with reference ROPP
#-----------------------------------------------------------------------------

echo
echo "1. Converting EUM netCDF4 file -> ROPP file"
echo "==========================================="
echo

# Extraction of all Lev1a data
# ----------------------------

IFILE=../data/eum_test.n4

OFILE=eum_test_l.nc
rm $OFILE > /dev/null 2>&1

echo "$EXEC  $IFILE  -l  -o $OFILE"
      $EXEC  $IFILE  -l  -o $OFILE

if [ ! -f $OFILE ]; then
  echo
  echo "*** Failed to generate a ROPP file: FAIL"
  exit
fi

echo
echo "2. Comparing output ROPP file with reference file"
echo "================================================="
echo

RFILE=../data/${OFILE}_ref

./nc_diff  $OFILE  $RFILE  "eum2ropp"


#-----------------------------------------------------------------------------
# 2. Check that eum2ropp = eum2bufr | bufr2ropp, if possible
#-----------------------------------------------------------------------------

if [ -f ../tools/eum2bufr ] && [ -f ../tools/bufr2ropp ] ; then

echo
echo "3. Converting EUM netCDF4 file -> ROPP file"
echo "==========================================="
echo

# Extraction of all Lev1b data
# ----------------------------

IFILE=../data/eum_test.n4

OFILE1=eum_test_b.nc
rm $OFILE1 > /dev/null 2>&1

echo "$EXEC  $IFILE  -b  -o $OFILE1"
      $EXEC  $IFILE  -b  -o $OFILE1

if [ ! -f $OFILE1 ]; then
  echo
  echo "*** Failed to generate a ROPP file: FAIL"
  exit
fi

echo
echo "4. Comparing eum2ropp and eum2bufr | bufr2ropp"
echo "=============================================="
echo

OFILE2=eum_test_b.bfr
rm $OFILE2 > /dev/null 2>&1

echo "../tools/eum2bufr  $IFILE  -o $OFILE2"
      ../tools/eum2bufr  $IFILE  -o $OFILE2

if [ ! -f $OFILE2 ]; then
  echo
  echo "*** Failed to generate a BUFR file: FAIL"
  exit
fi

OFILE3=eum_test_b_viabufr.nc
rm $OFILE3 > /dev/null 2>&1

echo "../tools/bufr2ropp  $OFILE2  -o $OFILE3"
      ../tools/bufr2ropp  $OFILE2  -o $OFILE3

if [ ! -f $OFILE3 ]; then
  echo
  echo "*** Failed to generate a ROPP file: FAIL"
  exit
fi


./nc_diff  $OFILE1  $OFILE3  "eum2roppb"

fi
