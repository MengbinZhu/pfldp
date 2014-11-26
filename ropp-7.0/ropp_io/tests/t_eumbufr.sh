#!@SHELL@
#
# $Id: t_eum2bufr.sh 2905 2011-06-22 11:25:15Z idculv $
#
#****s* Tools/t_eum2bufr *
#
# NAME
#    t_eum2bufr - Test the eum2bufr program.
#
# SYNOPSIS
#    t_eum2bufr.sh
#
# DESCRIPTION
#    This shell script undertakes some tests of the eum2bufr program. 
#    It uses example eum format nc4 data located in the ../data directory, 
#    and creates "standard" ECMWF BUFR output.  The latter can then be compared 
#    with master versions of these files (originally also created by this 
#    script) using the standard unix utility cmp.
#
# NOTES
#   1) Assumes PWD = ropp_io/tests
#
# REFERENCES
#   See the ROPP User Guide (SAF/ROM/METO/UG/ROPP/002)
#
#****

if [ "$(echo $(basename $(dirname $PWD)) |cut -c1-7)"/"$(basename $PWD)" != "ropp_io/tests" ] ; then
  echo "*** Not in ropp_io*/tests subdirectory - test NOT PERFORMED"
  exit
fi

#-----------------------------------------------------------------------------

EXEC=../tools/eum2bufr

#-----------------------------------------------------------------------------

#-----------------------------------------------------------------------------
# 1. Convert an EUM netCDF4 file to BUFR & compare with reference BUFR
#-----------------------------------------------------------------------------

echo
echo "1. Converting EUM netCDF4 file -> BUFR file"
echo "==========================================="
echo

# Extraction of all Lev1a data
# ----------------------------

IFILE=../data/eum_test.n4

OFILE=eum_test_l.bfr
rm $OFILE > /dev/null 2>&1

echo "$EXEC  $IFILE  -o $OFILE"
      $EXEC  $IFILE  -o $OFILE

if [ ! -f $OFILE ]; then
  echo
  echo "*** Failed to generate a BUFR file: FAIL"
  exit
fi


echo
echo "2. Comparing output BUFR file with reference file"
echo "================================================="
echo

RFILE=../data/${OFILE}_ref

echo "Using cmp to compare $OFILE and $RFILE"
cmp  $OFILE  $RFILE

if [ $? -eq 0 ]; then
  echo " "
  echo " *** No differences found in test BUFR files"
  echo " "
  echo "******************************"
  echo "*** eum2bufr    test: PASS ***"
  echo "******************************"
else
  echo " "
  echo " *** Differences found in test BUFR files"
  echo " "
  echo "******************************"
  echo "*** eum2bufr    test: FAIL ***"
  echo "******************************"
fi


#-----------------------------------------------------------------------------
# 2. Check that eum2bufr = eum2ropp | ropp2bufr, if possible
#-----------------------------------------------------------------------------

if [ -f ../tools/eum2ropp ] && [ -f ../tools/ropp2bufr ] ; then

echo
echo "3. Converting EUM netCDF4 file -> BUFR file"
echo "==========================================="
echo

# Extraction of all Lev1b data
# ----------------------------

IFILE=../data/eum_test.n4

OFILE1=eum_test_b.bfr
rm $OFILE1 > /dev/null 2>&1

echo "$EXEC  $IFILE  -o $OFILE1"
      $EXEC  $IFILE  -o $OFILE1

if [ ! -f $OFILE1 ]; then
  echo
  echo "*** Failed to generate a BUFR file: FAIL"
  exit
fi

echo
echo "4. Comparing eum2bufr and eum2ropp | ropp2bufr"
echo "=============================================="
echo

OFILE2=eum_test_b.nc
rm $OFILE2 > /dev/null 2>&1

echo "../tools/eum2ropp  $IFILE  -b  -o $OFILE2"
      ../tools/eum2ropp  $IFILE  -b  -o $OFILE2

if [ ! -f $OFILE2 ]; then
  echo
  echo "*** Failed to generate a ROPP file: FAIL"
  exit
fi

OFILE3=eum_test_b_viaropp.bfr
rm $OFILE3 > /dev/null 2>&1

echo "../tools/ropp2bufr  $OFILE2  -o $OFILE3"
      ../tools/ropp2bufr  $OFILE2  -o $OFILE3

if [ ! -f $OFILE3 ]; then
  echo
  echo "*** Failed to generate a ROPP file: FAIL"
  exit
fi

echo "Using cmp to compare $OFILE1 and $OFILE3"
cmp  $OFILE1  $OFILE3

if [ $? -eq 0 ]; then
  echo " "
  echo " *** No differences found in test BUFR files"
  echo " "
  echo "******************************"
  echo "*** eum2bufrb   test: PASS ***"
  echo "******************************"
else
  echo " "
  echo " *** Differences found in test BUFR files"
  echo " "
  echo "******************************"
  echo "*** eum2bufrb   test: FAIL ***"
  echo "******************************"
fi


fi

