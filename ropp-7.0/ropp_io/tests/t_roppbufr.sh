#!/bin/sh
#
# $Id: t_roppbufr.sh 3551 2013-02-25 09:51:28Z idculv $
#
#****s* Tools/t_roppbufr *
#
# NAME
#    t_roppbufr - Test the BUFR encoder/decoder programs.
#
# SYNOPSIS
#    t_roppbufr
#
# USES
#    ropp2bufr
#    bufr2ropp
#    ncdump
#
# DESCRIPTION
#    This shell script tests ROPP functionality by performing a
#    conversion using the ropp2bufr & bufr2ropp programs.
#    It uses example data located in the ../data directory,
#    and dumps a .cdl representation of a similarly created
#    netCDF reference file.
#    The latter is then compared with reference version of the
#    CDL file using the standard Unix cmp(1) (ignoring file names
#    & processing date/times); if differences are found, a full
#    diff(1) dump is done. NB: the reference CDL file should be
#    a dump of a decoded BUFR file, _not_ directly from the master
#    netCDF, as the BUFR conversion will 'lose' several parameters
#    resulting in erroneous differences being reported.
#    NB: this script does not validate the correct translation of
#    all ROPP parameters, only that a locally-built ropp2bufr and
#    bufr2ropp programs work identically to the reference versions.
#    The following tests are run:
#      1. Convert netCDF master input file to BUFR
#      2. Binary comparison of new & master BUFR files
#      3. Convert BUFR file back to netCDF
#      4. Comparison of CDL dumps of new & reference netCDF files
#    NB The final comparison will often flag up differences between the
#       new and reference files (on last decimal place in output). Check
#       the generated difference file to determine whether more significant
#       differences exist.
#
# FILES
#   The test reference files used are:
#      ../data/ropp_test.nc
#      ../data/ropp_test.bfr (or ../data/ropp_test.ecbfr)
#      ../data/ropp_testb.nc
#
# AUTHOR
#   Met Office, Exeter, UK.
#   Any comments on this software should be given via the ROM SAF
#   Helpdesk at http://www.romsaf.org
#
# COPYRIGHT
#   (c) EUMETSAT. All rights reserved.
#   For further details please refer to the file COPYRIGHT
#   which you should have received as part of this distribution.
#
#****

#-------------------------------------------------------------------------------
# 0. Set up files to use in the test; wipe any old local ones
#    from a prior test; show program versions
#-------------------------------------------------------------------------------

export PRINT_TABLE_NAMES=false

bindir=../tools

ref_ncdf=../data/ropp_test.nc
ref_bufr=../data/ropp_test.bfr
bfr_ncdf=../data/ropp_testb.nc

tst_ncdf=bufr_test.nc
tst_bufr=bufr_test.bfr
tst_diff=bufr_test.dif

ref_dump=ropp_test.cdl
bfr_dump=ropp_test.cdl
tst_dump=bufr_test.cdl

rm $tst_ncdf $tst_bufr $tst_diff \
   $ref_dump $bfr_dump $tst_dump \
   > /dev/null 2>&1

ecbufr=$($bindir/ropp2bufr -v | grep -c "ECMWF")
if [ $ecbufr -gt 0 ] && [ -f ../data/ropp_test.ecbfr ]; then
  ref_bufr=../data/ropp_test.ecbfr
fi

unset NCDUMP
if which ncdump >/dev/null 2>&1; then
  export NCDUMP=`which ncdump`
  echo \$NCDUMP = $NCDUMP
else
  if which $ROPP_ROOT/*/bin/ncdump >/dev/null 2>&1; then
    export NCDUMP=`ls -ut $ROPP_ROOT/*/bin/ncdump |head -1`
    echo \$NCDUMP = $NCDUMP
  else
    echo 'ncdump not found in $PATH or under $ROPP_ROOT/*/bin'
    echo 'Will be unable to carry out comparisons of netCDF data'
  fi
fi

#-------------------------------------------------------------------------------
# 1. Convert a ROPP netCDF file to BUFR & compare with reference BUFR
#-------------------------------------------------------------------------------

echo
echo "1. Encoding ROPP netCDF -> BUFR"
echo "==============================="
$bindir/ropp2bufr $ref_ncdf -o $tst_bufr

if [ ! -f $tst_bufr ]; then
  echo "*** Failed to generate a BUFR file from netCDF"
  echo
  echo "***********************"
  echo "*** BUFR test: FAIL ***"
  echo "***********************"
  echo
  exit
fi

echo
echo "2. Comparing BUFR with reference file"
echo "====================================="
echo
cmp $ref_bufr $tst_bufr
if [ $? -eq 0 ]; then
  echo " OK -  found no differences in the test BUFR files"
else
  echo " *** Differences found in test BUFR files (continuing test)"
###  echo " *** Differences found in test BUFR files: FAIL"
###  exit
fi

#-------------------------------------------------------------------------------
# 2. Convert BUFR back to netCDF & compare with the reference netCDF file
#    (comparison in dumped CDL format)
#-------------------------------------------------------------------------------

echo
echo "3. Decoding BUFR --> ROPP netCDF"
echo "================================"
echo
$bindir/bufr2ropp $tst_bufr -o $tst_ncdf -n1
#
if [ ! -f $tst_ncdf ]; then
  echo "*** Failed to generate a netCDF file from BUFR"
  echo
  echo "***********************"
  echo "*** BUFR test: FAIL ***"
  echo "***********************"
  echo
  exit
fi

echo
echo "4. Comparing CDL dump with reference file"
echo "========================================="
echo
echo "$NCDUMP $bfr_ncdf > $bfr_dump"
$NCDUMP -p 5,5 $bfr_ncdf > $bfr_dump
echo "$NCDUMP $tst_ncdf > $tst_dump"
$NCDUMP -p 5,5 $tst_ncdf > $tst_dump
echo
echo "To check results, compare $bfr_dump and $tst_dump"
echo "(Ignore the difference in file names and processing_date value)"
echo
grep -v "^netcdf .*{" $bfr_dump | grep -v "processing_date" > x.cdl
grep -v "^netcdf .*{" $tst_dump | grep -v "processing_date" > y.cdl
if cmp -s x.cdl y.cdl; then
  echo "***********************"
  echo "*** BUFR test: PASS ***"
  echo "***********************"
else
#  echo
#  echo "***********************"
#  echo "*** BUFR test: FAIL ***"
#  echo "***********************"
#  echo
  diff -c $bfr_dump $tst_dump > $tst_diff
  echo "*** Differences found; please review $tst_diff to check significance."
  echo "    Differences of +/-1 in the last decimal place can be expected and"
  echo "    these are not considered to be significant."
  echo
fi
rm -f x.cdl y.cdl
echo
#
exit
