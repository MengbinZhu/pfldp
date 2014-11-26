#!@SHELL@
#
# $Id: t_gfz2ropp.sh 3551 2013-02-25 09:51:28Z idculv $
#
#****s* Tools/t_gfz2ropp *
#
# NAME
#    t_gfz2ropp - Test the gfz2ropp program.
#
# SYNOPSIS
#    t_gfz2ropp
#
# USES
#    gfz2ropp
#    ncdump
#
# DESCRIPTION
#    This shell script tests ROPP functionality by performing a
#    conversion using the gfz2ropp program. It uses example data
#    located in the ../data directory, and dumps a .cdl
#    representations of the reference and converted netCDF files.
#    The latter are then compared using the standard Unix cmp(1)
#    (ignoring file names & processing date/times); if differences
#    are found, a full diff(1) dump is done.
#    NB: this script does not validate the correct translation of
#    all ROPP parameters, only that a locally-built gfz2ropp works
#    identically to the reference version.
#    Warnings about reversing the order of profiles may safely be
#    ignored.
#
# FILES
#   The test reference files used are:
#      ../data/gfz_test.dat  - original GFZ data file
#      ../data/gfz_test.dsc  - original GFZ description file
#      ../data/gfz_test.nc   - GFZ file pair pre-converted to ROPP netCDF
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
# 0. Set up files & wipe any from a previous test
#-------------------------------------------------------------------------------

bindir=../tools

ref_data=../data/gfz_test.dat
ref_desc=../data/gfz_test.dsc
ref_ropp=../data/gfz_test.nc
ref_dump=../data/gfz_test.cdl

tst_ropp=gfz_test.nc
tst_dump=gfz_test.cdl
tst_diff=gfz_test.dif

rm $ref_dump \
   $tst_dump $tst_ropp $tst_diff \
   > /dev/null 2>&1

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
# 1. Convert a GFZ file pair to ROPP netCDF & dump it to CDL
#-------------------------------------------------------------------------------

echo
echo "1. Converting GFZ file pair -> ROPP netCDF"
echo "=========================================="
echo
$bindir/gfz2ropp $ref_data -o $tst_ropp

if [ ! -f $tst_ropp ]; then
  echo
  echo "*** Failed to generate a ROPP netCDF file from GFZ files: FAIL"
  exit
fi

echo
echo "Generating CDL dumps..."
echo "$NCDUMP $ref_ropp > $ref_dump"
$NCDUMP -p 5,5 $ref_ropp > $ref_dump
echo "$NCDUMP $tst_ropp > $tst_dump"
$NCDUMP -p 5,5 $tst_ropp > $tst_dump

#-------------------------------------------------------------------------------
# 2. Compare the refrence & test CDL files
#-------------------------------------------------------------------------------

echo
echo "2. Comparing CDL dump against reference file"
echo "============================================"
echo
echo "To check results, compare $ref_dump and $tst_dump"
echo "(Ignore the difference in file names and processing_date value)"
echo
grep -v "^netcdf .*{" $ref_dump | grep -v "processing_date" > x.cdl
grep -v "^netcdf .*{" $tst_dump | grep -v "processing_date" > y.cdl
if cmp -s x.cdl y.cdl; then
  echo
  echo "***************************"
  echo "*** gfz2ropp test: PASS ***"
  echo "***************************"
  echo
else
  echo
  echo "***************************"
  echo "*** gfz2ropp test: FAIL ***"
  echo "***************************"
  echo
  diff -c $ref_dump $tst_dump > $tst_diff
  echo "*** Differences found; please review $tst_diff to check significance"
  echo
fi
rm -f x.cdl y.cdl
#
exit
