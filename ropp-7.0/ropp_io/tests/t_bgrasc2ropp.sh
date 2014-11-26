#!@SHELL@
#
# $Id: t_bgrasc2ropp.sh 2905 2011-06-22 11:25:15Z idculv $
#
#****s* Tools/t_bgrasc2ropp *
#
# NAME
#    t_bgrasc2ropp - Test the bgrasc2ropp program.
#
# SYNOPSIS
#    t_bgrasc2ropp.sh
#
# DESCRIPTION
#    This shell script undertakes some tests of the bgrasc2ropp program. 
#    It uses example namelist data located in the ../data directory, 
#    (created by the sister program grib2bgrasc), and generates output in 
#    ROPP format.  The latter can then be compared with master versions of
#    these files (originally also created by this script) using NCOperators.
#
# NOTES
#   1) Assumes PWD = ropp_io/tests
#   2) Requires the fortran executable nc_diff, in this directory
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

EXEC=../tools/bgrasc2ropp

#-----------------------------------------------------------------------------

echo
echo "1. Converting Fortran namelist ascii -> ROPP netCDF"
echo "==================================================="
echo

# Time extrapolation to 0Z from GRIB2 forecasts at 6Z and 12Z
# ----------------------------------------------------------------

IFILE=../data/hy_20121001000000_T+0.nml_ref

OFILE=`basename $IFILE |sed -es/'.nml_ref'/'.nc'/`
rm $OFILE > /dev/null 2>&1

echo "$EXEC  $IFILE  -o $OFILE"

$EXEC  $IFILE  -o $OFILE

if [ ! -f $OFILE ]; then
  echo
  echo "*** Failed to generate a ROPP netCDF file from Fortran namelist ascii file: FAIL"
  exit
fi


echo
echo "2. Comparing output ROPP file with reference file"
echo "================================================="
echo

RFILE=../data/${OFILE}_ref

./nc_diff  $OFILE  $RFILE  "bgrasc2ropp"
