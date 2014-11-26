#!@SHELL@
#
# $Id: t_grib2bgrasc.sh 2905 2011-06-22 11:25:15Z idculv $
#
#****s* Tools/t_grib2bgrasc *
#
# NAME
#    t_grib2bgrasc - Test the grib2bgrasc program.
#
# SYNOPSIS
#    t_grib2bgrasc.sh
#
# DESCRIPTION
#    This shell script undertakes some tests of the grib2bgrasc program. 
#    It uses example grib data located in the ../data directory, and creates 
#    .nml output.  The latter can then be compared with master versions of
#    these files (originally also created by this script) using the program
#    nml_diff, in this directory.
#
# NOTES
#   1) Assumes PWD = ropp_io/tests
#   2) Requires the fortran executable nml_diff, in this same directory
#   3) Calculation of undulations requires the environment variables
#      GEOPOT_COEF and GEOPOT_CORR to be set correctly.
#
# REFERENCES
#   See the ROPP User Guide (SAF/ROM/METO/UG/ROPP/002)
#
#****

if test -s nml_diff >/dev/null 2>&1; then
  echo " "
else
  echo "*** nml_diff not found in this directory - test NOT PERFORMED"
  exit
fi

if [ "$(echo $(basename $(dirname $PWD)) |cut -c1-7)"/"$(basename $PWD)" != "ropp_io/tests" ] ; then
  echo "*** Not in ropp_io*/tests subdirectory - test NOT PERFORMED"
  exit
fi

#-----------------------------------------------------------------------------

# These env vars are needed to generate undulations. 
# The ones in the ropp_pp module of this distribution may not yet be unzipped, 
# so allow the user to specify their own.

export GEOPOT_COEF=${GEOPOT_COEF:-"../../ropp_pp/data/egm96.dat"}
export GEOPOT_CORR=${GEOPOT_CORR:-"../../ropp_pp/data/corrcoef.dat"}

EXEC=../tools/grib2bgrasc

#-----------------------------------------------------------------------------

echo
echo "1. Converting ECMWF GRIB -> Fortran namelist ascii"
echo "=================================================="
echo

# Time extrapolation to 0Z from GRIB2 forecasts at 6Z and 12Z
# ----------------------------------------------------------------

IFILE=../data/fc_20121001000000_T+6.grib

IFILE2=../data/fc_20121001000000_T+12.grib

ZFILE=../data/an_20121001000000_T+0.grib

lat=50.72

lon=-3.53

date=20121001

time=0000

OFILE=hy_20121001000000_T+0.nml
rm $OFILE > /dev/null 2>&1

echo "$EXEC  $IFILE  -lat $lat  -lon $lon  -date $date  -time $time  -g $IFILE2  -z $ZFILE  -o $OFILE"

$EXEC  $IFILE  -lat $lat  -lon $lon  -date $date  -time $time  -g $IFILE2  -z $ZFILE  -o $OFILE

if [ ! -f $OFILE ]; then
  echo
  echo "*** Failed to generate a Fortran namelist ascii file from GRIB file: FAIL"
  exit
fi


echo
echo "2. Comparing output namelist with reference file"
echo "================================================"
echo

RFILE=../data/${OFILE}_ref

./nml_diff  $OFILE  $RFILE  "grib2bgrasc test:"


