#!/bin/sh
#
#****s* 1d_var/test_1dvar_GRAS.sh
#
# NAME
#    test_1dvar_GRAS.sh - Test the ropp_1dvar program.
#
# SYNOPSIS
#   test_1dvar_GRAS.sh
#
# DESCRIPTION
#   Runs the ropp_1dvar executables ropp_1dvar_refrac and ropp_1dvar_bangle.
#   Produces output netCDF file IT-1DVAR-05.1.nc
#   The 1D-VAR retrieves temperature, water vapor, and pressure from
#   10 GRAS refractivity and bending angle profiles and co-located ECMWF
#   background fields.
#   The IDL code checks the differences between input and output
#   refractivity and bending angles and produces plot of the differences:
#   ropp_1dvar_comp_05*.jpg
#   These are compared with the reference results. By default, the output 
#   images are displayed without pausing. To make the script wait between 
#   the display of each pair of images, export ROPP_PAUSE=TRUE
#   before running this script.  With this option, each pair of images 
#   is deleted before the next pair are displayed.
#
# OPTIONS
#
# NOTES
#   1) Requires IDL to be installed to perform the check & generate plots
#   2) Checks through a list of viewers to display the jpg plots. Insert a
#      locally available viewer if not in the list.
#
# EXAMPLE
#
# REFERENCES
#   See the ROPP User Guide (SAF/ROM/METO/UG/ROPP/002)
#
#****

# Loop over options to test

for file_id in '05' '05comp' ; do

echo "Working on file_id = "${file_id}

case $file_id  in

  05) extra_command=""
  ;;
  05comp) extra_command=" -comp"
  ;;

esac

#-------------------------------------------------------------------------------
# 1. Run the ropp_1dvar bending angle executable
#-------------------------------------------------------------------------------

OBS="../data/IT-1DVAR-05_y.nc"              #observations data
BACK="../data/IT-1DVAR-05_b.nc"             #background data
COV="../data/IT-1DVAR-05_c.nc"              #correlations for bg errors

OUT="../data/IT-1DVAR-"${file_id}".1.nc"    #output solution data
CONFIG="../config/ecmwf_bangle_1dvar.cf"    #configuration file

## 1.1 Run executable

if [ -f ../tools/ropp_1dvar_bangle ]; then
  echo "Running ropp_1dvar executable..."${extra_command}
  ../tools/ropp_1dvar_bangle -y  $OBS -b $BACK --bg-corr $COV -o $OUT -c $CONFIG ${extra_command}

else
  echo "*** 1D-Var bending angle application not found - test FAILED"
  exit
fi

## 1.2 Check output file is produced
echo " "
echo "Finished 1D-var"
echo " "
if [[ -f $OUT ]]; then
 echo "Output file $OUT was created - test PASSED"
else
 echo "Output file $OUT was not created - test FAILED"
 exit
fi

## 1.3 Run idl to test the result
echo " "
if which idl >/dev/null 2>&1; then
  echo "*** Found IDL to check test results"
  echo "*** Running IDL"
  echo "***"

# The idl bit
#export DISPLAY=""
  idl 2>&1 << EOF
  plot_1dvar,'${file_id}', extra_command='${extra_command}'
EOF
  echo "Finished IDL"
  echo " "
#
## 1.4 Show results
  if [[ -f "ropp_1dvar_comp_"${file_id}"_ba.jpg" ]]; then
    echo "plot (ropp_1dvar_comp_"${file_id}"_ba.jpg) was produced"
    echo "Compare with reference ropp_1dvar_comp_"${file_id}"_ba_example.jpg for validation of ROPP build"
    echo ""

    # Check for output viewer device
    device=" "
    if which xv >/dev/null 2>&1; then
      device="xv"
    elif which display >/dev/null 2>&1; then
      device="display"
    elif which eog >/dev/null 2>&1; then
      device="eog"
    elif which gthumb >/dev/null 2>&1; then
      device="gthumb"
    fi

    if [[ $device = " " ]] ; then
      echo "Could not find output software to output plot - check test_1dvar_GRAS.sh script for options. "
    else
      echo "Showing output using $device"
      $device ropp_1dvar_comp_"${file_id}"_ba.jpg &
      echo "This figure should be compared to its reference version:"
      $device ropp_1dvar_comp_"${file_id}"_ba_example.jpg &
      if [[ $ROPP_PAUSE != '' ]] ; then 
        echo "Hit any key to continue"
        read hak
        pids=$(ps -C $device | tail -2 | awk '{print $1}')
        kill -KILL $pids >/dev/null 2>&1
      fi
    fi

  fi

else

  echo "*** IDL not found in path. Cannot check test results in output"
  echo "*** See plot_fm.pro for hints on reading and plotting output data."
  echo "***"

fi

#
#-------------------------------------------------------------------------------
# 2. Run the ropp_1dvar refractivity executable
#-------------------------------------------------------------------------------

OBS="../data/IT-1DVAR-05_y.nc"              #observations data
BACK="../data/IT-1DVAR-05_b.nc"             #background data
COV="../data/IT-1DVAR-05_c.nc"              #correlations for bg errors
COB="../data/IT-1DVAR-05_o.nc"              #correlations for ob errors

OUT="../data/IT-1DVAR-"${file_id}".1.nc"    #output solution data
CONFIG="../config/ecmwf_refrac_1dvar.cf"    #configuration file

## 2.1 Run executable

if [ -f ../tools/ropp_1dvar_refrac ]; then
  echo "Running ropp_1dvar executable..."
  ../tools/ropp_1dvar_refrac -y  $OBS -b $BACK --bg-corr $COV --obs-corr $COB -o $OUT -c $CONFIG ${extra_command}

else
  echo "*** 1D-Var refractivity application not found - test FAILED"
  exit
fi

## 2.2 Check output file is produced
echo " "
echo "Finished 1D-var"
echo " "
if [[ -f $OUT ]]; then
 echo "Output file $OUT was created - test PASSED"
else
 echo "Output file $OUT was not created - test FAILED"
 exit
fi

## 2.3 Run idl to test the result
echo " "
if which idl >/dev/null 2>&1; then
  echo "*** Found IDL to check test results"
  echo "*** Running IDL"
  echo "***"

# The idl bit
#export DISPLAY=""
  idl 2>&1 << EOF
  plot_1dvar,'${file_id}', extra_command='${extra_command}'
EOF
  echo "Finished IDL"
  echo " "
#

## 2.4 Show results
  if [[ -f "ropp_1dvar_comp_"${file_id}"_ref.jpg" ]]; then
    echo "plot (ropp_1dvar_comp_"${file_id}"_ref.jpg) was produced"
    echo "Compare with reference ropp_1dvar_comp_"${file_id}"_ref_example.jpg for validation of ROPP build"
    echo " "
    if [[ $device = " " ]] ; then
      echo "Could not find output software to output plot - check test_1dvar_GRAS.sh script for options. "
    else
      echo "Showing output using $device"
      $device ropp_1dvar_comp_"${file_id}"_ref.jpg &
      echo "This figure should be compared to its reference version:"
      $device ropp_1dvar_comp_"${file_id}"_ref_example.jpg &
      if [[ $ROPP_PAUSE != '' ]] ; then 
        echo "Hit any key to continue"
        read hak
        pids=$(ps -C $device | tail -2 | awk '{print $1}')
        kill -KILL $pids >/dev/null 2>&1
      fi
    fi

  fi

else

  echo "*** IDL not found in path. Cannot check test results in output"
  echo "*** See plot_fm.pro for hints on reading and plotting output data."
  echo "***"

fi

done # end of loop over file_ids



exit
