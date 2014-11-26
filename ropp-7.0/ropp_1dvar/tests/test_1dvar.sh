#!/bin/sh
#
#****s* 1d_var/test_1dvar.sh
#
# NAME
#    test_1dvar.sh - Test the ropp_1dvar program.
#
# SYNOPSIS
#   test_1dvar.sh
#
# DESCRIPTION
#   Runs the ropp_1dvar executable ropp_1dvar_refrac.
#   Produces output netCDF file IT-1DVAR-01.1.nc
#   The 1D-VAR retrieves temperature, water vapor, and pressure from
#   5 FASCOD and 3 ECMWF profiles with ducting, using a background
#   identical to atmospheric profiles used in generating the simulated
#   measurements. The retreives output values should therefore be identical
#   (within rounding errors) to the background. The test is deemed a
#   "Pass" if errors are within processing precision.
#   The IDL code checks the differences between input and output
#   refractivity and bending angles and produces plot of the differences:
#   ropp_1dvar_comp_01_ref.jpg
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

for file_id in '01' '01comp' ; do

echo "Working on file_id = "${file_id}

case $file_id  in

  01) extra_command=""
  ;;
  01comp) extra_command=" -comp"
  ;;

esac

#-------------------------------------------------------------------------------
# 1. Run the ropp_1dvar executable
#-------------------------------------------------------------------------------
OBS="../data/IT-1DVAR-01_y.nc"      #observations created using the background in this test
BACK="../data/IT-1DVAR-01_b.nc"     #background data
COV="../data/IT-1DVAR-01_c.nc"      #correlations for errors
OUT="../data/IT-1DVAR-"${file_id}".1.nc"      #output solution data

if [ -f ../tools/ropp_1dvar_refrac ]; then
  echo "Running ropp_1dvar executable..."${extra_command}
  ../tools/ropp_1dvar_refrac -y  $OBS -b $BACK --bg-corr $COV -o $OUT $extra_command
else
  echo "*** 1D-Var application not found - test FAILED"
  exit
fi
#
#-------------------------------------------------------------------------------
# 2.  Check output file is produced
#-------------------------------------------------------------------------------
echo " "
echo "Finished 1D-var"
echo " "
if [[ -f $OUT ]]; then
 echo "Output file $OUT was created - test PASSED"
else
 echo "Output file $OUT was not created - test FAILED"
 exit
fi
#
#-------------------------------------------------------------------------------
# 3. Run idl to test the result
#-------------------------------------------------------------------------------
#
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
#
#-------------------------------------------------------------------------------
# 4. Check jpeg is produced from IDL
#-------------------------------------------------------------------------------
  echo "Finished IDL"
  echo " "
  if [[ -f "ropp_1dvar_comp_"${file_id}"_ref.jpg" ]]; then
    echo "plot (ropp_1dvar_comp_"${file_id}"_ref.jpg) was produced"
    echo "Compare with reference ropp_1dvar_comp_"${file_id}"_ref_example.jpg for validation of ROPP build"
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
      echo "Could not find output software to output plot - check test_1dvar.sh script for options. "
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
