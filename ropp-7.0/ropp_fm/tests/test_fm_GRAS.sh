#!/bin/sh
#
#****s* fm/test_fm_GRAS.sh
#
# NAME
#    test_fm_GRAS.sh - Test the ropp_fm program.
#
# SYNOPSIS
#   test_fm.sh
#
# DESCRIPTION
#   Runs the ropp_fm executable ropp_fm_bg2ro_1d.
#   Uses input netCDF files ../data/bgr*_XXXX.nc.
#   Produces output netCDF file ropp_fm_bgr20090401_single.nc
#   The FM computes refractivity and bending angle from 5 ECMWF temperature,
#   water vapor, and pressure profiles. The test runs ropp_fm_bg2ro with a
#   set of single-profile input files and (optionally) again with a 
#   multi-file containing all profiles.
#   The IDL code plots results: ropp_fm_bgr20090401_single.nc.jpg
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

for file_id in 'ropp_fm_bgr20090401' 'ropp_fm_bgr20090401_comp' ; do

echo "Working on file_id = "${file_id}

case $file_id  in

  ropp_fm_bgr20090401) extra_command=""
  ;;
  ropp_fm_bgr20090401_comp) extra_command=" -comp"
  ;;

esac


#-------------------------------------------------------------------------------
# 1. Run the ropp_fm executable
#-------------------------------------------------------------------------------

## 1.1 Run executable
if [ -f ../tools/ropp_fm_bg2ro_1d ]; then
    echo "Running ropp_fm executable (single files)..."$extra_command
    ## Single profile files version
    OUT=${file_id}"_single.nc"
  ../tools/ropp_fm_bg2ro_1d -f ../data/bgr*_XXXX.nc -o $OUT ${extra_command}
#    ## Multi-profile files version
#  OUT="ropp_fm_bgr20090401_multi.nc"
#  ../tools/ropp_fm_bg2ro_1d -f ../data/bgr20090401_multi.nc -o $OUT
else
    echo "*** 1D-FM application not found - test FAILED"
    exit
fi

## 1.2 Check output file is produced
echo " "
echo "Finished FM"
echo " "
if [[ -f $OUT ]]; then
 echo "Output file $OUT was created - test PASSED"
else
 echo "Output file $OUT was not created - test FAILED."
 exit
fi

## 1.3 Run idl to test the result
echo " "
if which idl >/dev/null 2>&1; then
    echo "*** Found IDL to check test results"
    echo "*** Running IDL"
    echo "***"
else
    echo "*** IDL not found in path. Cannot check test results in output"
    echo "*** See plot_fm.pro for hints on reading and plotting output data."
    echo "***"
    exit
fi

# The idl bit
idl 2>&1 << EOF
plot_fm,'${file_id}_single.nc', extra_command='${extra_command}'
EOF
echo "Finished IDL"
echo " "
#
## 1.4 Show results (find appropriate device)
if [[ -f ${file_id}"_single.nc.jpg" ]]; then
  echo "plot ("${file_id}"_single.nc.jpg) was produced"
  echo "Compare with reference ropp_fm_bgr20090401_single-example.jpg for validation of ROPP build"
  echo ""

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
    echo "Could not find output software to output plot - check test_fm_GRAS.sh script for options. "
  else
    echo "Showing output using $device"
    $device "${file_id}_single.nc.jpg" &
    echo "This figure should be compared to its reference version:"
    $device "${file_id}_single-example.jpg" &
    if [[ $ROPP_PAUSE != '' ]] ; then 
      echo "Hit any key to continue"
      read hak
      pids=$(ps -C $device | tail -2 | awk '{print $1}')
      kill -KILL $pids >/dev/null 2>&1
    fi
  fi

fi


echo
echo "****************************"
echo "*** test_fm_GRAS.sh PASS ***"
echo "****************************"
echo

done # end of loop over file_ids

exit
