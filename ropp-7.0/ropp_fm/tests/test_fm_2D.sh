#!/bin/sh
#
#****s* fm/test_fm_2D.sh
#
# NAME
#    test_fm_2D.sh - Test the ropp_fm 2D program.
#
# SYNOPSIS
#   test_fm_2D.sh
#
# DESCRIPTION
#   Runs the ropp_fm executable ropp_fm_bg2ro_2d.
#   Produces output netCDF file ECMWF_10_OCCS_out.nc
#   The FM computes bending angle from a ECMWF temperature,
#   water vapor, and pressure profile using a 2D operator. The test runs
#   ropp_fm_bg2ro_2d with a single-profile input file.
#   The IDL code plots results: ECMWF_10_OCCS_out.nc.jpg
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

for file_id in 'ECMWF_10_OCCS' 'ECMWF_10_OCCS_comp' ; do

echo "Working on file_id = "${file_id}

case $file_id  in

  ECMWF_10_OCCS) extra_command=""
  ;;
  ECMWF_10_OCCS_comp) extra_command=" -comp"
  ;;

esac

#-------------------------------------------------------------------------------
# 1. Run the ropp_fm executable
#-------------------------------------------------------------------------------

## 1.1 Run executable
if [ -f ../tools/ropp_fm_bg2ro_2d ]; then
    echo "Running ropp_fm executable (single file)..."$extra_command
    ## Single profile files version
    OUT=${file_id}"_out.nc"
  ../tools/ropp_fm_bg2ro_2d ../data/ECMWF_10_OCCS.nc -o $OUT ${extra_command}
#    ## Multi-profile files version
#  OUT="ropp_fm_bgr20090401_multi.nc"
#  ../tools/ropp_fm_bg2ro_2d ../data/ECMWF_10_OCCS.nc -o $OUT
else
    echo "*** 2D-FM application not found"
    exit
fi

## 1.2 Check output file is produced
echo " "
echo "Finished FM"
echo " "
if [[ -f $OUT ]]; then
 echo "Output file $OUT was created"
else
 echo "Output file $OUT was not created - test failed."
 exit
fi

## 1.3 Run idl to test the result
echo " "
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
 plot_fm_twod,'${file_id}_out.nc', extra_command='${extra_command}'
EOF
echo "Finished IDL"
echo " "
#
## 1.4 Show results (find appropriate device)
if [[ -f ${file_id}"_out.nc.jpg" ]]; then
  echo "plot ("${file_id}"_out.nc.jpg) was produced"
  echo "Compare with reference ECMWF_2D_test_output-example.jpg for validation of ROPP build"
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
    echo "Could not find output software to output plot - check test_fm_2D.sh script for options. "
  else
    echo "Showing output using $device"
    $device ${file_id}"_out.nc.jpg" &
    echo "This figure should be compared to its reference version:"
    $device "$(echo $file_id |sed -es/'ECMWF_10_OCCS'/'ECMWF_2D_test_output'/)-example.jpg" &
    if [[ $ROPP_PAUSE != '' ]] ; then 
      echo "Hit any key to continue"
      read hak
      pids=$(ps -C $device | tail -2 | awk '{print $1}')
      kill -KILL $pids >/dev/null 2>&1
    fi
  fi

fi


echo
echo "**************************"
echo "*** test_fm_2D.sh PASS ***"
echo "**************************"
echo

done # end of loop over file_ids

exit
