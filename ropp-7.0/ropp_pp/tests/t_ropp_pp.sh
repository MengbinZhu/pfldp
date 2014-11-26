# !@SHELL@
#
# $Id: t_ropp_pp.sh 1640 2008-07-09 13:56:40Z frdo $
#
#****s* Tools/t_ropp_pp *
#
# NAME
#    t_ropp_pp - Test the ropp_pp program.
#
# SYNOPSIS
#    t_ropp_pp <options>
#
# DESCRIPTION
#    This shell script tests the ROPP_PP functionality by performing several
#    retrievals using the ropp_pp_invert_tool, ropp_pp_grasrs2ropp, 
#    ropp_pp_occ_tool and ropp_pp_spectra_tool programs. It uses example data located
#    in the ../data directory, and plots the results using IDL (if available).
#    These are compared with the reference results. By default, the output 
#    images are displayed without pausing. To make the script wait between 
#    the display of each pair of images, export ROPP_PAUSE=TRUE
#    before running this script.  With this option, each pair of images 
#    is deleted before the next pair are displayed.
#
# USES
#   ropp_pp_tool
#   ncdump
#
# FILES
#   The tests use reference files from the ropp_io/data directory:
#      ../data/ropp_pp_test.nc         -  ROPP netCDF master file (single profile)
#      ../data/ropp_pp_grasrs_test.nc  -  GRAS raw sampling data netCDF file (single profile)
#
# DESCRIPTION
#    Output files should contain similar data to the input files. 
#    The IDL procedures read both, compare them and plot the differences. 
#    These difference plots should look the same as the corresponding reference.
#
# REFERENCES
#   ROPP Interface file format.
#   Ref: SAF/GRAS/METO/FMT/ROPP/001
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
# 0. Ensure any old files are wiped
#-------------------------------------------------------------------------------
rm -f ropp_pp_test_*.* ROanalysis_*_L?.dat > /dev/null 2>&1

#-------------------------------------------------------------------------------
# 1. Test single profile file [INVERT]
#-------------------------------------------------------------------------------

echo
echo "Test 1: Single file L1, L2 bending angle --> refractivity"
echo "-----------------------------------------------------------------------"

# 1.1 Run tool
OUT=ropp_pp_test_1m.nc
../tools/ropp_pp_invert_tool ../data/ropp_pp_test.nc -o $OUT

# 1.2 Check output file is produced

echo
echo "Finished pp"
echo
if [[ -f $OUT ]]; then
  echo "Output file $OUT was created - test PASSED"
else
  echo "Output file $OUT was not created - test FAILED"
  exit
fi

# 1.3 Run idl to test the result
echo
if which idl >/dev/null 2>&1; then
  echo "*** Found IDL to check test results"
  echo "*** Running IDL"
  echo "***"

# The idl bit
  idl 2>&1 << EOF
  it_pp_01
EOF
#
  echo "Finished IDL"

# 1.4 Show results (find appropriate device)
  if [[ -f "ropp_pp_comp.jpg" ]]; then
    cp ropp_pp_comp.jpg ropp_pp_comp1.jpg
    echo "plot (ropp_pp_comp1.jpg) was produced"
    echo "Compare with reference ropp_pp_comp1-example.jpg for "
    echo "validation of ROPP build"
    echo

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

    if [[ $device = " " ]]; then
      echo "Could not find output software to output plot"
      echo "  - check t_ropp_pp.sh script for options. "
    else
      echo "Showing output using $device"
      $device "ropp_pp_comp1.jpg" &
      echo "This figure should be compared to its reference version:"
      $device "ropp_pp_comp1-example.jpg" &
      if [[ $ROPP_PAUSE != '' ]] ; then 
        echo "Hit any key to continue"
        read hak
        pids=$(ps -C $device | tail -2 | awk '{print $1}')
        kill -KILL $pids >/dev/null 2>&1
      fi
    fi

  fi

else
  echo "***"
  echo "*** IDL not found in path. Cannot check test results in output"
  echo "*** See plot_fm.pro for hints on reading and plotting output data."
  echo "***"
fi

#-------------------------------------------------------------------------------
# 2. Test single profile file [OCCULTATION]
#-------------------------------------------------------------------------------

echo
echo "Test 2: Single file L1, L2 phase --> bending angle --> refractivity"
echo "-----------------------------------------------------------------------"

# 2.1 Run tool
OUT=ropp_pp_test_2m.nc
../tools/ropp_pp_occ_tool ../data/ropp_pp_test.nc -o $OUT -c ../config/cosmic_pp.cf -m GMSIS

# 2.2 Check output file is produced
echo
echo "Finished pp"
echo
if [[ -f $OUT ]]; then
  echo "Output file $OUT was created - test PASSED"
else
  echo "Output file $OUT was not created - test FAILED"
  exit
fi

# 2.3 Run idl to test the result
echo
if which idl >/dev/null 2>&1; then
  echo "*** Found IDL to check test results"
  echo "*** Running IDL"
  echo "***"

# The idl bit
#export DISPLAY=""
  idl 2>&1 << EOF
  it_pp_01, "$OUT"
EOF
#
  echo "Finished IDL"

# 2.4 Show results (find appropriate device)
  if [[ -f "ropp_pp_comp.jpg" ]]; then
    cp ropp_pp_comp.jpg ropp_pp_comp2.jpg
    echo "plot (ropp_pp_comp2.jpg) was produced"
    echo "Compare with reference ropp_pp_comp2-example.jpg for"
    echo "validation of ROPP build"
    echo

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
      echo "Could not find output software to output plot"
      echo "  - check t_ropp_pp.sh script for options. "
    else
      echo "Showing output using $device"
      $device "ropp_pp_comp2.jpg" &
      echo "This figure should be compared to its reference version:"
      $device "ropp_pp_comp2-example.jpg" &
      if [[ $ROPP_PAUSE != '' ]] ; then 
        echo "Hit any key to continue"
        read hak
        pids=$(ps -C $device | tail -2 | awk '{print $1}')
        kill -KILL $pids >/dev/null 2>&1
      fi
    fi
  fi

else
  echo "***"
  echo "*** IDL not found in path. Cannot check test results in output"
  echo "*** See plot_fm.pro for hints on reading and plotting output data."
  echo "***"
fi

#-------------------------------------------------------------------------------
# 3. Test single profile file [GRAS PROFILE OCCULTATION]
#-------------------------------------------------------------------------------

echo
echo "Test 3: GRAS RS L1, L2 phase --> bending angle --> refractivity"
echo "-----------------------------------------------------------------------"

# 3.1 Run tools
OUT=ropp_pp_test_3m.nc
../tools/ropp_pp_grasrs2ropp ../data/ropp_pp_grasrs_test.nc -o ropp_pp_test_gras.nc
../tools/ropp_pp_occ_tool ropp_pp_test_gras.nc -o $OUT -c ../config/gras_pp.cf -m GMSIS

# 3.2 Check output file is produced

echo
echo "Finished pp"
echo
if [[ -f $OUT ]]; then
  echo "Output file $OUT was created - test PASSED"
else
  echo "Output file $OUT was not created - test FAILED"
  exit
fi

#-------------------------------------------------------------------------------
# 4. Test single profile file [SPECTRA]
#-------------------------------------------------------------------------------

echo
echo "Test 4: Single file L1, L2 phase --> spectra"
echo "-----------------------------------------------------------------------"

# 4.1 Run tool
OUT=ROanalysis_dt_L1.dat
../tools/ropp_pp_spectra_tool ../data/ropp_pp_test.nc

# 4.2 Check output file is produced
echo
echo "Finished pp"
echo
if [[ -f $OUT ]]; then
  echo "Output file $OUT was created - test PASSED"
else
  echo "Output file $OUT was not created - test FAILED"
  exit
fi

# 4.3 Run idl to test the results
echo
if which idl >/dev/null 2>&1; then
  echo "*** Found IDL to check test results"
  echo "*** Running IDL"
  echo "***"

# What format of output file should we produce?

  device="xv"
  op_format="jpg"
  if which display >/dev/null ; then
    device="display"
    op_format="eps"
  elif which gv >/dev/null ; then
    device="gv"
    op_format="eps"
  elif which ghostview >/dev/null ; then
    device="ghostview"
    op_format="eps"
  elif which xv >/dev/null ; then
    device="xv"
    op_format="jpg"
  fi

  if [[ $device = " " ]] ; then

    echo "Could not find software to display plot"
    echo "  - check t_ropp_pp.sh script for options. "

  else 
  
# First plot
    echo "Started IDL"
#export DISPLAY=""
  idl 2>&1 << EOF
  it_pp_spectra_dt, op_format='${op_format}'
EOF
    echo "Finished IDL"

    if [[ -f "ropp_pp_spectra_dt.${op_format}" ]]; then
      echo "plot (ropp_pp_spectra_dt.${op_format}) was produced"
      echo "Showing output using $device"
      $device "ropp_pp_spectra_dt.${op_format}" &
      echo "This figure should be compared to its reference version:"
      $device "ropp_pp_spectra_dt-example.${op_format}" &
      if [[ $ROPP_PAUSE != '' ]] ; then 
        echo "Hit any key to continue"
        read hak
        pids=$(ps -C $device | tail -2 | awk '{print $1}')
        kill -KILL $pids >/dev/null 2>&1
      fi

    fi

# Second plot
    echo "Started IDL"
#export DISPLAY=""
  idl 2>&1 << EOF
  it_pp_spectra_ep, op_format='${op_format}'
EOF
    echo "Finished IDL"

    if [[ -f "ropp_pp_spectra_ep.${op_format}" ]]; then
      echo "plot (ropp_pp_spectra_ep.${op_format}) was produced"
      echo "Showing output using $device"
      $device "ropp_pp_spectra_ep.${op_format}" &
      echo "This figure should be compared to its reference version:"
      $device "ropp_pp_spectra_ep-example.${op_format}" &
      if [[ $ROPP_PAUSE != '' ]] ; then 
        echo "Hit any key to continue"
        read hak
        pids=$(ps -C $device | tail -2 | awk '{print $1}')
        kill -KILL $pids >/dev/null 2>&1
      fi

    fi

  fi


else

  echo "***"
  echo "*** IDL not found in path. Cannot check test results in output"
  echo "*** See plot_fm.pro for hints on reading and plotting output data."
  echo "***"

fi

#-------------------------------------------------------------------------------
# 5. Test TPH tool on single file
#-------------------------------------------------------------------------------

echo
echo "Test 5: TPH generation"
echo "----------------------"

# 5.1 Run tool
IFILE=../data/ropp_pp_tph_test.nc
OFILE=ropp_pp_tph_test.nc

../tools/ropp_pp_tph_tool -d $IFILE -o $OFILE


# 5.2 Check output file is produced
echo
echo "Finished ropp_pp_tph_tool"
echo
if [[ -f $OFILE ]]; then
  echo "Output file $OFILE was created - test PASSED"
else
  echo "Output file $OFILE was not created - test FAILED"
  exit
fi


# 5.3 Run ncdump/bc to compare the results
ropp_MDFV=-9.9999e+07

ierr=0

for tph_var in `echo tph_bangle tph_refrac tph_tdry_lrt tph_tdry_cpt prh_tdry_cpt tph_temp_lrt tph_temp_cpt prh_temp_cpt \
                     tpa_bangle tpn_refrac tpt_tdry_lrt tpt_tdry_cpt prt_tdry_cpt tpt_temp_lrt tpt_temp_cpt prt_temp_cpt` ; do

  tph_cntl=`ncdump -v $tph_var $IFILE |grep $tph_var' = ' |cut -d"=" -f2 |cut -d";" -f1`

  tph_test=`ncdump -v $tph_var $OFILE |grep $tph_var' = ' |cut -d"=" -f2 |cut -d";" -f1`

  if [ $tph_cntl != $ropp_MDFV  -a  $tph_cntl != $ropp_MDFV ] ; then

    tph_diff=`echo "scale=10; $tph_test - $tph_cntl; " | bc`

    tph_diff_flag=`echo "scale=10; $tph_diff * $tph_diff > 0.000001; " | bc`

    if [[ $tph_diff_flag = '1' ]] ; then
      ierr=`expr $ierr \+ 1`
      echo "Difference in $tph_var between reference and test of more than 1.0e-3m - test FAILED"
    fi

  fi

done

if [[ $ierr -eq 0 ]]; then
  echo "No significant differences in TPH - test PASSED"
fi


#-------------------------------------------------------------------------------
# 6. Exit
#-------------------------------------------------------------------------------

exit

