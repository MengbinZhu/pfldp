#! /bin/sh
#
# Copyright by The HDF Group.
# Copyright by the Board of Trustees of the University of Illinois.
# All rights reserved.
#
# This file is part of HDF5.  The full HDF5 copyright notice, including
# terms governing use, modification, and redistribution, is contained in
# the files COPYING and Copyright.html.  COPYING can be found at the root
# of the source code distribution tree; Copyright.html can be found at the
# root level of an installed copy of the electronic HDF5 document set and
# is linked from the top-level documents page.  It can also be found at
# http://hdfgroup.org/HDF5/doc/Copyright.html.  If you do not have
# access to either file, you may request a copy from help@hdfgroup.org.
#
# Tests for the h5repack tool
#
# Modification:
#  Pedro Vicente Nunes, 11/15/2006
#  Added $FILEN variables for file names
#   

USE_FILTER_SZIP="no"
USE_FILTER_DEFLATE="yes"
USE_FILTER_SHUFFLE="yes"
USE_FILTER_FLETCHER32="yes"
USE_FILTER_NBIT="yes"
USE_FILTER_SCALEOFFSET="yes"

TESTNAME=h5repack
EXIT_SUCCESS=0
EXIT_FAILURE=1

H5REPACK=h5repack               # The tool name
H5REPACK_BIN=`pwd`/$H5REPACK    # The path of the tool binary

H5DIFF=../h5diff/h5diff         # The h5diff tool name 
H5DIFF_BIN=`pwd`/$H5DIFF        # The path of the h5diff tool binary

H5DUMP=../h5dump/h5dump         # The h5dump tool name
H5DUMP_BIN=`pwd`/$H5DUMP        # The path of the h5dump tool binary

GREP='grep'
CP='cp'

H5DETECTSZIP=testh5repack_detect_szip              
H5DETECTSZIP_BIN=`pwd`/$H5DETECTSZIP    


nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
#
if test -z "$srcdir"; then
   srcdir=.
fi

# source dirs
SRC_TOOLS="$srcdir/.."
SRC_TOOLS_TESTFILES="$SRC_TOOLS/testfiles"
# testfiles source dirs for tools
SRC_H5LS_TESTFILES="$SRC_TOOLS_TESTFILES"
SRC_H5DUMP_TESTFILES="$SRC_TOOLS_TESTFILES"
SRC_H5DIFF_TESTFILES="$SRC_TOOLS/h5diff/testfiles"
SRC_H5COPY_TESTFILES="$SRC_TOOLS/h5copy/testfiles"
SRC_H5REPACK_TESTFILES="$SRC_TOOLS/h5repack/testfiles"
SRC_H5JAM_TESTFILES="$SRC_TOOLS/h5jam/testfiles"
SRC_H5STAT_TESTFILES="$SRC_TOOLS/h5stat/testfiles"
SRC_H5IMPORT_TESTFILES="$SRC_TOOLS/h5import/testfiles"

TESTDIR=./testfiles
test -d $TESTDIR || mkdir $TESTDIR

######################################################################
# test files
# --------------------------------------------------------------------
# All the test files copy from source directory to test directory
# NOTE: Keep this framework to add/remove test files.
#       Any test files from other tools can be used in this framework.
#       This list are also used for checking exist.
#       Comment '#' without space can be used.
# --------------------------------------------------------------------
LIST_HDF5_TEST_FILES="
$SRC_H5REPACK_TESTFILES/h5repack_attr.h5
$SRC_H5REPACK_TESTFILES/h5repack_attr_refs.h5
$SRC_H5REPACK_TESTFILES/h5repack_deflate.h5
$SRC_H5REPACK_TESTFILES/h5repack_early.h5
$SRC_H5REPACK_TESTFILES/h5repack_ext.h5
$SRC_H5REPACK_TESTFILES/h5repack_fill.h5
$SRC_H5REPACK_TESTFILES/h5repack_filters.h5
$SRC_H5REPACK_TESTFILES/h5repack_fletcher.h5
$SRC_H5REPACK_TESTFILES/h5repack_hlink.h5
$SRC_H5REPACK_TESTFILES/h5repack_layout.h5
$SRC_H5REPACK_TESTFILES/h5repack_layouto.h5
$SRC_H5REPACK_TESTFILES/h5repack_layout2.h5
$SRC_H5REPACK_TESTFILES/h5repack_named_dtypes.h5
$SRC_H5REPACK_TESTFILES/h5repack_nbit.h5
$SRC_H5REPACK_TESTFILES/h5repack_objs.h5
$SRC_H5REPACK_TESTFILES/h5repack_refs.h5
$SRC_H5REPACK_TESTFILES/h5repack_shuffle.h5
$SRC_H5REPACK_TESTFILES/h5repack_soffset.h5
$SRC_H5REPACK_TESTFILES/h5repack_szip.h5
$SRC_TOOLS_TESTFILES/tfamily00000.h5
$SRC_TOOLS_TESTFILES/tfamily00001.h5
$SRC_TOOLS_TESTFILES/tfamily00002.h5
$SRC_TOOLS_TESTFILES/tfamily00003.h5
$SRC_TOOLS_TESTFILES/tfamily00004.h5
$SRC_TOOLS_TESTFILES/tfamily00005.h5
$SRC_TOOLS_TESTFILES/tfamily00006.h5
$SRC_TOOLS_TESTFILES/tfamily00007.h5
$SRC_TOOLS_TESTFILES/tfamily00008.h5
$SRC_TOOLS_TESTFILES/tfamily00009.h5
$SRC_TOOLS_TESTFILES/tfamily00010.h5
"

LIST_OTHER_TEST_FILES="
$SRC_H5REPACK_TESTFILES/h5repack_ext.bin
$SRC_H5REPACK_TESTFILES/ublock.bin
$SRC_H5REPACK_TESTFILES/h5repack.info
$SRC_TOOLS_TESTFILES/h5repack_filters.h5.ddl
"

#
# copy test files and expected output files from source dirs to test dir
#
COPY_TESTFILES="$LIST_HDF5_TEST_FILES $LIST_OTHER_TEST_FILES"

COPY_TESTFILES_TO_TESTDIR()
{
    # copy test files. Used -f to make sure get a new copy
    for tstfile in $COPY_TESTFILES
    do
        # ignore '#' comment
        echo $tstfile | tr -d ' ' | grep '^#' > /dev/null
        RET=$?
        if [ $RET -eq 1 ]; then
            if [ -a $tstfile ]; then
                $CP -f $tstfile $TESTDIR
            else
                echo "Error: FAILED to copy $tstfile"
                echo "       $tstfile doesn't exist!"
                exit $EXIT_FAILURE
            fi
        fi
    done
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Testing".
#
TESTING() {
   SPACES="                                                               "
   echo "Testing $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Verifying".
#
VERIFY() {
 SPACES="                                                               "
 echo "Verifying h5diff output $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Print a message that a test has been skipped (because a required filter
# was unavailable)
SKIP() {
    TESTING $H5REPACK $@
    echo " -SKIP-"
}

# Call the h5diff tool
#
DIFFTEST() 
{
    VERIFY  $@
    $RUNSERIAL $H5DIFF_BIN -q  "$@" 
    RET=$?
    if [ $RET != 0 ] ; then
         echo "*FAILED*"
         nerrors="`expr $nerrors + 1`"
    else
         echo " PASSED"
    fi
        
}
          
# Call h5repack
#


# call TOOLTEST_MAIN and delete $output file
TOOLTEST() 
{
    echo $@
    TOOLTEST_MAIN $@
    outfile=$TESTDIR/out.$1
    rm -f $outfile
}

# TOOLTEST main function, doesn't delete $output file
TOOLTEST_MAIN() 
{
    # Run test.
    TESTING $H5REPACK $@

    infile=$TESTDIR/$1
    outfile=$TESTDIR/out.$1
    shift
    $RUNSERIAL $H5REPACK_BIN "$@" $infile $outfile
    RET=$?
    if [ $RET != 0 ] ; then
	echo "*FAILED*"
	nerrors="`expr $nerrors + 1`"
    else
	echo " PASSED"
	DIFFTEST $infile $outfile
    fi
}

#------------------------------------------
# Verifying layouts of a dataset 
VERIFY_LAYOUT_DSET()
{
    outfile=$TESTDIR/out.$1
    layoutfile=$TESTDIR/layout.$1
    dset=$2
    expectlayout=$3
    
    #---------------------------------
    # check the layout from a dataset
    VERIFY  "Layout"
    $RUNSERIAL $H5DUMP_BIN -d $dset -pH $outfile > $layoutfile
    $GREP $expectlayout $layoutfile > /dev/null
    if [ $? -eq 0 ]; then
        echo " PASSED"
    else
        echo " FAILED"
    fi

    # clean up tmp files
    rm -f $outfile
    rm -f $layoutfile
}

#----------------------------------------
# Verifying layouts from entire file
VERIFY_LAYOUT_ALL()
{
    outfile=$TESTDIR/out.$1
    layoutfile=$TESTDIR/layout.$1
    expectlayout=$2
    
    #---------------------------------
    # check the layout from a dataset
    # check if the other layouts still exsit
    VERIFY  "Layout "
        # if CONTIGUOUS
        if [ $expectlayout = "CONTIGUOUS" ]; then
            $RUNSERIAL $H5DUMP_BIN -pH $outfile > $layoutfile
            $GREP "COMPACT" $layoutfile  > /dev/null
            if [ $? -eq 0 ]; then
                echo " FAILED"
            else
                $GREP "CHUNKED" $layoutfile  > /dev/null
                if [ $? -eq 0 ]; then
                    echo " FAILED"
                else
                    echo " PASSED"
                fi
            fi
        else    
            # if COMPACT
            if [ $expectlayout = "COMPACT" ]; then
                $RUNSERIAL $H5DUMP_BIN -pH $outfile > $layoutfile
                $GREP "CHUNKED" $layoutfile  > /dev/null
                if [ $? -eq 0 ]; then
                    echo " FAILED"
                else
                    $GREP "CONTIGUOUS" $layoutfile  > /dev/null
                    if [ $? -eq 0 ]; then
                        echo " FAILED"
                    else
                        echo " PASSED"
                    fi
                fi
            else
                # if CHUNKED
                if [ $expectlayout = "CHUNKED" ]; then
                    $RUNSERIAL $H5DUMP_BIN -pH $outfile > $layoutfile
                    $GREP "CONTIGUOUS" $layoutfile  > /dev/null
                    if [ $? -eq 0 ]; then
                        echo " FAILED"
                    else
                        $GREP "COMPACT" $layoutfile  > /dev/null
                        if [ $? -eq 0 ]; then
                            echo " FAILED"
                        else
                            echo " PASSED"
                        fi
                    fi
                fi
           fi
        fi

    # clean up tmp files
    rm -f $outfile
    rm -f $layoutfile
}

# same as TOOLTEST, but it uses the old syntax -i input_file -o output_file
#
TOOLTEST0() 
{
    # Run test.
    TESTING $H5REPACK $@

    infile=$TESTDIR/$1
    outfile=$TESTDIR/out.$1
    shift
    $RUNSERIAL $H5REPACK_BIN -i $infile -o $outfile "$@"
    RET=$?
    if [ $RET != 0 ] ; then
        echo "*FAILED*"
        nerrors="`expr $nerrors + 1`"
    else
        echo " PASSED"
        DIFFTEST $infile $outfile
    fi
    rm -f $outfile
}


# same as TOOLTEST, but it uses without -i -o options
# used to test the family driver, where these files reside
#
TOOLTEST1() 
{
    # Run test.
    TESTING $H5REPACK $@

    infile=$TESTDIR/$1
    outfile=$TESTDIR/out.$1
    shift
    $RUNSERIAL $H5REPACK_BIN "$@" $infile $outfile
    RET=$?
    if [ $RET != 0 ] ; then
	echo "*FAILED*"
	nerrors="`expr $nerrors + 1`"
    else
	echo " PASSED"
	DIFFTEST $infile $outfile
    fi
    rm -f $outfile
}
          
# Call h5repack and compare output to a text file for -v option
#
TOOLTESTV() 
{
    # Run test.
    TESTING $H5REPACK $@
    expect="$TESTDIR/$1.ddl"
    actual="$TESTDIR/`basename $1 .ddl`.out"
    actual_err="$TESTDIR/`basename $1 .ddl`.err"

    infile=$TESTDIR/$1
    outfile=$TESTDIR/out.$1
    shift
    $RUNSERIAL $H5REPACK_BIN "$@" $infile $outfile >$actual 2>$actual_err
    cp $actual $actual_sav
    STDOUT_FILTER $actual
    cat $actual_err >> $actual

    if cmp -s $expect $actual; then
     echo " PASSED"
    else
     echo "*FAILED*"
     echo "    Expected result (*.ddl) differs from actual result (*.out)"
     nerrors="`expr $nerrors + 1`"
     test yes = "$verbose" && diff -c $expect $actual |sed 's/^/    /'
    fi
    
   rm -f $actual $actual_err $actual_sav
}

# This is different from $srcdir/../../bin/output_filter.sh
STDOUT_FILTER() {
    result_file=$1
    tmp_file=/tmp/h5test_tmp_$$
    # Filter name of files.
    cp $result_file $tmp_file
    sed -e '/^Opening file/d' -e '/^Making file/d' \
    < $tmp_file > $result_file
    # cleanup
    rm -f $tmp_file
}

#
# The tests
# We use the files generated by h5repacktst
# Each run generates "<file>.out.h5" and the tool h5diff is used to
# compare the input and output files
#
# the tests are the same as the program h5repacktst, but run from the CLI 
#

# See which filters are usable (and skip tests for filters we
# don't have).  Do this by searching H5pubconf.h to see which
# filters are defined.

# detect whether the encoder is present. 
USE_FILTER_SZIP_ENCODER="no";
if test $USE_FILTER_SZIP = "yes"; then
USE_FILTER_SZIP_ENCODER=`$RUNSERIAL $H5DETECTSZIP_BIN`
fi

##############################################################################
###			  T H E   T E S T S                               
##############################################################################
# prepare for test
COPY_TESTFILES_TO_TESTDIR

# copy files (these files have no filters) 
TOOLTEST h5repack_fill.h5
TOOLTEST h5repack_objs.h5
TOOLTEST h5repack_attr.h5
TOOLTEST h5repack_hlink.h5
TOOLTEST h5repack_layout.h5
TOOLTEST h5repack_early.h5


# use h5repack_layout.h5 to write some filters  (this file has  no filters)

# gzip with individual object
arg="h5repack_layout.h5 -f dset1:GZIP=1  -l dset1:CHUNK=20x10"
if test  $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi
  
# gzip for all 
arg="h5repack_layout.h5 -f GZIP=1"
if test  $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# szip with individual object
arg="h5repack_layout.h5 -f dset2:SZIP=8,EC  -l dset2:CHUNK=20x10"
if test $USE_FILTER_SZIP_ENCODER != "yes" -o $USE_FILTER_SZIP != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg 
fi

# szip for all
arg="h5repack_layout.h5 -f SZIP=8,NN"
if test $USE_FILTER_SZIP_ENCODER != "yes" -o $USE_FILTER_SZIP != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg 
fi

# shuffle with individual object
arg="h5repack_layout.h5 -f dset2:SHUF  -l dset2:CHUNK=20x10"
if test $USE_FILTER_SHUFFLE != "yes"  ; then
 SKIP $arg
else
 TOOLTEST $arg 
fi
  

# shuffle for all
arg="h5repack_layout.h5 -f SHUF"
if test $USE_FILTER_SHUFFLE != "yes"  ; then
 SKIP $arg
else
 TOOLTEST $arg
fi
  
# fletcher32  with individual object
arg="h5repack_layout.h5 -f dset2:FLET  -l dset2:CHUNK=20x10"
if test $USE_FILTER_FLETCHER32 != "yes"  ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# fletcher32 for all
arg="h5repack_layout.h5 -f FLET"
if test $USE_FILTER_FLETCHER32 != "yes"  ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# all filters
arg="h5repack_layout.h5 -f dset2:SHUF -f dset2:FLET -f dset2:SZIP=8,NN -f dset2:GZIP=1 -l dset2:CHUNK=20x10"
if test $USE_FILTER_SZIP_ENCODER != "yes" -o $USE_FILTER_SZIP != "yes" -o $USE_FILTER_SHUFFLE != "yes" -o $USE_FILTER_FLETCHER32 != "yes" -o $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# verbose gzip with individual object
arg="h5repack_filters.h5 -v -f /dset_deflate:GZIP=9"
if test  $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi
  
###########################################################
# the following tests assume the input files have filters
###########################################################

# szip copy
arg="h5repack_szip.h5"
if test $USE_FILTER_SZIP_ENCODER != "yes" -o $USE_FILTER_SZIP != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi
  
# szip remove
arg="h5repack_szip.h5 --filter=dset_szip:NONE"
if test $USE_FILTER_SZIP_ENCODER != "yes" -o $USE_FILTER_SZIP != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi
  
# deflate copy
arg="h5repack_deflate.h5"
if test $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# deflate remove
arg="h5repack_deflate.h5 -f dset_deflate:NONE"
if test $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi
    
# shuffle copy
arg="h5repack_shuffle.h5"
if test $USE_FILTER_SHUFFLE != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# shuffle remove
arg="h5repack_shuffle.h5 -f dset_shuffle:NONE"
if test $USE_FILTER_SHUFFLE != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# fletcher32 copy
arg="h5repack_fletcher.h5"
if test $USE_FILTER_FLETCHER32 != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# fletcher32 remove
arg="h5repack_fletcher.h5 -f dset_fletcher32:NONE"
if test $USE_FILTER_FLETCHER32 != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi        

# nbit copy
arg="h5repack_nbit.h5"
if test $USE_FILTER_NBIT != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# nbit remove
arg="h5repack_nbit.h5 -f dset_nbit:NONE"
if test $USE_FILTER_NBIT != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi        

# nbit add
arg="h5repack_nbit.h5 -f dset_int31:NBIT"
if test $USE_FILTER_NBIT != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# scaleoffset copy
arg="h5repack_soffset.h5"
if test $USE_FILTER_SCALEOFFSET != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# scaleoffset add
arg="h5repack_soffset.h5 -f dset_none:SOFF=31,IN"
if test $USE_FILTER_SCALEOFFSET != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# scaleoffset remove
arg="h5repack_soffset.h5 -f dset_scaleoffset:NONE"
if test $USE_FILTER_SCALEOFFSET != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi        

# remove all  filters
arg="h5repack_filters.h5 -f NONE"
if test $USE_FILTER_FLETCHER32 != "yes" -o $USE_FILTER_DEFLATE != "yes" -o $USE_FILTER_SZIP != "yes" -o $USE_FILTER_SZIP_ENCODER != "yes" -o $USE_FILTER_SHUFFLE != "yes"  -o $USE_FILTER_NBIT != "yes"  -o $USE_FILTER_SCALEOFFSET != "yes"  ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

#filter conversions

arg="h5repack_deflate.h5 -f dset_deflate:SZIP=8,NN"
if test $USE_FILTER_SZIP_ENCODER != "yes" -o $USE_FILTER_SZIP != "yes" -o $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg 
fi

arg="h5repack_szip.h5 -f dset_szip:GZIP=1"
if test $USE_FILTER_SZIP != "yes" -o $USE_FILTER_SZIP_ENCODER != "yes"  -o $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg 
fi


#limit
arg="h5repack_layout.h5 -f GZIP=1 -m 1024"
if test $USE_FILTER_DEFLATE != "yes"  ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

#file
arg="h5repack_layout.h5 -e $TESTDIR/h5repack.info"
if test $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg 
else
 TOOLTEST $arg 
fi

#########################################################
# layout options (these files have no filters)
#########################################################
TOOLTEST_MAIN h5repack_layout.h5 --layout dset2:CHUNK=20x10
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset2 CHUNKED

TOOLTEST_MAIN h5repack_layout.h5 -l CHUNK=20x10
VERIFY_LAYOUT_ALL h5repack_layout.h5 CHUNKED

TOOLTEST_MAIN h5repack_layout.h5 -l dset2:CONTI
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset2 CONTIGUOUS

TOOLTEST_MAIN h5repack_layout.h5 -l CONTI
VERIFY_LAYOUT_ALL h5repack_layout.h5 CONTIGUOUS

TOOLTEST_MAIN h5repack_layout.h5 -l dset2:COMPA
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset2 COMPACT

TOOLTEST_MAIN h5repack_layout.h5 -l COMPA
VERIFY_LAYOUT_ALL h5repack_layout.h5 COMPACT

################################################################
# layout conversions (file has no filters)
###############################################################

TOOLTEST_MAIN h5repack_layout.h5 -l dset_compact:CONTI
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset_compact CONTIGUOUS

TOOLTEST_MAIN h5repack_layout.h5 -l dset_compact:CHUNK=2x5
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset_compact CHUNKED

TOOLTEST_MAIN h5repack_layout.h5 -l dset_compact:COMPA
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset_compact COMPACT

TOOLTEST_MAIN h5repack_layout.h5 -l dset_contiguous:COMPA
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset_contiguous COMPACT

TOOLTEST_MAIN h5repack_layout.h5 -l dset_contiguous:CHUNK=3x6
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset_contiguous CHUNKED

TOOLTEST_MAIN h5repack_layout.h5 -l dset_contiguous:CONTI
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset_contiguous CONTIGUOUS

TOOLTEST_MAIN h5repack_layout.h5 -l dset_chunk:COMPA
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset_chunk COMPACT

TOOLTEST_MAIN h5repack_layout.h5 -l dset_chunk:CONTI
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset_chunk CONTIGUOUS

TOOLTEST_MAIN h5repack_layout.h5 -l dset_chunk:CHUNK=18x13
VERIFY_LAYOUT_DSET h5repack_layout.h5 dset_chunk CHUNKED

# test convert small size dataset ( < 1k) to compact layout without -m
TOOLTEST_MAIN h5repack_layout2.h5 -l contig_small:COMPA
VERIFY_LAYOUT_DSET h5repack_layout2.h5 contig_small COMPACT

TOOLTEST_MAIN h5repack_layout2.h5 -l chunked_small_fixed:COMPA
VERIFY_LAYOUT_DSET h5repack_layout2.h5 chunked_small_fixed COMPACT

# Native option
# Do not use FILE1, as the named dtype will be converted to native, and h5diff will
# report a difference.
TOOLTEST h5repack_fill.h5 -n
TOOLTEST h5repack_attr.h5 -n


# latest file format with long switches. use FILE4=h5repack_layout.h5 (no filters)
arg="h5repack_layout.h5 --layout CHUNK=20x10 --filter GZIP=1 --minimum=10 --native --latest --compact=8 --indexed=6 --ssize=8[:dtype]"
if test $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg
else
 TOOLTEST_MAIN $arg
 VERIFY_LAYOUT_ALL h5repack_layout.h5 CHUNKED
fi

# latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
arg="h5repack_layout.h5 -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype]"
if test $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg
else
 TOOLTEST_MAIN $arg
 VERIFY_LAYOUT_ALL h5repack_layout.h5 CHUNKED
fi

# several global filters

arg="h5repack_layout.h5 --filter GZIP=1 --filter SHUF"
if test $USE_FILTER_DEFLATE != "yes" -o $USE_FILTER_SHUFFLE != "yes" ; then
 SKIP $arg
else
 TOOLTEST $arg
fi

# syntax of -i infile -o outfile
# latest file format with short switches. use FILE4=h5repack_layout.h5 (no filters)
arg="h5repack_layout.h5 -l CHUNK=20x10 -f GZIP=1 -m 10 -n -L -c 8 -d 6 -s 8[:dtype]"
if test $USE_FILTER_DEFLATE != "yes" ; then
 SKIP $arg
else
 TOOLTEST0 $arg
fi 

# add a userblock to file
arg="h5repack_objs.h5 -u ublock.bin -b 2048"
TOOLTEST $arg

# add alignment
arg="h5repack_objs.h5 -t 1 -a 1 "
TOOLTEST $arg

# Check repacking file with old version of layout message (should get upgraded
#       to new version and be readable, etc.)
TOOLTEST h5repack_layouto.h5

# test for datum size > H5TOOLS_MALLOCSIZE
TOOLTEST h5repack_objs.h5 -f GZIP=1

# Check repacking file with committed datatypes in odd configurations
TOOLTEST h5repack_named_dtypes.h5

# tests family driver (file is located in common testfiles folder, uses TOOLTEST1
TOOLTEST1 tfamily%05d.h5

# test various references (bug 1814 and 1726)
TOOLTEST h5repack_refs.h5

# test attribute with various references (bug1797 / HDFFV-5932)
# the references in attribute of compund or vlen datatype 
TOOLTEST h5repack_attr_refs.h5 

if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi

