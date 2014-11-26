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
# Tests for the h5dump tool

# Determine which filters are available
USE_FILTER_SZIP="no"
USE_FILTER_DEFLATE="yes"
USE_FILTER_SHUFFLE="yes"
USE_FILTER_FLETCHER32="yes"
USE_FILTER_NBIT="yes"
USE_FILTER_SCALEOFFSET="yes"

TESTNAME=h5dump
EXIT_SUCCESS=0
EXIT_FAILURE=1

DUMPER=h5dump                     # The tool name
DUMPER_BIN=`pwd`/$DUMPER          # The path of the tool binary

H5DIFF=../h5diff/h5diff           # The h5diff tool name 
H5DIFF_BIN=`pwd`/$H5DIFF          # The path of the h5diff  tool binary

H5IMPORT=../h5import/h5import     # The h5import tool name 
H5IMPORT_BIN=`pwd`/$H5IMPORT      # The path of the h5import  tool binary


CMP='cmp -s'
DIFF='diff -c'
CP='cp'

nerrors=0
verbose=yes

# The build (current) directory might be different than the source directory.
if test -z "$srcdir"; then
   srcdir=.
fi
# source dirs
SRC_TOOLS="$srcdir/../"
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
$SRC_H5DUMP_TESTFILES/filter_fail.h5
$SRC_H5DUMP_TESTFILES/packedbits.h5
$SRC_H5DUMP_TESTFILES/taindices.h5
$SRC_H5DUMP_TESTFILES/tall.h5
$SRC_H5DUMP_TESTFILES/tarray1.h5
$SRC_H5DUMP_TESTFILES/tarray1_big.h5
$SRC_H5DUMP_TESTFILES/tarray2.h5
$SRC_H5DUMP_TESTFILES/tarray3.h5
$SRC_H5DUMP_TESTFILES/tarray4.h5
$SRC_H5DUMP_TESTFILES/tarray5.h5
$SRC_H5DUMP_TESTFILES/tarray6.h5
$SRC_H5DUMP_TESTFILES/tarray7.h5
$SRC_H5DUMP_TESTFILES/tarray8.h5
$SRC_H5DUMP_TESTFILES/tattr.h5
$SRC_H5DUMP_TESTFILES/tattr2.h5
$SRC_H5DUMP_TESTFILES/tattrreg.h5
$SRC_H5DUMP_TESTFILES/tbigdims.h5
$SRC_H5DUMP_TESTFILES/tbinary.h5
$SRC_H5DUMP_TESTFILES/tchar.h5
$SRC_H5DUMP_TESTFILES/tcompound.h5
$SRC_H5DUMP_TESTFILES/tcompound_complex.h5
$SRC_H5DUMP_TESTFILES/tdatareg.h5
$SRC_H5DUMP_TESTFILES/tdset.h5
$SRC_H5DUMP_TESTFILES/tempty.h5
$SRC_H5DUMP_TESTFILES/tsoftlinks.h5
$SRC_H5DUMP_TESTFILES/textlinkfar.h5
$SRC_H5DUMP_TESTFILES/textlinksrc.h5
$SRC_H5DUMP_TESTFILES/textlinktar.h5
$SRC_H5DUMP_TESTFILES/textlink.h5
$SRC_H5DUMP_TESTFILES/tfamily00000.h5
$SRC_H5DUMP_TESTFILES/tfamily00001.h5
$SRC_H5DUMP_TESTFILES/tfamily00002.h5
$SRC_H5DUMP_TESTFILES/tfamily00003.h5
$SRC_H5DUMP_TESTFILES/tfamily00004.h5
$SRC_H5DUMP_TESTFILES/tfamily00005.h5
$SRC_H5DUMP_TESTFILES/tfamily00006.h5
$SRC_H5DUMP_TESTFILES/tfamily00007.h5
$SRC_H5DUMP_TESTFILES/tfamily00008.h5
$SRC_H5DUMP_TESTFILES/tfamily00009.h5
$SRC_H5DUMP_TESTFILES/tfamily00010.h5
$SRC_H5DUMP_TESTFILES/tfcontents1.h5
$SRC_H5DUMP_TESTFILES/tfcontents2.h5
$SRC_H5DUMP_TESTFILES/tfilters.h5
$SRC_H5DUMP_TESTFILES/tfpformat.h5
$SRC_H5DUMP_TESTFILES/tfvalues.h5
$SRC_H5DUMP_TESTFILES/tgroup.h5
$SRC_H5DUMP_TESTFILES/tgrp_comments.h5
$SRC_H5DUMP_TESTFILES/thlink.h5
$SRC_H5DUMP_TESTFILES/thyperslab.h5
$SRC_H5DUMP_TESTFILES/tlarge_objname.h5
#$SRC_H5DUMP_TESTFILES/tldouble.h5
$SRC_H5DUMP_TESTFILES/tlonglinks.h5
$SRC_H5DUMP_TESTFILES/tloop.h5
$SRC_H5DUMP_TESTFILES/tmulti-b.h5
$SRC_H5DUMP_TESTFILES/tmulti-g.h5
$SRC_H5DUMP_TESTFILES/tmulti-l.h5
$SRC_H5DUMP_TESTFILES/tmulti-o.h5
$SRC_H5DUMP_TESTFILES/tmulti-r.h5
$SRC_H5DUMP_TESTFILES/tmulti-s.h5
$SRC_H5DUMP_TESTFILES/tnamed_dtype_attr.h5
$SRC_H5DUMP_TESTFILES/tnestedcomp.h5
$SRC_H5DUMP_TESTFILES/tnullspace.h5
$SRC_H5DUMP_TESTFILES/zerodim.h5
$SRC_H5DUMP_TESTFILES/torderattr.h5
$SRC_H5DUMP_TESTFILES/tordergr.h5
$SRC_H5DUMP_TESTFILES/tsaf.h5
$SRC_H5DUMP_TESTFILES/tslink.h5
$SRC_H5DUMP_TESTFILES/tsplit_file-m.h5
$SRC_H5DUMP_TESTFILES/tsplit_file-r.h5
$SRC_H5DUMP_TESTFILES/tstr.h5
$SRC_H5DUMP_TESTFILES/tstr2.h5
$SRC_H5DUMP_TESTFILES/tstr3.h5
$SRC_H5DUMP_TESTFILES/tudlink.h5
$SRC_H5DUMP_TESTFILES/tvldtypes1.h5
$SRC_H5DUMP_TESTFILES/tvldtypes2.h5
$SRC_H5DUMP_TESTFILES/tvldtypes3.h5
$SRC_H5DUMP_TESTFILES/tvldtypes4.h5
$SRC_H5DUMP_TESTFILES/tvldtypes5.h5
$SRC_H5DUMP_TESTFILES/tvlstr.h5
$SRC_H5DUMP_TESTFILES/tvms.h5
"

LIST_OTHER_TEST_FILES="
$SRC_H5DUMP_TESTFILES/filter_fail.ddl
$SRC_H5DUMP_TESTFILES/packedbits.ddl
$SRC_H5DUMP_TESTFILES/tall-1.ddl
$SRC_H5DUMP_TESTFILES/tall-2.ddl
$SRC_H5DUMP_TESTFILES/tall-2A.ddl
$SRC_H5DUMP_TESTFILES/tall-2B.ddl
$SRC_H5DUMP_TESTFILES/tall-3.ddl
$SRC_H5DUMP_TESTFILES/tall-4s.ddl
$SRC_H5DUMP_TESTFILES/tall-5s.ddl
$SRC_H5DUMP_TESTFILES/tall-6.ddl
$SRC_H5DUMP_TESTFILES/tallfilters.ddl
$SRC_H5DUMP_TESTFILES/tarray1.ddl
$SRC_H5DUMP_TESTFILES/tarray1_big.ddl
$SRC_H5DUMP_TESTFILES/tarray2.ddl
$SRC_H5DUMP_TESTFILES/tarray3.ddl
$SRC_H5DUMP_TESTFILES/tarray4.ddl
$SRC_H5DUMP_TESTFILES/tarray5.ddl
$SRC_H5DUMP_TESTFILES/tarray6.ddl
$SRC_H5DUMP_TESTFILES/tarray7.ddl
$SRC_H5DUMP_TESTFILES/tarray8.ddl
$SRC_H5DUMP_TESTFILES/tattr-1.ddl
$SRC_H5DUMP_TESTFILES/tattr-2.ddl
$SRC_H5DUMP_TESTFILES/tattr-3.ddl
$SRC_H5DUMP_TESTFILES/tattrreg.ddl
$SRC_H5DUMP_TESTFILES/tattrregR.ddl
$SRC_H5DUMP_TESTFILES/tbin1.ddl
$SRC_H5DUMP_TESTFILES/tbin1.ddl
$SRC_H5DUMP_TESTFILES/tbin2.ddl
$SRC_H5DUMP_TESTFILES/tbin3.ddl
$SRC_H5DUMP_TESTFILES/tbin4.ddl
$SRC_H5DUMP_TESTFILES/tbinregR.ddl
$SRC_H5DUMP_TESTFILES/tbigdims.ddl
$SRC_H5DUMP_TESTFILES/tboot1.ddl
$SRC_H5DUMP_TESTFILES/tboot2.ddl
$SRC_H5DUMP_TESTFILES/tchar1.ddl
$SRC_H5DUMP_TESTFILES/tchunked.ddl
$SRC_H5DUMP_TESTFILES/tcomp-1.ddl
$SRC_H5DUMP_TESTFILES/tcomp-2.ddl
$SRC_H5DUMP_TESTFILES/tcomp-3.ddl
$SRC_H5DUMP_TESTFILES/tcomp-4.ddl
$SRC_H5DUMP_TESTFILES/tcompact.ddl
$SRC_H5DUMP_TESTFILES/tcontents.ddl
$SRC_H5DUMP_TESTFILES/tcontiguos.ddl
$SRC_H5DUMP_TESTFILES/tdatareg.ddl
$SRC_H5DUMP_TESTFILES/tdataregR.ddl
$SRC_H5DUMP_TESTFILES/tdeflate.ddl
$SRC_H5DUMP_TESTFILES/tdset-1.ddl
$SRC_H5DUMP_TESTFILES/tdset-2.ddl
$SRC_H5DUMP_TESTFILES/tdset-3s.ddl
$SRC_H5DUMP_TESTFILES/tempty.ddl
$SRC_H5DUMP_TESTFILES/texceedsubstart.ddl
$SRC_H5DUMP_TESTFILES/texceedsubcount.ddl
$SRC_H5DUMP_TESTFILES/texceedsubstride.ddl
$SRC_H5DUMP_TESTFILES/texceedsubblock.ddl
$SRC_H5DUMP_TESTFILES/texternal.ddl
$SRC_H5DUMP_TESTFILES/textlinksrc.ddl
$SRC_H5DUMP_TESTFILES/textlinkfar.ddl
$SRC_H5DUMP_TESTFILES/textlink.ddl
$SRC_H5DUMP_TESTFILES/tfamily.ddl
$SRC_H5DUMP_TESTFILES/tfill.ddl
$SRC_H5DUMP_TESTFILES/tfletcher32.ddl
$SRC_H5DUMP_TESTFILES/tfpformat.ddl
$SRC_H5DUMP_TESTFILES/tgroup-1.ddl
$SRC_H5DUMP_TESTFILES/tgroup-2.ddl
$SRC_H5DUMP_TESTFILES/tgrp_comments.ddl
$SRC_H5DUMP_TESTFILES/thlink-1.ddl
$SRC_H5DUMP_TESTFILES/thlink-2.ddl
$SRC_H5DUMP_TESTFILES/thlink-3.ddl
$SRC_H5DUMP_TESTFILES/thlink-4.ddl
$SRC_H5DUMP_TESTFILES/thlink-5.ddl
$SRC_H5DUMP_TESTFILES/thyperslab.ddl
$SRC_H5DUMP_TESTFILES/tindicesno.ddl
$SRC_H5DUMP_TESTFILES/tindicessub1.ddl
$SRC_H5DUMP_TESTFILES/tindicessub2.ddl
$SRC_H5DUMP_TESTFILES/tindicessub3.ddl
$SRC_H5DUMP_TESTFILES/tindicessub4.ddl
$SRC_H5DUMP_TESTFILES/tindicesyes.ddl
$SRC_H5DUMP_TESTFILES/tlarge_objname.ddl
#$SRC_H5DUMP_TESTFILES/tldouble.ddl 
$SRC_H5DUMP_TESTFILES/tlonglinks.ddl
$SRC_H5DUMP_TESTFILES/tloop-1.ddl
$SRC_H5DUMP_TESTFILES/tmulti.ddl
$SRC_H5DUMP_TESTFILES/tnamed_dtype_attr.ddl
$SRC_H5DUMP_TESTFILES/tnestcomp-1.ddl
$SRC_H5DUMP_TESTFILES/tnbit.ddl
$SRC_H5DUMP_TESTFILES/tnofilename.ddl
$SRC_H5DUMP_TESTFILES/tnullspace.ddl
$SRC_H5DUMP_TESTFILES/zerodim.ddl
$SRC_H5DUMP_TESTFILES/tordergr1.ddl
$SRC_H5DUMP_TESTFILES/tordergr2.ddl
$SRC_H5DUMP_TESTFILES/tordergr3.ddl
$SRC_H5DUMP_TESTFILES/tordergr4.ddl
$SRC_H5DUMP_TESTFILES/tordergr5.ddl
$SRC_H5DUMP_TESTFILES/torderattr1.ddl
$SRC_H5DUMP_TESTFILES/torderattr2.ddl
$SRC_H5DUMP_TESTFILES/torderattr3.ddl
$SRC_H5DUMP_TESTFILES/torderattr4.ddl
$SRC_H5DUMP_TESTFILES/tperror.ddl
$SRC_H5DUMP_TESTFILES/treference.ddl
$SRC_H5DUMP_TESTFILES/tsaf.ddl
$SRC_H5DUMP_TESTFILES/tscaleoffset.ddl
$SRC_H5DUMP_TESTFILES/tshuffle.ddl
$SRC_H5DUMP_TESTFILES/tslink-1.ddl
$SRC_H5DUMP_TESTFILES/tslink-2.ddl
$SRC_H5DUMP_TESTFILES/tsplit_file.ddl
$SRC_H5DUMP_TESTFILES/tstr-1.ddl
$SRC_H5DUMP_TESTFILES/tstr-2.ddl
$SRC_H5DUMP_TESTFILES/tstring.ddl
$SRC_H5DUMP_TESTFILES/tstring2.ddl
$SRC_H5DUMP_TESTFILES/tstringe.ddl
$SRC_H5DUMP_TESTFILES/tszip.ddl
$SRC_H5DUMP_TESTFILES/tudlink-1.ddl
$SRC_H5DUMP_TESTFILES/tudlink-2.ddl
$SRC_H5DUMP_TESTFILES/tuserfilter.ddl
$SRC_H5DUMP_TESTFILES/tvldtypes1.ddl
$SRC_H5DUMP_TESTFILES/tvldtypes2.ddl
$SRC_H5DUMP_TESTFILES/tvldtypes3.ddl
$SRC_H5DUMP_TESTFILES/tvldtypes4.ddl
$SRC_H5DUMP_TESTFILES/tvldtypes5.ddl
$SRC_H5DUMP_TESTFILES/tvlstr.ddl
$SRC_H5DUMP_TESTFILES/tvms.ddl
$SRC_H5DUMP_TESTFILES/h5dump-help.txt
$SRC_H5DUMP_TESTFILES/out3.h5import
$SRC_H5DUMP_TESTFILES/tpbitsArray.ddl
$SRC_H5DUMP_TESTFILES/tpbitsCompound.ddl
$SRC_H5DUMP_TESTFILES/tpbitsIncomplete.ddl
$SRC_H5DUMP_TESTFILES/tpbitsLengthExceeded.ddl
$SRC_H5DUMP_TESTFILES/tpbitsCharLengthExceeded.ddl
$SRC_H5DUMP_TESTFILES/tpbitsIntLengthExceeded.ddl
$SRC_H5DUMP_TESTFILES/tpbitsLongLengthExceeded.ddl
$SRC_H5DUMP_TESTFILES/tpbitsLengthPositive.ddl
$SRC_H5DUMP_TESTFILES/tpbitsMax.ddl
$SRC_H5DUMP_TESTFILES/tpbitsMaxExceeded.ddl
$SRC_H5DUMP_TESTFILES/tpbitsOffsetExceeded.ddl
$SRC_H5DUMP_TESTFILES/tpbitsCharOffsetExceeded.ddl
$SRC_H5DUMP_TESTFILES/tpbitsIntOffsetExceeded.ddl
$SRC_H5DUMP_TESTFILES/tpbitsLongOffsetExceeded.ddl
$SRC_H5DUMP_TESTFILES/tpbitsOffsetNegative.ddl
$SRC_H5DUMP_TESTFILES/tpbitsOverlapped.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSigned.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsigned.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedInt.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedInt.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedLong.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedLong.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedLongLong.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedLongLong.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedWhole.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedWhole.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedIntWhole.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedIntWhole.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedLongWhole.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedLongWhole.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedLongLongWhole.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedLongLongWhole.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedLongLongWhole1.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedLongLongWhole1.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedLongLongWhole63.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedLongLongWhole63.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSigned4.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsigned4.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedInt8.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedInt8.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedLong16.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedLong16.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedLongLong32.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedLongLong32.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSigned2.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsigned2.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedInt4.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedInt4.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedLong8.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedLong8.ddl
$SRC_H5DUMP_TESTFILES/tpbitsSignedLongLong16.ddl
$SRC_H5DUMP_TESTFILES/tpbitsUnsignedLongLong16.ddl
$SRC_H5DUMP_TESTFILES/tbinregR.exp
"

LIST_HDF5_TEST_FILES_XML="
$SRC_H5DUMP_TESTFILES/tbitfields.h5
$SRC_H5DUMP_TESTFILES/tcompound2.h5
$SRC_H5DUMP_TESTFILES/tdset2.h5
$SRC_H5DUMP_TESTFILES/tenum.h5
$SRC_H5DUMP_TESTFILES/textlink.h5
$SRC_H5DUMP_TESTFILES/tloop2.h5
$SRC_H5DUMP_TESTFILES/tmany.h5
$SRC_H5DUMP_TESTFILES/tname-amp.h5
$SRC_H5DUMP_TESTFILES/tname-apos.h5
$SRC_H5DUMP_TESTFILES/tname-gt.h5
$SRC_H5DUMP_TESTFILES/tname-lt.h5
$SRC_H5DUMP_TESTFILES/tname-quot.h5
$SRC_H5DUMP_TESTFILES/tname-sp.h5
$SRC_H5DUMP_TESTFILES/tnodata.h5
$SRC_H5DUMP_TESTFILES/tobjref.h5
$SRC_H5DUMP_TESTFILES/topaque.h5
$SRC_H5DUMP_TESTFILES/tref.h5
$SRC_H5DUMP_TESTFILES/tref-escapes.h5
$SRC_H5DUMP_TESTFILES/tref-escapes-at.h5
$SRC_H5DUMP_TESTFILES/tstring.h5
$SRC_H5DUMP_TESTFILES/tstring-at.h5
"

LIST_OTHER_TEST_FILES_XML="
$SRC_H5DUMP_TESTFILES/tall.h5.xml
$SRC_H5DUMP_TESTFILES/tall-2A.h5.xml
$SRC_H5DUMP_TESTFILES/tarray1.h5.xml
$SRC_H5DUMP_TESTFILES/tarray2.h5.xml
$SRC_H5DUMP_TESTFILES/tarray3.h5.xml
$SRC_H5DUMP_TESTFILES/tarray6.h5.xml
$SRC_H5DUMP_TESTFILES/tarray7.h5.xml
$SRC_H5DUMP_TESTFILES/tattr.h5.xml
$SRC_H5DUMP_TESTFILES/tbitfields.h5.xml
$SRC_H5DUMP_TESTFILES/tcompound_complex.h5.xml
$SRC_H5DUMP_TESTFILES/tcompound.h5.xml
$SRC_H5DUMP_TESTFILES/tcompound2.h5.xml
$SRC_H5DUMP_TESTFILES/tdatareg.h5.xml
$SRC_H5DUMP_TESTFILES/tdset.h5.xml
$SRC_H5DUMP_TESTFILES/tdset2.h5.xml
$SRC_H5DUMP_TESTFILES/tempty.h5.xml
$SRC_H5DUMP_TESTFILES/tempty-dtd.h5.xml
$SRC_H5DUMP_TESTFILES/tempty-dtd-2.h5.xml
$SRC_H5DUMP_TESTFILES/tempty-dtd-uri.h5.xml
$SRC_H5DUMP_TESTFILES/tempty-nons.h5.xml
$SRC_H5DUMP_TESTFILES/tempty-nons-2.h5.xml
$SRC_H5DUMP_TESTFILES/tempty-nons-uri.h5.xml
$SRC_H5DUMP_TESTFILES/tempty-ns.h5.xml
$SRC_H5DUMP_TESTFILES/tempty-ns-2.h5.xml
$SRC_H5DUMP_TESTFILES/tenum.h5.xml
$SRC_H5DUMP_TESTFILES/textlink.h5.xml
$SRC_H5DUMP_TESTFILES/tfpformat.h5.xml
$SRC_H5DUMP_TESTFILES/tgroup.h5.xml
$SRC_H5DUMP_TESTFILES/thlink.h5.xml
$SRC_H5DUMP_TESTFILES/tloop.h5.xml
$SRC_H5DUMP_TESTFILES/tloop2.h5.xml
$SRC_H5DUMP_TESTFILES/tmany.h5.xml
$SRC_H5DUMP_TESTFILES/tname-amp.h5.xml
$SRC_H5DUMP_TESTFILES/tname-apos.h5.xml
$SRC_H5DUMP_TESTFILES/tnamed_dtype_attr.h5.xml
$SRC_H5DUMP_TESTFILES/tname-gt.h5.xml
$SRC_H5DUMP_TESTFILES/tname-lt.h5.xml
$SRC_H5DUMP_TESTFILES/tname-quot.h5.xml
$SRC_H5DUMP_TESTFILES/tname-sp.h5.xml
$SRC_H5DUMP_TESTFILES/tnestedcomp.h5.xml
$SRC_H5DUMP_TESTFILES/tnodata.h5.xml
$SRC_H5DUMP_TESTFILES/tobjref.h5.xml
$SRC_H5DUMP_TESTFILES/topaque.h5.xml
$SRC_H5DUMP_TESTFILES/torderattr1.h5.xml
$SRC_H5DUMP_TESTFILES/torderattr2.h5.xml
$SRC_H5DUMP_TESTFILES/torderattr3.h5.xml
$SRC_H5DUMP_TESTFILES/torderattr4.h5.xml
$SRC_H5DUMP_TESTFILES/tref.h5.xml
$SRC_H5DUMP_TESTFILES/tref-escapes.h5.xml
$SRC_H5DUMP_TESTFILES/tref-escapes-at.h5.xml
$SRC_H5DUMP_TESTFILES/tsaf.h5.xml
$SRC_H5DUMP_TESTFILES/tslink.h5.xml
$SRC_H5DUMP_TESTFILES/tstr.h5.xml
$SRC_H5DUMP_TESTFILES/tstr2.h5.xml
$SRC_H5DUMP_TESTFILES/tstring.h5.xml
$SRC_H5DUMP_TESTFILES/tstring-at.h5.xml
$SRC_H5DUMP_TESTFILES/tudlink.h5.xml
$SRC_H5DUMP_TESTFILES/tvldtypes1.h5.xml
$SRC_H5DUMP_TESTFILES/tvldtypes2.h5.xml
$SRC_H5DUMP_TESTFILES/tvldtypes3.h5.xml
$SRC_H5DUMP_TESTFILES/tvldtypes4.h5.xml
$SRC_H5DUMP_TESTFILES/tvldtypes5.h5.xml
$SRC_H5DUMP_TESTFILES/tvlstr.h5.xml
"

#
# copy test files and expected output files from source dirs to test dir
#
COPY_TESTFILES="$LIST_HDF5_TEST_FILES $LIST_OTHER_TEST_FILES $LIST_HDF5_TEST_FILES_XML $LIST_OTHER_TEST_FILES_XML"

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

# Source in the output filter function definitions.
. $srcdir/../../bin/output_filter.sh

# Run a test and print PASS or *FAIL*.  If a test fails then increment
# the `nerrors' global variable and (if $verbose is set) display the
# difference between the actual output and the expected output. The
# expected output is given as the first argument to this function and
# the actual output file is calculated by replacing the `.ddl' with
# `.out'.  The actual output is not removed if $HDF5_NOCLEANUP has a
# non-zero value.
#
TOOLTEST() {
    expect="$TESTDIR/$1"
    actual="$TESTDIR/`basename $1 .ddl`.out"
    actual_err="$TESTDIR/`basename $1 .ddl`.err"
    actual_sav=${actual}-sav
    actual_err_sav=${actual_err}-sav
    shift

    # Run test.
    TESTING $DUMPER $@
    (
    cd $TESTDIR
      $RUNSERIAL $DUMPER_BIN $@
    ) >$actual 2>$actual_err

    # save actual and actual_err in case they are needed later.
    cp $actual $actual_sav
    STDOUT_FILTER $actual
    cp $actual_err $actual_err_sav
    STDERR_FILTER $actual_err
    cat $actual_err >> $actual

  if [ ! -f $expect ]; then
    # Create the expect file if it doesn't yet exist.
     echo " CREATED"
     cp $actual $expect
    elif $CMP $expect $actual; then
     echo " PASSED"
    else
     echo "*FAILED*"
     echo "    Expected result (*.ddl) differs from actual result (*.out)"
   nerrors="`expr $nerrors + 1`"
   test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    fi

    # Clean up output file
    if test -z "$HDF5_NOCLEANUP"; then
   rm -f $actual $actual_err $actual_sav $actual_err_sav $actual_ext
    fi

}


# same as TOOLTEST1 but compares generated file to expected output
#                   and compares the generated data file to the expected data file
# used for the binary tests that expect a full path in -o without -b
TOOLTEST2() {

    expectdata="$TESTDIR/$1"
    expect="$TESTDIR/`basename $1 .exp`.ddl"
    actualdata="$TESTDIR/`basename $1 .exp`.txt"
    actual="$TESTDIR/`basename $1 .exp`.out"
    actual_err="$TESTDIR/`basename $1 .exp`.err"
    shift

    # Run test.
    TESTING $DUMPER $@
    (
      cd $TESTDIR
      $RUNSERIAL $DUMPER_BIN $@
    ) >$actual 2>$actual_err
    cat $actual_err >> $actual

    if [ ! -f $expect ]; then
    # Create the expect file if it doesn't yet exist.
     echo " CREATED"
     cp $actual $expect
    elif $CMP $expect $actual; then
      if [ ! -f $expectdata ]; then
      # Create the expect data file if it doesn't yet exist.
        echo " CREATED"
        cp $actualdata $expectdata
      elif $CMP $expectdata $actualdata; then
        echo " PASSED"
      else
        echo "*FAILED*"
        echo "    Expected datafile (*.exp) differs from actual datafile (*.txt)"
        nerrors="`expr $nerrors + 1`"
        test yes = "$verbose" && $DIFF $expectdata $actualdata |sed 's/^/    /'
      fi
    else
     echo "*FAILED*"
     echo "    Expected result (*.ddl) differs from actual result (*.out)"
     nerrors="`expr $nerrors + 1`"
     test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    fi
    
    # Clean up output file
    if test -z "$HDF5_NOCLEANUP"; then
     rm -f $actual $actualdata $actual_err
    fi

}

# same as TOOLTEST but filters error stack outp
# Extract file name, line number, version and thread IDs because they may be different
TOOLTEST3() {

    expect="$TESTDIR/$1"
    actual="$TESTDIR/`basename $1 .ddl`.out"
    actual_err="$TESTDIR/`basename $1 .ddl`.err"
    actual_ext="$TESTDIR/`basename $1 .ddl`.ext"
    actual_sav=${actual}-sav
    actual_err_sav=${actual_err}-sav
    shift

    # Run test.
    TESTING $DUMPER $@
    (
      cd $TESTDIR
      $RUNSERIAL $DUMPER_BIN $@
    ) >$actual 2>$actual_err

    # save actual and actual_err in case they are needed later.
    cp $actual $actual_sav
    STDOUT_FILTER $actual
    cp $actual_err $actual_err_sav
    STDERR_FILTER $actual_err

    # Extract file name, line number, version and thread IDs because they may be different
    sed -e 's/thread [0-9]*/thread (IDs)/' -e 's/: .*\.c /: (file name) /' \
        -e 's/line [0-9]*/line (number)/' \
        -e 's/v[1-9]*\.[0-9]*\./version (number)\./' \
        -e 's/[1-9]*\.[0-9]*\.[0-9]*[^)]*/version (number)/' \
        -e 's/H5Eget_auto[1-2]*/H5Eget_auto(1 or 2)/' \
        -e 's/H5Eset_auto[1-2]*/H5Eset_auto(1 or 2)/' \
     $actual_err > $actual_ext
    cat $actual_ext >> $actual

    if [ ! -f $expect ]; then
    # Create the expect file if it doesn't yet exist.
     echo " CREATED"
     cp $actual $expect
    elif $CMP $expect $actual; then
     echo " PASSED"
    else
     echo "*FAILED*"
     echo "    Expected result (*.ddl) differs from actual result (*.out)"
     nerrors="`expr $nerrors + 1`"
     test yes = "$verbose" && $DIFF $expect $actual |sed 's/^/    /'
    fi

    # Clean up output file
    if test -z "$HDF5_NOCLEANUP"; then
   rm -f $actual $actual_err $actual_sav $actual_err_sav
    fi

}

# Print a "SKIP" message
SKIP() {
   TESTING $DUMPER $@
    echo  " -SKIP-"
}
  
# Print a line-line message left justified in a field of 70 characters
#
PRINT_H5DIFF() {
 SPACES="                                                               "
 echo " Running h5diff $* $SPACES" | cut -c1-70 | tr -d '\012'
}


# Call the h5diff tool
#
DIFFTEST() 
{
    PRINT_H5DIFF  $@
    (
  cd $TESTDIR
  $RUNSERIAL $H5DIFF_BIN "$@" -q
    )
    RET=$?
    if [ $RET != 0 ] ; then
         echo "*FAILED*"
         nerrors="`expr $nerrors + 1`"
    else
         echo " PASSED"
    fi
        
}

# Print a line-line message left justified in a field of 70 characters
# beginning with the word "Verifying".
#
PRINT_H5IMPORT() {
 SPACES="                                                               "
 echo " Running h5import $* $SPACES" | cut -c1-70 | tr -d '\012'
}

# Call the h5import tool
#
IMPORTTEST() 
{
    # remove the output hdf5 file if it exists
    hdf5_file="$TESTDIR/$5"
    if [ -f $hdf5_file ]; then
     rm -f $hdf5_file
    fi

    PRINT_H5IMPORT  $@
    (
  cd $TESTDIR
  $RUNSERIAL $H5IMPORT_BIN "$@" 
    )
    RET=$?
    if [ $RET != 0 ] ; then
         echo "*FAILED*"
         nerrors="`expr $nerrors + 1`"
    else
         echo " PASSED"
    fi
        
}


##############################################################################
##############################################################################
###        T H E   T E S T S                                            ###
##############################################################################
##############################################################################
# prepare for test
COPY_TESTFILES_TO_TESTDIR

# test for signed/unsigned datasets
TOOLTEST packedbits.ddl packedbits.h5
# test for displaying groups
TOOLTEST tgroup-1.ddl tgroup.h5
# test for displaying the selected groups
TOOLTEST tgroup-2.ddl --group=/g2 --group / -g /y tgroup.h5

# test for displaying simple space datasets
TOOLTEST tdset-1.ddl tdset.h5
# test for displaying selected datasets
TOOLTEST tdset-2.ddl -H -d dset1 -d /dset2 --dataset=dset3 tdset.h5

# test for displaying attributes
TOOLTEST tattr-1.ddl tattr.h5
# test for displaying the selected attributes of string type and scalar space
TOOLTEST tattr-2.ddl -a /attr1 --attribute /attr4 --attribute=/attr5 tattr.h5
# test for header and error messages
TOOLTEST tattr-3.ddl --header -a /attr2 --attribute=/attr tattr.h5
# test for displaying attributes in shared datatype (also in group and dataset)
TOOLTEST tnamed_dtype_attr.ddl tnamed_dtype_attr.h5

# test for displaying soft links and user-defined links
TOOLTEST tslink-1.ddl tslink.h5
TOOLTEST tudlink-1.ddl tudlink.h5
# test for displaying the selected link
TOOLTEST tslink-2.ddl -l slink2 tslink.h5
TOOLTEST tudlink-2.ddl -l udlink2 tudlink.h5

# tests for hard links
TOOLTEST thlink-1.ddl thlink.h5
TOOLTEST thlink-2.ddl -d /g1/dset2 --dataset /dset1 --dataset=/g1/g1.1/dset3 thlink.h5
TOOLTEST thlink-3.ddl -d /g1/g1.1/dset3 --dataset /g1/dset2 --dataset=/dset1 thlink.h5
TOOLTEST thlink-4.ddl -g /g1 thlink.h5
TOOLTEST thlink-5.ddl -d /dset1 -g /g2 -d /g1/dset2 thlink.h5

# tests for compound data types
TOOLTEST tcomp-1.ddl tcompound.h5
# test for named data types
TOOLTEST tcomp-2.ddl -t /type1 --datatype /type2 --datatype=/group1/type3 tcompound.h5
# test for unamed type 
TOOLTEST tcomp-3.ddl -t /#6632 -g /group2 tcompound.h5
# test complicated compound datatype
TOOLTEST tcomp-4.ddl tcompound_complex.h5

#test for the nested compound type
TOOLTEST tnestcomp-1.ddl tnestedcomp.h5

# test for options
TOOLTEST tall-1.ddl tall.h5
TOOLTEST tall-2.ddl --header -g /g1/g1.1 -a attr2 tall.h5
TOOLTEST tall-3.ddl -d /g2/dset2.1 -l /g1/g1.2/g1.2.1/slink tall.h5

# test for loop detection
TOOLTEST tloop-1.ddl tloop.h5

# test for string 
TOOLTEST tstr-1.ddl tstr.h5
TOOLTEST tstr-2.ddl tstr2.h5

# test for file created by Lib SAF team
TOOLTEST tsaf.ddl tsaf.h5

# test for file with variable length data
TOOLTEST tvldtypes1.ddl tvldtypes1.h5
TOOLTEST tvldtypes2.ddl tvldtypes2.h5
TOOLTEST tvldtypes3.ddl tvldtypes3.h5
TOOLTEST tvldtypes4.ddl tvldtypes4.h5
TOOLTEST tvldtypes5.ddl tvldtypes5.h5

#test for file with variable length string data
TOOLTEST tvlstr.ddl tvlstr.h5

# test for files with array data
TOOLTEST tarray1.ddl tarray1.h5
# # added for bug# 2092 - tarray1_big.h
TOOLTEST tarray1_big.ddl -R tarray1_big.h5
TOOLTEST tarray2.ddl tarray2.h5
TOOLTEST tarray3.ddl tarray3.h5
TOOLTEST tarray4.ddl tarray4.h5
TOOLTEST tarray5.ddl tarray5.h5
TOOLTEST tarray6.ddl tarray6.h5
TOOLTEST tarray7.ddl tarray7.h5
TOOLTEST tarray8.ddl tarray8.h5

# test for files with empty data
TOOLTEST tempty.ddl tempty.h5

# test for files with groups that have comments
TOOLTEST tgrp_comments.ddl tgrp_comments.h5

# test the --filedriver flag
TOOLTEST tsplit_file.ddl --filedriver=split tsplit_file
TOOLTEST tfamily.ddl --filedriver=family tfamily%05d.h5
TOOLTEST tmulti.ddl --filedriver=multi tmulti

# test for files with group names which reach > 1024 bytes in size
TOOLTEST tlarge_objname.ddl -w157 tlarge_objname.h5

# test '-A' to suppress data but print attr's
TOOLTEST tall-2A.ddl -A tall.h5

# test '-r' to print attributes in ASCII instead of decimal
TOOLTEST tall-2B.ddl -A -r tall.h5

# test Subsetting
TOOLTEST tall-4s.ddl --dataset=/g1/g1.1/dset1.1.1 --start=1,1 --stride=2,3 --count=3,2 --block=1,1 tall.h5
TOOLTEST tall-5s.ddl -d "/g1/g1.1/dset1.1.2[0;2;10;]" tall.h5
TOOLTEST tdset-3s.ddl -d "/dset1[1,1;;;]" tdset.h5


# test printing characters in ASCII instead of decimal
TOOLTEST tchar1.ddl -r tchar.h5

# test failure handling
# Missing file name
TOOLTEST tnofilename.ddl

# rev. 2004

# tests for super block
TOOLTEST tboot1.ddl -H -B -d dset tfcontents1.h5
TOOLTEST tboot2.ddl -B tfcontents2.h5

# test -p with a non existing dataset
TOOLTEST tperror.ddl -p -d bogus tfcontents1.h5

# test for file contents
TOOLTEST tcontents.ddl -n tfcontents1.h5

# tests for storage layout
# compact
TOOLTEST tcompact.ddl -H -p -d compact tfilters.h5
# contiguous
TOOLTEST tcontiguos.ddl -H -p -d contiguous tfilters.h5
# chunked
TOOLTEST tchunked.ddl -H -p -d chunked tfilters.h5
# external 
TOOLTEST texternal.ddl -H -p -d external tfilters.h5

# fill values
TOOLTEST tfill.ddl -p tfvalues.h5

# several datatype, with references , print path
TOOLTEST treference.ddl  tattr2.h5

# escape/not escape non printable characters
TOOLTEST tstringe.ddl -e tstr3.h5
TOOLTEST tstring.ddl tstr3.h5
# char data as ASCII with non escape
TOOLTEST tstring2.ddl -r -d str4 tstr3.h5

# array indices print/not print
TOOLTEST tindicesyes.ddl taindices.h5
TOOLTEST tindicesno.ddl -y taindices.h5

########## array indices with subsetting
# 1D case
TOOLTEST tindicessub1.ddl -d 1d -s 1 -S 10 -c 2  -k 3 taindices.h5

# 2D case
TOOLTEST tindicessub2.ddl -d 2d -s 1,2  -S 3,3 -c 3,2 -k 2,2 taindices.h5

# 3D case
TOOLTEST tindicessub3.ddl -d 3d -s 0,1,2 -S 1,3,3 -c 2,2,2  -k 1,2,2  taindices.h5

# 4D case
TOOLTEST tindicessub4.ddl -d 4d -s 0,0,1,2  -c 2,2,3,2 -S 1,1,3,3 -k 1,1,2,2  taindices.h5

#Exceed the dimensions for subsetting
TOOLTEST texceedsubstart.ddl -d 1d -s 1,3 taindices.h5
TOOLTEST texceedsubcount.ddl -d 1d -c 1,3 taindices.h5
TOOLTEST texceedsubstride.ddl -d 1d -S 1,3 taindices.h5
TOOLTEST texceedsubblock.ddl -d 1d -k 1,3 taindices.h5


# tests for filters
# SZIP
option="-H -p -d szip tfilters.h5"
if test $USE_FILTER_SZIP != "yes"; then
 SKIP $option
else
TOOLTEST tszip.ddl $option
fi
# deflate
option="-H -p -d deflate tfilters.h5"
if test $USE_FILTER_DEFLATE != "yes"; then
 SKIP $option
else
 TOOLTEST tdeflate.ddl $option
fi
# shuffle
option="-H -p -d shuffle tfilters.h5"
if test $USE_FILTER_SHUFFLE != "yes"; then
 SKIP $option
else
 TOOLTEST tshuffle.ddl $option
fi
# fletcher32
option="-H -p -d fletcher32  tfilters.h5"
if test $USE_FILTER_FLETCHER32 != "yes"; then
 SKIP $option
else
 TOOLTEST tfletcher32.ddl $option
fi
# nbit
option="-H -p -d nbit  tfilters.h5"
if test $USE_FILTER_NBIT != "yes"; then
 SKIP $option
else
 TOOLTEST tnbit.ddl $option
fi
# scaleoffset
option="-H -p -d scaleoffset  tfilters.h5"
if test $USE_FILTER_SCALEOFFSET != "yes"; then
 SKIP $option
else
 TOOLTEST tscaleoffset.ddl $option
fi
# all
option="-H -p -d all  tfilters.h5"
if test $USE_FILTER_FLETCHER32 != "yes" -o  $USE_FILTER_SZIP != "yes" -o  $USE_FILTER_DEFLATE != "yes" -o  $USE_FILTER_SHUFFLE != "yes" -o $USE_FILTER_NBIT != "yes" -o  $USE_FILTER_SCALEOFFSET != "yes"; then
 SKIP $option
else
 TOOLTEST tallfilters.ddl $option
fi
# user defined
TOOLTEST tuserfilter.ddl -H  -p -d myfilter  tfilters.h5

# test for displaying objects with very long names
TOOLTEST tlonglinks.ddl tlonglinks.h5

# dimensions over 4GB, print boundary 
TOOLTEST tbigdims.ddl -d dset4gb -s 4294967284 -c 22 tbigdims.h5

# hyperslab read
TOOLTEST thyperslab.ddl thyperslab.h5


#
    
# test for displaying dataset and attribute of null space
TOOLTEST tnullspace.ddl tnullspace.h5

# test for displaying dataset and attribute of space with 0 dimension size
TOOLTEST zerodim.ddl zerodim.h5

# test for long double (some systems do not have long double)
#TOOLTEST tldouble.ddl tldouble.h5

# test for vms
TOOLTEST tvms.ddl tvms.h5

# test for binary output
TOOLTEST   tbin1.ddl -d integer -o out1.bin -b LE    tbinary.h5

# NATIVE default. the NATIVE test can be validated with h5import/h5diff
TOOLTEST   tbin1.ddl -d integer -o out1.bin  -b      tbinary.h5
IMPORTTEST out1.bin -c out3.h5import -o out1.h5
DIFFTEST   tbinary.h5 out1.h5 /integer /integer

TOOLTEST   tbin2.ddl -b BE -d float  -o out2.bin      tbinary.h5

# the NATIVE test can be validated with h5import/h5diff
TOOLTEST   tbin3.ddl -d integer -o out3.bin -b NATIVE tbinary.h5
IMPORTTEST out3.bin -c out3.h5import -o out3.h5
DIFFTEST   tbinary.h5 out3.h5 /integer /integer

TOOLTEST   tbin4.ddl -d double  -b FILE -o out4.bin    tbinary.h5
   
# Clean up binary output files
if test -z "$HDF5_NOCLEANUP"; then
 rm -f out[1-4].bin
 rm -f out1.h5
 rm -f out3.h5
fi

# test for dataset region references 
TOOLTEST tdatareg.ddl tdatareg.h5
TOOLTEST tdataregR.ddl -R tdatareg.h5
TOOLTEST tattrreg.ddl tattrreg.h5
TOOLTEST tattrregR.ddl -R tattrreg.h5

TOOLTEST2   tbinregR.exp -d /Dataset1 -s 0 -R -y -o tbinregR.txt    tdatareg.h5

# Clean up text output files
if test -z "$HDF5_NOCLEANUP"; then
 rm -f tbinregR.txt
fi

# tests for group creation order
# "1" tracked, "2" name, root tracked
TOOLTEST tordergr1.ddl --group=1 --sort_by=creation_order --sort_order=ascending tordergr.h5
TOOLTEST tordergr2.ddl --group=1 --sort_by=creation_order --sort_order=descending tordergr.h5
TOOLTEST tordergr3.ddl -g 2 -q name -z ascending tordergr.h5
TOOLTEST tordergr4.ddl -g 2 -q name -z descending tordergr.h5
TOOLTEST tordergr5.ddl -q creation_order tordergr.h5

# tests for attribute order
TOOLTEST torderattr1.ddl -H --sort_by=name --sort_order=ascending torderattr.h5
TOOLTEST torderattr2.ddl -H --sort_by=name --sort_order=descending torderattr.h5
TOOLTEST torderattr3.ddl -H --sort_by=creation_order --sort_order=ascending torderattr.h5
TOOLTEST torderattr4.ddl -H --sort_by=creation_order --sort_order=descending torderattr.h5

# tests for floating point user defined printf format
TOOLTEST tfpformat.ddl -m %.7f tfpformat.h5

# tests for traversal of external links
TOOLTEST textlinksrc.ddl textlinksrc.h5
TOOLTEST textlinkfar.ddl textlinkfar.h5

# test for dangling external links
TOOLTEST textlink.ddl textlink.h5

# test for error stack display (BZ2048)
TOOLTEST3 filter_fail.ddl --enable-error-stack filter_fail.h5

# test for -o -y for dataset with attributes
TOOLTEST tall-6.ddl -y -o data -d /g1/g1.1/dset1.1.1 tall.h5

# test for dataset packed bits 
# Limits:
# Maximum number of packed bits is 8 (for now).
# Maximum integer size is 64 (for now).
# Maximun Offset is 63 (Maximum size - 1).
# Maximum Offset+Length is 64 (Maximum size).
# Tests:
# Normal operation on both signed and unsigned int datasets.
# Sanity check
# Their rawdata output should be the same.
TOOLTEST tpbitsSignedWhole.ddl -d /DS08BITS -M 0,8 packedbits.h5
TOOLTEST tpbitsUnsignedWhole.ddl -d /DU08BITS -M 0,8 packedbits.h5
TOOLTEST tpbitsSignedIntWhole.ddl -d /DS16BITS -M 0,16 packedbits.h5
TOOLTEST tpbitsUnsignedIntWhole.ddl -d /DU16BITS -M 0,16 packedbits.h5
TOOLTEST tpbitsSignedLongWhole.ddl -d /DS32BITS -M 0,32 packedbits.h5
TOOLTEST tpbitsUnsignedLongWhole.ddl -d /DU32BITS -M 0,32 packedbits.h5
TOOLTEST tpbitsSignedLongLongWhole.ddl -d /DS64BITS -M 0,64 packedbits.h5
TOOLTEST tpbitsUnsignedLongLongWhole.ddl -d /DU64BITS -M 0,64 packedbits.h5
TOOLTEST tpbitsSignedLongLongWhole63.ddl -d /DS64BITS -M 0,63 packedbits.h5
TOOLTEST tpbitsUnsignedLongLongWhole63.ddl -d /DU64BITS -M 0,63 packedbits.h5
TOOLTEST tpbitsSignedLongLongWhole1.ddl -d /DS64BITS -M 1,63 packedbits.h5
TOOLTEST tpbitsUnsignedLongLongWhole1.ddl -d /DU64BITS -M 1,63 packedbits.h5
# Half sections
TOOLTEST tpbitsSigned4.ddl -d /DS08BITS -M 0,4,4,4 packedbits.h5
TOOLTEST tpbitsUnsigned4.ddl -d /DU08BITS -M 0,4,4,4 packedbits.h5
TOOLTEST tpbitsSignedInt8.ddl -d /DS16BITS -M 0,8,8,8 packedbits.h5
TOOLTEST tpbitsUnsignedInt8.ddl -d /DU16BITS -M 0,8,8,8 packedbits.h5
TOOLTEST tpbitsSignedLong16.ddl -d /DS32BITS -M 0,16,16,16 packedbits.h5
TOOLTEST tpbitsUnsignedLong16.ddl -d /DU32BITS -M 0,16,16,16 packedbits.h5
TOOLTEST tpbitsSignedLongLong32.ddl -d /DS64BITS -M 0,32,32,32 packedbits.h5
TOOLTEST tpbitsUnsignedLongLong32.ddl -d /DU64BITS -M 0,32,32,32 packedbits.h5
# Quarter sections
TOOLTEST tpbitsSigned2.ddl -d /DS08BITS -M 0,2,2,2,4,2,6,2 packedbits.h5
TOOLTEST tpbitsUnsigned2.ddl -d /DU08BITS -M 0,2,2,2,4,2,6,2 packedbits.h5
TOOLTEST tpbitsSignedInt4.ddl -d /DS16BITS -M 0,4,4,4,8,4,12,4 packedbits.h5
TOOLTEST tpbitsUnsignedInt4.ddl -d /DU16BITS -M 0,4,4,4,8,4,12,4 packedbits.h5
TOOLTEST tpbitsSignedLong8.ddl -d /DS32BITS -M 0,8,8,8,16,8,24,8 packedbits.h5
TOOLTEST tpbitsUnsignedLong8.ddl -d /DU32BITS -M 0,8,8,8,16,8,24,8 packedbits.h5
TOOLTEST tpbitsSignedLongLong16.ddl -d /DS64BITS -M 0,16,16,16,32,16,48,16 packedbits.h5
TOOLTEST tpbitsUnsignedLongLong16.ddl -d /DU64BITS -M 0,16,16,16,32,16,48,16 packedbits.h5
# Begin and End
TOOLTEST tpbitsSigned.ddl -d /DS08BITS -M 0,2,2,6 packedbits.h5
TOOLTEST tpbitsUnsigned.ddl -d /DU08BITS -M 0,2,2,6 packedbits.h5
TOOLTEST tpbitsSignedInt.ddl -d /DS16BITS -M 0,2,10,6 packedbits.h5
TOOLTEST tpbitsUnsignedInt.ddl -d /DU16BITS -M 0,2,10,6 packedbits.h5
TOOLTEST tpbitsSignedLong.ddl -d /DS32BITS -M 0,2,26,6 packedbits.h5
TOOLTEST tpbitsUnsignedLong.ddl -d /DU32BITS -M 0,2,26,6 packedbits.h5
TOOLTEST tpbitsSignedLongLong.ddl -d /DS64BITS -M 0,2,58,6 packedbits.h5
TOOLTEST tpbitsUnsignedLongLong.ddl -d /DU64BITS -M 0,2,58,6 packedbits.h5
# Overlapped packed bits.
TOOLTEST tpbitsOverlapped.ddl -d /DS08BITS -M 0,1,1,1,2,1,0,3 packedbits.h5
# Maximum number of packed bits.
TOOLTEST tpbitsMax.ddl -d /DS08BITS -M 0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5
# Compound type.
TOOLTEST tpbitsCompound.ddl -d /dset1 -M 0,1,1,1 tcompound.h5
# Array type.
TOOLTEST tpbitsArray.ddl -d /Dataset1 -M 0,1,1,1 tarray1.h5
# Test Error handling.
# Too many packed bits requested. Max is 8 for now.
TOOLTEST tpbitsMaxExceeded.ddl -d /DS08BITS -M 0,1,0,1,1,1,2,1,3,1,4,1,5,1,6,1,7,1 packedbits.h5
# Offset too large. Max is 7 (8-1) for now.
TOOLTEST tpbitsOffsetExceeded.ddl -d /DS08BITS -M 64,1 packedbits.h5
TOOLTEST tpbitsCharOffsetExceeded.ddl -d /DS08BITS -M 8,1 packedbits.h5
TOOLTEST tpbitsIntOffsetExceeded.ddl -d /DS16BITS -M 16,1 packedbits.h5
TOOLTEST tpbitsLongOffsetExceeded.ddl -d /DS32BITS -M 32,1 packedbits.h5
# Bad offset, must not be negative.
TOOLTEST tpbitsOffsetNegative.ddl -d /DS08BITS -M -1,1 packedbits.h5
# Bad length, must not be positive.
TOOLTEST tpbitsLengthPositive.ddl -d /DS08BITS -M 4,0 packedbits.h5
# Offset+Length is too large. Max is 8 for now.
TOOLTEST tpbitsLengthExceeded.ddl -d /DS08BITS -M 37,28 packedbits.h5
TOOLTEST tpbitsCharLengthExceeded.ddl -d /DS08BITS -M 2,7 packedbits.h5
TOOLTEST tpbitsIntLengthExceeded.ddl -d /DS16BITS -M 10,7 packedbits.h5
TOOLTEST tpbitsLongLengthExceeded.ddl -d /DS32BITS -M 26,7 packedbits.h5
# Incomplete pair of packed bits request.
TOOLTEST tpbitsIncomplete.ddl -d /DS08BITS -M 0,2,2,1,0,2,2, packedbits.h5


# Report test results and exit
if test $nerrors -eq 0 ; then
    echo "All $TESTNAME tests passed."
    exit $EXIT_SUCCESS
else
    echo "$TESTNAME tests failed with $nerrors errors."
    exit $EXIT_FAILURE
fi
