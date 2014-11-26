#!/bin/sh
#-----------------------------------------------------------------------
#
# FILE NAME      : run_test
#
# TYPE           : shell script
#
# PURPOSE        : Builds and runs example encode and decode programs
#
#
# REVISION INFO  :
#
# $Source:$
# $Date: 16/02/2012 11:40:44$
# $Revision: 1$
#
# $Author: Sheila Needham$
#
# $Log:
#  1    Met_DB_Project 1.0         16/02/2012 11:40:44    Sheila Needham  New
#       script to run test encode and decode programs
# $
#-----------------------------------------------------------------------
# (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.
#
# Met Office, United Kingdom
#
# The use, duplication and disclosure of this code is strictly
# prohibited without the permission of The Meteorological Database 
# Team at the above address.
#-----------------------------------------------------------------------

if [ $# -ne 1 ]
then
  echo 'Usage:run_test.sh <fortran_compiler>'
  exit 1
fi 

FF=$1
export BUFR_LIBRARY=./
export BUFRDEC_FILE=./TestMessage.bufr

rm -f encode.exe decode.exe > /dev/null 2>&1

$FF -o encode.exe -g BUFRencode.f90 libbufr.a
$FF -o decode.exe -g BUFRdecode.f90 libbufr.a

if [ -f ./encode.exe ] && [ -f ./decode.exe ]
then
  ./encode.exe
  ./decode.exe

else
  echo 'Failed to compile one or more test programs.'
  exit 8
  
fi

exit 0
