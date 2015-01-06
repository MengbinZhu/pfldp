#!/bin/sh
#Running scripts for the main program
./mainprogram > output 2>&1

IDIMEn=$(awk 'NR==1 {print $1}' output)
IDIMV=$(awk 'NR==2 {print $1}' output)
echo $IDIMEn
echo $IDIMV
mv output output_${IDIMV}_En${IDIMEn}
