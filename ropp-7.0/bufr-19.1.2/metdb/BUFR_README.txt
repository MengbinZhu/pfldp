                    ===============================

                       UK Meteorological Office
 
                      BUFR Encode/Decode Software

                           metdb@metoffice.gov.uk

                    ===============================

        (C) CROWN COPYRIGHT 2012 - MET OFFICE. All Rights Reserved.

                      Met Office, United Kingdom

       The use, duplication and disclosure of this code is strictly
     prohibited without the permission of The Meteorological Database 
                      Team at the above address.

                    ===============================

Contents:

1.  Introduction
2.  Quick Start
3.  How to use the BUFR tables
4.  Compile the BUFR source
5.  To encode a BUFR message
6.  To decode a BUFR message
7.  Troubleshooting
8.  Fortran-C interlanguage calls
9.  Y2K statement
10.  End

----------------------------------------------------------------------------
1. Introduction
----------------------------------------------------------------------------

The MetDB BUFR software has been tested on a Linux machine running
RHEL6.1 with Intel ifort v12.0_64, nagfor 5.2_64, pgfortran 11.7_64 and
GNU gcc 4.4.4.

MetDB BUFR software will also work on IBM z/OS 01.10.00 and z/Linux, although
this is not documented here.

This tar file when unpacked will contain the following:

  a) The Fortran BUFR source subroutines (*.F90 and *.f90)

  b) The BUFR tables TABLEB, TABLED and CODEFIG

  c) BUFR encoding/decoding main programs (BUFRencode.f90, BUFRdecode.f90)
     to call the BUFR routines contained in (a). These are example programs
     for adaptation. 
  
  d) Shell script to build and run the sample encode/decode programs.
     run_test.sh <compiler>    

  e) C routines for I/O. These are used to read/write BUFR messages.
     MetDB_c_utils.c
  
  f) Makefile : Makefile_BUFRrelease

  g) BUFR encoding/decoding technical note (dmtn1.html).

  h) A list of files/versions contained in the tar file (ver19.1.00_contents.html).       

  i) A software license document (SoftwareLicense.doc)

  j) This BUFR_README.txt file.

  k) Release Notes e.g. ver19.1.00_Release_Notes.txt


NOTE: Some fortran files are suffixed with a .F90 extension - this is because
      these files contain C pre-processor statements. The .F90 extension tells 
      the fortran compiler to invoke the C pre-processor before fortran 
      compiling. The fortran compiler you use must be able to handle 
      the .F90 extension.

NOTE: ifort (Intel Fortran 90) compilation is described in the notes below. You
      may change this to another compiler if you wish.

NOTE: It is safest to be consistent with fortran compilers. i.e. if
      libbufr.a is produced by compiling source with the ifort compiler,
      then the BUFRencode.f90 or BUFRdecode.f90 source should also be compiled
      with the ifort compiler, otherwise unsatisfied symbols may occur.
      Mod files produced by Intel and Nag compilers are not interchangeable.

NOTE: The BUFR encode/decode software is written in FORTRAN90, but it
      does make use of C routines to perform I/O. This means there are
      FORTRAN to C interlanguage calls - most importantly of character
      variables. These interlanguage calls are compiler-dependant. It
      might be necessary for changes to be made to the MetDB_c_utils.c
      file - but hopefully not. Please read section 8 at the end of this
      file for more details.
      
----------------------------------------------------------------------------
2. Quick Start
----------------------------------------------------------------------------

The Makefile is set up for RHEL6 using ifort and gcc compilers.  It will 
compile and build the BUFR code into a library (libbufr.a) then compile and 
run the sample encode and decode programs to check the set-up.

If it is successful you will eventually see output from the encode and decode
programs, ending with:

 Obs in message      =        2
 Descriptors per ob  =        8

       BUFR version and Master Table .........   4   0
       BUFR Master & Local Table versions ....  14   0
       Originating centre & subcentre ........  74   0
       Update sequence & Section 2 flag ......   0   F
       Data category and sub-categories ...... 255   0   0
       Date and time .................... 2010  10  14   9  19  30

 Observation number :    1

 0001  005002        60.000
 0002  006002       120.000
 0003  100004
 0004  012001       273.000
 0005  012001       274.000
 0006  012001       275.000
 0007  012001       276.000
 0008  001026    524289.000

 Observation number :    2

 0001  005002        30.000
 0002  006002        90.000
 0003  100004
 0004  012001       300.000
 0005  012001       301.000
 0006  012001       302.000
 0007  012001       303.000
 0008  001026    524297.000
 Message read. IRC=          -1

----------------------------------------------------------------------------
3. How to use the BUFR tables
----------------------------------------------------------------------------

The BUFR routines need to know the locations of the BUFR tables TABLEB,
TABLED and CODEFIG.

  a) Set the environment variable BUFR_LIBRARY to point to the library
     containing TABLEB, TABLED and CODEFIG.

     e.g. export BUFR_LIBRARY=/u/m12/t12sc/BUFR/

     NOTE: the final '/' is necessary.

     NOTE: If using this option, the BUFR source in section 4 below must be
     been compiled WITH -DBPATH in FFLAGS in the Makefile i.e.

     in Makefile_BUFRrelease:      FFLAGS        = -DBPATH

----------------------------------------------------------------------------
4. Compile the BUFR source
----------------------------------------------------------------------------
    
  a) In the main directory, produce the BUFR library file libbufr.a using
     the Makefile provided. NOTE: libbufr.a is a collection of object code.
     
          make -f Makefile_BUFRrelease
 
  b) Failing this, each source routine may be compiled individually. See
     the Makefile for the source list.

     e.g. ifort -c -g -DBPATH code.F90

     The library libbufr.a can be made from the individual source:

          ar r libbufr.a *.o

          
  NOTE: It may be necessary to change the compiler optins FFLAGS and/or
        CFLAGS in Makefile_BUFRrelease to suit your system.

----------------------------------------------------------------------------
5. To encode a BUFR message
----------------------------------------------------------------------------

  a) An example BUFR encode program is included - BUFRencode.f90
     The filename for the BUFR file produced is hard-coded in this
     source, currently "TestMessage.bufr"

  b) compile & link your main program e.g.
     
          ifort -g BUFRencode.f90 libbufr.a
     
  c) run the executable 

          ./a.out

----------------------------------------------------------------------------
6. To decode a BUFR message
----------------------------------------------------------------------------
         
  a) An example BUFR decode program is included - BUFRdecode.f90

  b) compile & link your main program e.g.
         
          ifort -g BUFRdecode.f90 libbufr.a
    
  c) The example program BUFRdecode.f90 reads the environment variable
     BUFRDEC_FILE for the filename of the BUFR file you wish to decode.

     You must set this environment variable before running the
     executable.

     e.g. to BUFR decode the "TestMessage.bufr" produced by section 5:

     export BUFRDEC_FILE=/u/m12/t12sc/BUFR/TestMessage.bufr
     
  d) run the executable
     
          ./a.out

----------------------------------------------------------------------------
7. Troubleshooting
----------------------------------------------------------------------------

  a) BUFRPATH: ERROR: Environment variable BUFR_LIBRARY not set

     This means you have compiled the BUFR source WITH the -DBPATH compiler
     option, but when running the executable, the environment variable
     BUFR_LIBRARY is not set. See section 2 for instructions on how to set
     it.

  b) TABLEB: ERROR - File /...directorypath.../TABLEB not found  or
     TABLED: ERROR - File /...directorypath.../TABLED not found  or
     CODE: ERROR - File /...directorypath.../CODEFIG not found

     This means you have compiled the BUFR source WITH the -DBPATH compiler
     option and set the environment variable BUFR_LIBRARY. However, the
     BUFR software cannot find the file(s) TABLEB, TABLED, CODEFIG in the
     directory set in BUFR_LIBRARY. Check that the path is correct.
     Note: You need to code the last '/' in the directory path. If the path
     is correct, check the files exist. See sections 3 and 4 for further
     info.

  c) TABLEB: ERROR - File TABLEB not found  or
     TABLED: ERROR - File TABLED not found  or
     CODE: ERROR - File CODEFIG not found

     This means you have compiled the BUFR source WITHOUT the -DBPATH
     compiler option. Check that the TABLEB, TABLED, CODEFIG tables or
     symbolic links to them exist in the directory your executable runs in.
     See section 3 for further info.

  d) BUFRdecode: ERROR: BUFRDEC_FILE environment not set

     This is an error from the example BUFR encode main program. It
     requires the environment variable BUFRDEC_FILE to contain the name
     of the file you wish to BUFR decode. See section 6 for further info.
     
  e) undefined metdb_getenv_ or metdb_copen_ or metdb_cread_ or
               metdb_cwrite_ or metdb_cclose_
                
     This means the routine MetDB_C_utils needs to be compiled with
     CFLAGS = -DUNDERSCORE in Makefile_BUFRrelease.

  f) undefined metdb_getenv__ or metdb_copen__ or metdb_cread__ or
               metdb_cwrite__ or metdb_cclose__
                
     This means the routine MetDB_C_utils needs to be compiled with
     CFLAGS = -DDUNDERSCORE in Makefile_BUFRrelease.

  g) Check the NOTES in the introduction for further information.

----------------------------------------------------------------------------
8. Fortran-C interlanguage calls
----------------------------------------------------------------------------

C routines are used in the MetDB BUFR software for binary I/O and for
getenv calls. Character variables are passed from fortran to C routines.
A Fortran CHARACTER variable is characterized by two items of information
(the address and the length). Unfortunately, the mechanism for passing
these two values is compiler dependent.

MetDB_c_utils.c accepts the length of a character variable as an integer
variable at the end of the C argument list. No change is required to the
fortran calling code. This is the most common method of passing character
variables from fortran to C and a simple example is shown below:

SUBROUTINE FORT(IVAR,CH,RVAR)
INTEGER   IVAR
CHARACTER CVAR
REAL      RVAR
CALL C(IVAR,CVAR,RVAR)
STOP
END

corresponding C routine:

void C(ivar,cvar,rvar,len_cvar)
int  *ivar;
char *cvar;
real *rvar;
int  len_cvar;
{
  ....
}


It may be necessary depending on the system and complier you are using to
change the way the variables are passed between fortran and C, but
hopefully not.

----------------------------------------------------------------------------
8. Y2K statement
----------------------------------------------------------------------------

Y2K Certificate:
MetDB BUFR code was tested by the MetDB Team for Y2K compliance during 
February 1999. 

TO THE BEST OF OUR ABILITY THE METDB TEAM CERTIFIES THAT THIS BUFR RELEASE
IS Y2K COMPLIANT. Signed: M. OULDRIDGE (Database Manager)

----------------------------------------------------------------------------
9. End

$Revision: 2$ 
$Date: 16/02/2012 11:37:40$
$Author: Sheila Needham$ 
$Folder: BUFR Package$
$Workfile: BUFR_README.txt$
---------------------------------------------------------------------------
