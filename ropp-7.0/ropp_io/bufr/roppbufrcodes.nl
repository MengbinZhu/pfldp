! $Id: roppbufrcodes.nl 3551 2013-02-25 09:51:28Z idculv $
!---------------------------------------------------------------
! NAMELIST file for ROPP BUFR encoder/decoder
!---------------------------------------------------------------
! Add new pairs of alpha-numeric character ID and associated
! BUFR code to the following lists to extend the known IDs to
! the ROPP/BUFR applications. The first pair should always be
! values for 'missing' or 'unknown' IDs.
!
! This file (roppbufrcodes.nl) should be installed to the
! directory path defined by environment variable BUFR_TABLES
! (for use with ECMWF BUFR library) and/or BUFR_LIBRARY for use
! with Met Office/MetDB BUFR library).
!---------------------------------------------------------------
!
! List of GNSS Tx Classes & their BUFR codes (Code Table 002020)
!
&GNScodes
GNSlist = "U",      "G", "R", "E"          ! [GPS, GLONASS, Galileo]
GNScode = -9999999, 401, 402, 403
/
!
! List of LEO Rx IDs & their BUFR codes (Code Table 001007 CCT C-5)
! and associated Instrument Type code   (Code Table 002019 CCT C-8)
! NB: MetOp-A .eqv. MetOp-2, -B <=> -1 and -C <=> -3.
!
&LEOcodes
LEOlist = "UNKN",  "OERS", "CHMP", "SUNS", "SACC", "GRAA", "GRAB",
                   "CO01", "CO02", "CO03", "CO04", "CO05", "CO06",
                   "META", "METB", "METC", "TSRX", "TDMX", "PAZE",
                   "OSAT", "CNOF"
LEOcode = -9999999, 040,    041,    800,    820,    722,    723,
                    740,    741,    742,    743,    744,    745,
                    004,    003,    005,    042,    043,    044,
                    421,    786
InsCode = -9999999, 102,    102,    102,    102,    102,    102,
                    102,    102,    102,    102,    102,    102,
                    202,    202,    202,    103,    103,    103,
                    287,    102
/
!
! List of (BUFR) Originating Centre IDs & their BUFR codes
! (Code Table 001033, CCT C-1, or 001035, CCT C-11)
! The (Processing) Sub-centre code should be valid for the
! associated Originating Centre code (for which Sub-Centre is 0).
! (Code Table 001034, CCT C-12)
! plus associated ICAO Location Indicator codes (for GTS routing headers)
! (ICAO Document 7910: Location Indicators)
!
&ORGcodes
ORGlist = "UNKNOWN", "DMI", "GFZ", "METO", "UCAR", "NESDIS", "EUMETSAT"
ORGcode = -9999999,   094,   078,   074,    060,     160,     254
Subcode = -9999999,   000,   173,   000,    000,     000,     000
ORGname = " ",
          "(ROM SAF)",
          "Helmholtz Centre, Potsdam",
          "Met Office, Exeter",
          "Boulder",
          "Washington"
          "Darmstadt"
ICAOcode = "ZZZZ",  "EKMI", "EDZW", "EGRR", "KWBC", "KNES", "EUMS"
/
!
! List of Background Generating centres & their BUFR codes.
! (Code Table 001033, CCT C-1, or 001033, CCT C-11)
!
&BGDcodes
BGDlist = "UNKNOWN", "ECMWF", "DMI", "METO", "NCEP"
BGDcode = -9999999,    098,    094,    074,    007
/
!
!End -----------------------------------------------------------
