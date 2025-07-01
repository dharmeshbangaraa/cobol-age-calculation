      ******************************************************************00010001
      *  REQUEST:   CSAGECAL - AGE CALCULATION MODULE                  *
      *                                                                *
      *  FUNCTION:  CALCULATE PASSENGER AGE FIELDS                     *
      *                                                                *
      *  UPDATES:   NONE                                               *
      *                                                                *
      *  INPUT:     INPUT FIELDS                                       *
      *                                                                *
      *  OUTPUT:    AGE FIELDS                                         *
      *                                                                *00040001
      *  USED WITH PROGRAM CSAGECAL                                    *00050001
      ******************************************************************00110001
      * - MUST POPULATE EITHER                                         *
      *   CSAGECAL-I-BIRTH-MMDDCCYY OR CSAGECAL-I-BIRTH-MMDDCCYY       *
      *                                                                *00060001
      * - MUST POPULATE EITHER                                         *
      *   CSAGECAL-I-DEPART-DATE-GREG OR CSAGECAL-I-DEPART-DATE-INTL   *
      *                                                                *00060001
      * - AGE-ACTUAL IS EITHER IN YEARS, EG, '034'                     *
      *   OR MONTHS, EG, '18M'                                         *00060001
      *                                                                *00060001
      * - POPULATE CALC-AGE-IN-MONTHS FROM CORRESPONDING PRF- FLAG     *
      *                                                                *00060001
      ******************************************************************00010001
      *    DATE     PROGRAMMER   DESCRIPTION OF CHANGE(S)              *00080001
      *  --------   ---------  --------------------------------------- *00090001
091005*  09/10/05   D. MORGAN  R10 DAT0296 RESTRICT INFANTS IN BOOKING
      ******************************************************************00110001
       01  CSAGECAL-PARMS.
           05  CSAGECAL-INPUT-AREA.
               10  CSAGECAL-I-BIRTH-MMDDCCYY   PIC S9(9) COMP-3.
               10  CSAGECAL-I-BIRTH-CCYYMMDD   PIC S9(9) COMP-3.
               10  CSAGECAL-I-DEPART-DATE-GREG PIC S9(7) COMP-3.
               10  CSAGECAL-I-DEPART-DATE-INTL PIC  X(7).
               10  CSAGECAL-I-CALC-AGE-IN-MONTHS
                                               PIC  9(3) COMP-3.
               10  FILLER                      PIC  X(10).

           05  CSAGECAL-OUTPUT-AREA.
               10  CSAGECAL-O-RETURN-CODE      PIC X(1).
                   88  CSAGECAL-O-RC-OK                 VALUE X'00' ' '.
                   88  CSAGECAL-O-RC-MISSING-INPUT      VALUE X'01'.
                   88  CSAGECAL-O-RC-DATE-ERROR         VALUE X'02'.
               10  CSAGECAL-O-AGE-ACTUAL       PIC X(3).
               10  CSAGECAL-O-AGE-ACTUAL-N REDEFINES
                   CSAGECAL-O-AGE-ACTUAL       PIC 9(3).
               10  CSAGECAL-O-AGE-YEARS        PIC 9(3).
               10  FILLER                      PIC X(10).
