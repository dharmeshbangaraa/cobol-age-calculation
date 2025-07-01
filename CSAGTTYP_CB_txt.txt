      ***************************************************************** 00010000
      *                                                               * 00020000
      *                         "CSAGTTYP"                            * 00030000
      *         CALLING PARAMETERS FOR AGENT FLAG SUBROUTINE.         * 00040010
      *                CRUISES RESERVATIONS SYSTEM                    * 00050000
      *                                                               * 00060000
      *                         LENGTH=36                             * 00070000
      *                                                               * 00080000
      ***************************************************************** 00090010
      * 02/15/02 - WEON    CREATED                                    * 00100010
      * 06/24/02 - WEON    MOON PROJECT. COMMENT OUT VALID-AGENT-COMPA* 00110010
      *                    NY                                         * 00120010
      * 08/11/09 - VIVIN   CPS0390 - VISITOR AGENCY                   * 00130010
      * 07/29/16 - WEON    DAT1386 POLAR VISION - CREAT A NEW WEB       00140010
      *                    BASED FRONT END                              00150010
      ***************************************************************** 00160010
                                                                        00170010
       01  CSAGTTYP-PARMS.                                              00180010
                                                                        00190010
           05  CSAGTTYP-INPUT-PARMS.                                    00200010
               10  CSAGTTYP-I-AGENT-KEY.                                00210010
                   15  CSAGTTYP-I-AGENT-COMPANY    PIC X(01).           00220010
062402*                88  CSAGTTYP-I-VALID-AGENT-COMPANY VALUE         00230010
062402*                                    '2' '3' '4' '5'.             00240010
                   15  CSAGTTYP-I-AGENT-NR         PIC X(08).           00250010
                       88  CSAGTTYP-I-NEW-AGENT          VALUE          00260010
                                           'NEWAGENT' 'NEWAGTMX'        00270010
072916                                     'NEWAGTUK' 'NEWAGTHA'        00280010
                                           'NEWAGTAU' 'NEWAGTNZ'.       00290010
               10  CSAGTTYP-I-TYPE-SALES-PROGRAM   PIC X(02).           00300010
               10  FILLER                          PIC X(10).           00310010
                                                                        00320010
           05  CSAGTTYP-OUTPUT-PARMS.                                   00330010
               10  CSAGTTYP-O-RETURN-CODE          PIC X(01).           00340010
                   88  CSAGTTYP-O-RC-OK                VALUE X'00'.     00350010
                   88  CSAGTTYP-O-RC-INVALID-INPUT     VALUE X'FF'.     00360010
               10  CSAGTTYP-O-AGENT-FLAG           PIC X(04).           00370010
                   88  CSAGTTYP-O-DIRECT               VALUE 'DIR '.    00380010
                   88  CSAGTTYP-O-NEW                  VALUE 'NEW '.    00390010
081109             88  CSAGTTYP-O-VISITOR              VALUE 'VIS '.    00400010
               10  FILLER                          PIC X(10).           00410010
