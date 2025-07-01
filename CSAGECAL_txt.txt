       PROCESS NODYNAM,RES,RENT                                         00010008
      *PROCESS NODYNAM,RES,NORENT                                       00020008
      ***************************************************************** 00030008
      *  NOTE - ONLINE COMPILE REQUIRES "RENT" PARAMETER                00040008
      *         BATCH MUST BE COMPILED "NORENT"                         00050008
      *                                                                 00060008
      *  IF YOU COMPILE THIS PGM WITH "GO", COMPILE AS A 'C'ICS SUBRTN  00070012
      ***************************************************************** 00080008
                                                                        00090008
       TITLE 'CSAGECAL - AGE CALCULATION ROUTINE'                       00100008
       IDENTIFICATION DIVISION.                                         00110008
       PROGRAM-ID.      CSAGECAL.                                       00120008
                                                                        00130008
      ***************************************************************** 00140008
      *                                                                 00150008
      *  VS/COBOL II - NO CICS COMMANDS                                 00160008
      *                                                                 00170008
      *  THIS PROGRAM CAN BE CALLED FROM BATCH OR FROM ONLINE.          00180008
      ***************************************************************** 00190008
      *  DATE      PROGRAMMER  DESCRIPTION OF CHANGE(S)                 00200008
      *  --------  ----------  ---------------------------------------- 00210008
091005*  09/10/05  D. MORGAN   R10 DAT0296 RESTRICT INFANTS IN BOOKING  00220008
093005*  09/30/05  D. MORGAN   JAVY-6GNJYN FIX AGE CALC IF < 1 YEAR     00221021
051104*  11/04/05  KURT        MWES-6HRRLL FIX MONTH CALC WHEN BIRTH    00222026
051104*                        MONTH = TO DEPART MONTH AND BIRTHDAY     00223026
051104*                        > SAIL DAY.                              00224026
060309*  06/03/09  MURSHID     JEGR-7SHJ55; FIX MONTHS CALC WHEN BIRTH  00225028
060309*                        MONTH = DEPART MONTH AND BIRTHDAY <=     00226028
060309*                        SAIL DAY.                                00227028
      ***************************************************************** 00230008
                                                                        00240008
      ***************************************************************** 00250008
      *          WORKING STORAGE SECTION                                00260008
      ***************************************************************** 00270008
                                                                        00280008
       DATA DIVISION.                                                   00290008
       WORKING-STORAGE SECTION.                                         00300008
                                                                        00310008
       01  WA-MISC-WORKAREA.                                            00320008
           05  WC-CSDATE                  PIC  X(8) VALUE 'CSDATE'.     00330021
093005*    05  WA-MONTHS                  PIC  9(2) VALUE ZEROS.        00341024
093005     05  WA-MONTHS                  PIC S9(2) VALUE ZEROS.        00342024
           05  WA-DATE-8                  PIC  9(8).                    00350008
           05  WA-BDAY-CCYYMMDD.                                        00360008
               10  WA-BDAY-CC             PIC  9(2) VALUE ZEROS.        00370008
               10  WA-BDAY-YYMMDD.                                      00380008
                   15  WA-BDAY-YY         PIC  9(2) VALUE ZERO.         00390008
                   15  WA-BDAY-MM         PIC  9(2) VALUE ZERO.         00400008
                   15  WA-BDAY-DD         PIC  9(2) VALUE ZERO.         00410008
           05  WA-BDAY-CCYYMMDD-N         REDEFINES                     00420008
               WA-BDAY-CCYYMMDD           PIC 9(08).                    00430008
           05 WA-BIRTH-YEAR               REDEFINES                     00440008
              WA-BDAY-CCYYMMDD-N          PIC  9(04).                   00450008
           05 WA-BASE-YEARS               PIC  9(03) VALUE ZEROS.       00460008
           05 WA-CALCULATED-AGE           PIC  9(03).                   00470008
           05 WA-DEPART-CCYYMMDD.                                       00480008
              10 WA-DEPART-CCYY           PIC  9(04).                   00490008
              10 WA-DEPART-MM             PIC  9(02).                   00500008
              10 WA-DEPART-DD             PIC  9(02).                   00510008
                                                                        00520008
       COPY CSDATE.                                                     00530008
                                                                        00540008
      ***************************************************************** 00550008
      *  LINKAGE SECTION                                                00560008
      ***************************************************************** 00570008
                                                                        00580008
       LINKAGE SECTION.                                                 00590008
                                                                        00600008
           COPY CSAGECAL.                                               00610008
                                                                        00620008
      ***************************************************************** 00630008
      *  PROCEDURE DIVISION                                             00640008
      ***************************************************************** 00650008
                                                                        00660008
       PROCEDURE DIVISION USING CSAGECAL-PARMS.                         00670008
                                                                        00680008
       00000-MAIN.                                                      00690008
                                                                        00700008
      * GET INPUT BIRTH DATE                                            00710008
           EVALUATE TRUE                                                00720008
           WHEN CSAGECAL-I-BIRTH-CCYYMMDD NUMERIC                       00730008
            AND CSAGECAL-I-BIRTH-CCYYMMDD > 0                           00731008
               MOVE CSAGECAL-I-BIRTH-CCYYMMDD                           00732008
                                          TO WA-BDAY-CCYYMMDD-N         00733008
           WHEN CSAGECAL-I-BIRTH-MMDDCCYY NUMERIC                       00734008
            AND CSAGECAL-I-BIRTH-MMDDCCYY > 0                           00735008
               MOVE CSAGECAL-I-BIRTH-MMDDCCYY                           00736009
                                          TO WA-DATE-8                  00737008
               MOVE WA-DATE-8 (1:2)       TO WA-BDAY-CCYYMMDD-N    (5:2)00738008
               MOVE WA-DATE-8 (3:2)       TO WA-BDAY-CCYYMMDD-N    (7:2)00739008
               MOVE WA-DATE-8 (5:2)       TO WA-BDAY-CCYYMMDD-N    (1:2)00740008
               MOVE WA-DATE-8 (7:2)       TO WA-BDAY-CCYYMMDD-N    (3:2)00750008
           WHEN OTHER                                                   00760008
               SET CSAGECAL-O-RC-MISSING-INPUT                          00770008
                                          TO TRUE                       00780008
               GO TO 00000-GOBACK                                       00790008
           END-EVALUATE                                                 00800008
                                                                        00810008
      * GET INPUT DEPART DATE                                           00820008
           MOVE LOW-VALUES                       TO CSDATE-PARMS        00830008
                                                                        00840008
           EVALUATE TRUE                                                00850008
           WHEN CSAGECAL-I-DEPART-DATE-INTL > SPACES                    00860008
               MOVE CSAGECAL-I-DEPART-DATE-INTL  TO CSDATE-INPUT-DATE   00870008
               SET CSDATE-FORMAT-INTL            TO TRUE                00880008
           WHEN CSAGECAL-I-DEPART-DATE-GREG NUMERIC                     00890008
            AND CSAGECAL-I-DEPART-DATE-GREG > 0                         00900008
               MOVE CSAGECAL-I-DEPART-DATE-GREG  TO CSDATE-INPUT-GREG   00910008
               SET CSDATE-FORMAT-GREG            TO TRUE                00920008
           WHEN OTHER                                                   00930008
               SET CSAGECAL-O-RC-MISSING-INPUT   TO TRUE                00940008
               GO TO 00000-GOBACK                                       00950008
           END-EVALUATE                                                 00960008
                                                                        00970008
           CALL WC-CSDATE  USING  CSDATE-PARMS                          00980008
                                                                        00990008
           IF  NOT CSDATE-RC-OK                                         01000008
               SET CSAGECAL-O-RC-DATE-ERROR TO TRUE                     01010008
               GO TO 00000-GOBACK                                       01020008
           END-IF                                                       01030008
                                                                        01040008
           MOVE CSDATE-OUT-CCYYMMDD         TO WA-DEPART-CCYYMMDD       01050008
                                                                        01060008
      *----------------------------------------------------------------*01070008
      * CALCULATE AGE = DEPART DATE MINUS BIRTHDATE                     01080008
      *----------------------------------------------------------------*01090008
                                                                        01100008
      * GET THE AGE IN YEARS                                            01110008
           SUBTRACT WA-BIRTH-YEAR            FROM WA-DEPART-CCYY        01120008
                                             GIVING WA-BASE-YEARS       01130008
                                                                        01140008
           IF  WA-DEPART-MM < WA-BDAY-MM                                01150008
               SUBTRACT 1                    FROM WA-BASE-YEARS         01160008
           END-IF                                                       01170008
                                                                        01180008
           IF  WA-DEPART-MM = WA-BDAY-MM                                01190008
           AND WA-DEPART-DD < WA-BDAY-DD                                01200008
               SUBTRACT 1                    FROM WA-BASE-YEARS         01210008
           END-IF                                                       01220008
                                                                        01230008
093005*    IF  WA-BASE-YEARS > CSAGECAL-I-CALC-AGE-IN-MONTHS            01240024
093005     IF  WA-BASE-YEARS >= CSAGECAL-I-CALC-AGE-IN-MONTHS           01241024
               MOVE WA-BASE-YEARS            TO CSAGECAL-O-AGE-ACTUAL-N 01250008
           ELSE                                                         01260008
               PERFORM 10000-CALCULATE-MONTHS THRU 10000-EXIT           01270008
           END-IF                                                       01280008
                                                                        01290008
           EVALUATE TRUE                                                01300008
           WHEN WA-BASE-YEARS < 1                                       01310008
               MOVE 1                        TO WA-BASE-YEARS           01320008
           WHEN WA-BASE-YEARS > 99                                      01330008
               MOVE 99                       TO WA-BASE-YEARS           01340008
           END-EVALUATE                                                 01350008
                                                                        01360008
           MOVE WA-BASE-YEARS                TO CSAGECAL-O-AGE-YEARS    01370008
           .                                                            01380008
       00000-GOBACK.                                                    01390008
                                                                        01400008
           GOBACK.                                                      01410008
                                                                        01420008
       10000-CALCULATE-MONTHS.                                          01430008
                                                                        01460008
060309     MOVE ZEROES                   TO WA-MONTHS.                  01461030
           IF  WA-DEPART-MM < WA-BDAY-MM                                01470008
               COMPUTE WA-MONTHS = (12 - WA-BDAY-MM)                    01480008
                     + WA-DEPART-MM                                     01490008
           ELSE                                                         01500008
051104         IF WA-DEPART-MM > WA-BDAY-MM                             01501026
                  COMPUTE WA-MONTHS                                     01510025
                     = WA-DEPART-MM - WA-BDAY-MM                        01520008
051104         ELSE                                                     01521026
060309            IF WA-DEPART-DD < WA-BDAY-DD                          01521128
051104               MOVE +12            TO WA-MONTHS                   01522028
060309            END-IF                                                01522128
051104         END-IF                                                   01523027
           END-IF                                                       01530008
                                                                        01540008
           IF  WA-DEPART-DD < WA-BDAY-DD                                01550008
               SUBTRACT 1                FROM WA-MONTHS                 01560008
           END-IF                                                       01570008
                                                                        01580008
093005     IF  WA-MONTHS     < 0                                        01581024
093005     AND WA-BASE-YEARS < 1                                        01582023
093005         MOVE 1                    TO WA-BASE-YEARS               01583023
093005     END-IF                                                       01584023
                                                                        01585023
           COMPUTE WA-MONTHS = (WA-BASE-YEARS * 12)                     01590008
                              + WA-MONTHS                               01600016
                                                                        01600816
      *   'CAP' THE MONTHS                                              01600920
           IF  WA-MONTHS >= (CSAGECAL-I-CALC-AGE-IN-MONTHS * 12)        01601019
               MOVE WA-BASE-YEARS        TO CSAGECAL-O-AGE-ACTUAL-N     01601119
           ELSE                                                         01601419
               MOVE WA-MONTHS            TO CSAGECAL-O-AGE-ACTUAL (1:2) 01601619
               MOVE 'M'                  TO CSAGECAL-O-AGE-ACTUAL (3:1) 01601719
           END-IF                                                       01601819
           .                                                            01620008
       10000-EXIT.                                                      01630008
           EXIT.                                                        01640008
