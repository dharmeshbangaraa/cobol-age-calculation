      ******************************************************************
      *  WHEN YOU COMPILE THIS PROGRAM,
      *  YOU SHOULD MAKE SURE TO GIVE THE VALUES BELOW ON THE 'GO' PANEL
      *      1) PROGRAM TYPE SHOULD BE "O".
      *      2) CICS/BATCH SUBRTN SHOULD BE "C".
      *
      *  THIS PROGRAM IS ONLY USED IN CICS.
      *
      ******************************************************************
       TITLE 'CSAGTTYP - AGENT TYPE SALES PROGRAM'.
       IDENTIFICATION DIVISION.
       PROGRAM-ID.      CSAGTTYP.

      *****************************************************************
      *
      *  VS/COBOL II - NO CICS COMMANDS
      *
      *  THIS PROGRAM CAN BE CALLED FROM BATCH OR ONLINE.
      *
      *  INPUT FIELDS ARE AS FOLLOWS:
      *    1) AGENT ID INCLUDING AGENT COMPANY CODE
      *    2) AGENT TYPE SALES PROGRAM
      *
      *  THE PROGRAM RETURNS THE 4-BYTE AGENT-FLAG TO BE MOVED TO
      *  CCA-H-AGENT-FLAG IN CALLING PROGRAMS.
      *
      *****************************************************************
      *  DATE      PROGRAMMER  DESCRIPTION OF CHANGE(S)
      *  --------  ----------  ----------------------------------------
      *  02/13/02  WEON        PROGRAM CREATED
062402*  06/24/02  WEON        COMMENT OUT NOT TO VALIDATE AGENT COMPANY
040403*  04/04/03  AIREEN      TREAT '7' AS '3' (PILOT FOR HAL)
      *  04/29/03  AIREEN      TREAT '8' AS '3' (PILOT FOR CARNIVAL)
042704*  04/27/04  BASHEER     1-2ICJS; DISPLAY AGENT TYPE FOR PA REGION
      *  07/14/04  AIREEN      1-2PCS9 - SETUP OF CUNARD PRODUCTION
010505*  01/05/05  P.AGUILAR   1-33BFQ - ADD NEW CU AGT TYPES.
111606*  11/16/06  REMA/       BDER-6VJL2L; CHANGE OF NAME FROM IEXCEL
111606*            MURSHID     AND IEXCEL GOLD TO EXCEL AND EXCEL GOLD
081109*  08/11/09  VIVIN       CPS0390 - VISITOR AGENCY
022614*  02/26/14  STONEKING   KGAD-9GMSY2: ADDED 5 NEW AGENT TYPES:
      *                        PM, SD, SH, SN, SS.
011421*  01/14/21  JANGEESH    PCS-1450; ADD NEW AGY TYPES FOR HAL
      *****************************************************************

      *****************************************************************
      *          WORKING STORAGE SECTION
      *****************************************************************

       DATA DIVISION.
       WORKING-STORAGE SECTION.

       01  WC-CONSTANTS.

           05  WC-STAR                   PIC X(04) VALUE '****'.
           05  WC-DIR                    PIC X(03) VALUE 'DIR'.
081109     05  WC-VIS                    PIC X(03) VALUE 'VIS'.
           05  WC-D                      PIC X(01) VALUE 'D'.
           05  WC-3                      PIC X(01) VALUE '3'.
           05  WC-4                      PIC X(01) VALUE '4'.
           05  WC-7                      PIC X(01) VALUE '7'.
           05  WC-8                      PIC X(01) VALUE '8'.
           05  WC-9                      PIC X(01) VALUE '9'.

      *****************************************************************
      *  TABLES
      *****************************************************************

       01  WT-TABLES.
042704*    05  WT-AGENT-FLAG-TABLE.
042704*        10  FILLER                PIC X(07) VALUE 'XG IXLG'.
042704*        10  FILLER                PIC X(07) VALUE 'XX IXL '.
042704*        10  FILLER                PIC X(07) VALUE 'KA KEY '.
042704*        10  FILLER                PIC X(07) VALUE 'HP HIP '.
042704*        10  FILLER                PIC X(07) VALUE 'FG SLCT'.
042704*        10  FILLER                PIC X(07) VALUE 'FS SLCT'.
042704*        10  FILLER                PIC X(07) VALUE 'FB SLCT'.
042704*        10  FILLER                PIC X(07) VALUE 'CC CUST'.
042704*        10  FILLER                PIC X(07) VALUE 'CD CUST'.
042704*        10  FILLER                PIC X(07) VALUE 'CG CUST'.
042704*        10  FILLER                PIC X(07) VALUE 'CI CUST'.
042704*        10  FILLER                PIC X(07) VALUE 'CO CUST'.
042704*        10  FILLER                PIC X(07) VALUE 'CW CUST'.
042704*        10  FILLER                PIC X(07) VALUE 'CT CUST'.
042704*
042704*    05  FILLER REDEFINES WT-AGENT-FLAG-TABLE.
042704*        10  WT-AGENT-TABLE
042704*                OCCURS 14 TIMES
042704*                ASCENDING KEY IS WT-TYPE-SALES-PROGRAM
042704*                INDEXED BY TYPE-IX.
042704*            15  WT-TYPE-SALES-PROGRAM PIC X(02).
042704*            15  FILLER                PIC X(01).
042704*            15  WT-AGENT-FLAG         PIC X(04).

042704     05  WT-AGENT-FLAG-TABLE.
111606*        10  FILLER                PIC X(09) VALUE '3 XG IXLG'.
111606*        10  FILLER                PIC X(09) VALUE '3 XX IXL '.
111606         10  FILLER                PIC X(09) VALUE '3 XG XLG '.
111606         10  FILLER                PIC X(09) VALUE '3 XX XL  '.
042704         10  FILLER                PIC X(09) VALUE '3 KA KEY '.
042704         10  FILLER                PIC X(09) VALUE '3 HP HIP '.
042704         10  FILLER                PIC X(09) VALUE '3 FG SLCT'.
042704         10  FILLER                PIC X(09) VALUE '3 FS SLCT'.
042704         10  FILLER                PIC X(09) VALUE '3 FB SLCT'.
042704         10  FILLER                PIC X(09) VALUE '3 CC CUST'.
042704         10  FILLER                PIC X(09) VALUE '3 CD CUST'.
042704         10  FILLER                PIC X(09) VALUE '3 CG CUST'.
042704         10  FILLER                PIC X(09) VALUE '3 CI CUST'.
042704         10  FILLER                PIC X(09) VALUE '3 CO CUST'.
042704         10  FILLER                PIC X(09) VALUE '3 CW CUST'.
042704         10  FILLER                PIC X(09) VALUE '3 CT CUST'.
042704         10  FILLER                PIC X(09) VALUE '4 P1 PRT1'.
042704         10  FILLER                PIC X(09) VALUE '4 P2 PRT2'.
010505         10  FILLER                PIC X(09) VALUE '9 IP ICP '.
010505         10  FILLER                PIC X(09) VALUE '9 IG ICG '.
010505         10  FILLER                PIC X(09) VALUE '9 IS ICS '.
010505         10  FILLER                PIC X(09) VALUE '9 IB ICB '.
022614         10  FILLER                PIC X(09) VALUE '3 PM PRM '.
022614         10  FILLER                PIC X(09) VALUE '3 SD STG '.
022614         10  FILLER                PIC X(09) VALUE '3 SH STG '.
022614         10  FILLER                PIC X(09) VALUE '3 SN STG '.
022614         10  FILLER                PIC X(09) VALUE '3 SS STG '.
011421         10  FILLER                PIC X(09) VALUE '7 PR PPPR'.
042704
042704     05  FILLER REDEFINES WT-AGENT-FLAG-TABLE.
042704         10  WT-AGENT-TABLE
010505*                OCCURS 16 TIMES
022614*                OCCURS 20 TIMES
011421*                OCCURS 25 TIMES
011421                 OCCURS 26 TIMES
042704                 ASCENDING KEY IS WT-TYPE-SALES-PROGRAM
042704                 INDEXED BY TYPE-IX.
042704             15  WT-AGENT-LOCATION     PIC X(01).
042704             15  FILLER                PIC X(01).
042704             15  WT-TYPE-SALES-PROGRAM PIC X(02).
042704             15  FILLER                PIC X(01).
042704             15  WT-AGENT-FLAG         PIC X(04).

      *****************************************************************
      *  LINKAGE SECTION
      *****************************************************************

       LINKAGE SECTION.

           COPY CSAGTTYP.

      *****************************************************************
      *  PROCEDURE DIVISION
      *****************************************************************

       PROCEDURE DIVISION USING CSAGTTYP-PARMS.

           SET  CSAGTTYP-O-RC-OK         TO TRUE.
           MOVE SPACES                   TO CSAGTTYP-O-AGENT-FLAG.

           PERFORM 10000-VALIDATE-INPUT  THRU 10000-EXIT.
           IF CSAGTTYP-O-RC-INVALID-INPUT
              GO TO 01000-RETURN
           END-IF.

           IF CSAGTTYP-I-NEW-AGENT
              SET CSAGTTYP-O-NEW         TO TRUE
              GO TO 01000-RETURN
           END-IF.

           IF CSAGTTYP-I-AGENT-COMPANY = WC-3 OR WC-4 OR WC-7 OR WC-8
                                              OR WC-9
              IF CSAGTTYP-I-AGENT-NR (1:3) = WC-DIR
                 SET CSAGTTYP-O-DIRECT   TO TRUE
                 GO TO 01000-RETURN
              END-IF
081109        IF CSAGTTYP-I-AGENT-NR (1:3) = WC-VIS
081109           SET CSAGTTYP-O-VISITOR  TO TRUE
081109           GO TO 01000-RETURN
081109        END-IF
           ELSE
              IF CSAGTTYP-I-AGENT-NR (1:1) = WC-D
                 SET CSAGTTYP-O-DIRECT   TO TRUE
                 GO TO 01000-RETURN
              END-IF
           END-IF.

           IF  (CSAGTTYP-I-AGENT-COMPANY NOT = WC-3)
           AND (CSAGTTYP-I-AGENT-COMPANY NOT = WC-4)
           AND (CSAGTTYP-I-AGENT-COMPANY NOT = WC-7)
           AND (CSAGTTYP-I-AGENT-COMPANY NOT = WC-8)
           AND (CSAGTTYP-I-AGENT-COMPANY NOT = WC-9)
              GO TO 01000-RETURN
           END-IF.

           IF CSAGTTYP-I-TYPE-SALES-PROGRAM = SPACES
              GO TO 01000-RETURN
           END-IF.

           SET TYPE-IX                   TO 1.
           SEARCH WT-AGENT-TABLE
             AT END MOVE SPACES          TO CSAGTTYP-O-AGENT-FLAG
             WHEN WT-TYPE-SALES-PROGRAM (TYPE-IX)
                                         = CSAGTTYP-I-TYPE-SALES-PROGRAM
042704            AND WT-AGENT-LOCATION (TYPE-IX)
042704                                = CSAGTTYP-I-AGENT-COMPANY
                  MOVE WT-AGENT-FLAG (TYPE-IX)
                                         TO CSAGTTYP-O-AGENT-FLAG
           END-SEARCH.

       01000-RETURN.

           IF CSAGTTYP-O-RC-INVALID-INPUT
              MOVE WC-STAR               TO CSAGTTYP-O-AGENT-FLAG
           END-IF.

           GOBACK.

       10000-VALIDATE-INPUT.

062402*    IF NOT CSAGTTYP-I-VALID-AGENT-COMPANY
      *       SET CSAGTTYP-O-RC-INVALID-INPUT TO TRUE
062402*    END-IF.

           IF CSAGTTYP-I-AGENT-NR NOT > SPACES
              SET CSAGTTYP-O-RC-INVALID-INPUT TO TRUE
           END-IF.

       10000-EXIT.
           EXIT.
      *****************  END OF SOURCE PROGRAM  ***********************
