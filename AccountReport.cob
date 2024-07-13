*> -*- cobol -*- ************************************************************
*>
*>  System        :
*>  Module        :
*>  Object Name   : $RCSfile$
*>  Revision      : $Revision$
*>  Date          : $Date$
*>  Author        : $Author$
*>  Created By    : Robert Heller
*>  Created       : Fri Jul 5 11:11:31 2024
*>  Last Modified : <240713.1716>
*>
*>  Description
*>
*>  Notes
*>
*>  History
*>
*>***************************************************************************
*>
*>    Copyright (C) 2024  Robert Heller D/B/A Deepwoods Software
*>			51 Locke Hill Road
*>			Wendell, MA 01379-9728
*>
*>    This program is free software; you can redistribute it and/or modify
*>    it under the terms of the GNU General Public License as published by
*>    the Free Software Foundation; either version 2 of the License, or
*>    (at your option) any later version.
*>
*>    This program is distributed in the hope that it will be useful,
*>    but WITHOUT ANY WARRANTY; without even the implied warranty of
*>    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
*>    GNU General Public License for more details.
*>
*>    You should have received a copy of the GNU General Public License
*>    along with this program; if not, write to the Free Software
*>    Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.
*>
*> 
*>
*>***************************************************************************
IDENTIFICATION DIVISION.
  PROGRAM-ID. AccountReport.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
    FILE-CONTROL.
      SELECT Accounts ASSIGN TO 'Accounts.dat' 
        ORGANISATION IS RECORD BINARY SEQUENTIAL
        FILE STATUS IS AccountsStatus.
      SELECT Report-File ASSIGN TO '/tmp/accreport.txt'
                        LINE SEQUENTIAL.
      SELECT Sort-File ASSIGN TO DISK.
DATA DIVISION.
  FILE SECTION.
    COPY "AccountFileData.cbi" REPLACING ACC BY Accounts 
                               Record BY Account-Struct.
    FD Report-File
        REPORT IS Account-Report.
    SD SORT-FILE.
    01 SORT-REC.
        02 F-AccountName PICTURE IS X(16).
        02 F-AccountPennies PICTURE IS S9(8).
        02 F-AccountNumber PICTURE IS 99.
  WORKING-STORAGE SECTION.
    01 TodaysDate          PIC 9(8).
    01 AccountNumber       PIC 99.
    01 Flags.
        02 F-EOF   PIC X(1).
    01 One-Const   PIC 9 VALUE 1.
    01 Grand-Totals.
        02 Account-Count PIC 99.
        02 Total-Bank-Balance      BINARY-LONG.
    01 AccountsStatus PIC XX.
  REPORT SECTION.
    RD Account-Report
        CONTROLS ARE FINAL
        PAGE LIMITS ARE 59 LINES 132 COLUMNS
            HEADING 1
            FIRST DETAIL 5
            LAST  DETAIL 59.
    01 TYPE IS PAGE HEADING.
        05 LINE NUMBER PLUS 1.
            10 COL 1 SOURCE TodaysDate PIC 9999/99/99.
            10 COL 20 VALUE 'Piggy Bank Account Report'.
            10 COL 120 VALUE 'Page:'.
            10 COL 125 SOURCE PAGE-COUNTER PIC ZZ9.
        05 LINE NUMBER PLUS 2.
            10 COL 1 VALUE 'Account Name'.
            10 COL 18 VALUE 'A#'.
            10 COL 22 VALUE 'Balance'.
        05 LINE NUMBER PLUS 1.
            10 COL 1 VALUE '================'.
            10 COL 18 VALUE '=='.
            10 COL 22 VALUE '============'.
        
    01 Detail-Line TYPE IS DETAIL.
        05 LINE NUMBER PLUS 1.
            10 COL 1 SOURCE F-AccountName PIC X(16).
            10 COL 18 SOURCE F-AccountNumber PIC 99.
            10 COL 22 SOURCE F-AccountPennies PIC ZZ,ZZZ,ZZ9DB.

    01 End-Overall TYPE IS CONTROL FOOTING FINAL.
        05 LINE NUMBER PLUS 1.
            10 COL 22 VALUE '------------'.
        05 LINE NUMBER PLUS 1.
            10 COL 1 VALUE 'Account Count:'.
            10 COL 15 SOURCE Account-Count PIC Z9.
            10 COL 22 SOURCE Total-Bank-Balance PIC ZZ,ZZZ,ZZ9DB.
            
PROCEDURE DIVISION.
  010-Main SECTION.
  1. ACCEPT TodaysDate FROM DATE YYYYMMDD
     SORT SORT-FILE
        ASCENDING KEY F-AccountName
        INPUT PROCEDURE 100-Pre-Process-Data
        OUTPUT PROCEDURE 200-Generate-Report
     STOP RUN.

  100-Pre-Process-Data SECTION.
  1. OPEN INPUT Accounts
     MOVE 0 TO AccountNumber
     PERFORM FOREVER
       READ Accounts
       AT END
            EXIT PERFORM
       END-READ
       MOVE AccountNumber TO F-AccountNumber
       MOVE AccountName TO F-AccountName
       MOVE AccountPennies TO F-AccountPennies
       RELEASE SORT-REC
       ADD 1 TO AccountNumber
     END-PERFORM
     CLOSE Accounts
     .
  200-Generate-Report SECTION.
  1. INITIALIZE Flags  Grand-Totals
     OPEN OUTPUT Report-File
     INITIATE Account-Report
     RETURN SORT-FILE
     AT END
       MOVE 'Y' TO F-EOF
     END-RETURN
     PERFORM UNTIL F-EOF = 'Y'
       GENERATE Detail-Line
       ADD 1 TO Account-Count
       ADD F-AccountPennies TO Total-Bank-Balance
       RETURN SORT-FILE
       AT END
            MOVE 'Y' TO F-EOF
       END-RETURN
     END-PERFORM
     TERMINATE Account-Report
     CLOSE Report-File
     CALL "SYSTEM" USING "a2ps -q -1 -B --borders=no --underlay=test --landscape --lines-per-page=66 --chars-per-line=132 --prolog=greenbar -o - /tmp/accreport.txt|ps2pdf - accreport.pdf"
     .
END PROGRAM AccountReport.
