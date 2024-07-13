*> -*- cobol -*- ************************************************************
*>
*>  System        :
*>  Module        :
*>  Object Name   : $RCSfile$
*>  Revision      : $Revision$
*>  Date          : $Date$
*>  Author        : $Author$
*>  Created By    : Robert Heller
*>  Created       : Fri Jul 5 11:11:12 2024
*>  Last Modified : <240713.1712>
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
  PROGRAM-ID. TransReport.
ENVIRONMENT DIVISION.
  INPUT-OUTPUT SECTION.
    FILE-CONTROL.
      SELECT Accounts ASSIGN TO 'Accounts.dat' 
        ORGANISATION IS RECORD BINARY SEQUENTIAL
        FILE STATUS IS AccountsStatus.
      SELECT Transactions ASSIGN TO 'Transactions.dat'
        ORGANISATION IS RECORD BINARY SEQUENTIAL
        ACCESS MODE IS SEQUENTIAL 
        FILE STATUS IS TransactionsStatus.
      SELECT Report-File ASSIGN TO '/tmp/report.txt'
                        LINE SEQUENTIAL.
      SELECT Sort-File ASSIGN TO DISK.
DATA DIVISION.
  FILE SECTION.
    COPY "AccountFileData.cbi" REPLACING ACC BY Accounts 
                               Record BY Account-Struct.
    COPY "TransactionFileData.cbi" REPLACING TRANS BY Transactions
                                             Record BY Transaction-Struct.
    FD Report-File
        REPORT IS Transaction-Report.
    SD SORT-FILE.
    01 SORT-REC.
        02 F-TransactionID.
            05 CDT-Year               PIC 9(4).
            05 CDT-Month              PIC 9(2). *> 01-12
            05 CDT-Day                PIC 9(2). *> 01-31
            05 CDT-Hour               PIC 9(2). *> 00-23
            05 CDT-Minutes            PIC 9(2). *> 00-59
            05 CDT-Seconds            PIC 9(2). *> 00-59
            05 CDT-Hundredths-Of-Secs PIC 9(2). *> 00-99
            05 CDT-GMT-Diff-Hours     PIC S9(2)
                                      SIGN LEADING SEPARATE.
            05 CDT-GMT-Diff-Minutes   PIC 9(2). *> 00 or 30
            05 FILLER                 PIC X(11).
        02 F-AccountNumber PICTURE IS 9(2).
        02 F-AmountOfPennies PICTURE IS S9(8).
  WORKING-STORAGE SECTION.
    01 AccountsStatus PICTURE IS XX.
    01 TransactionsStatus PICTURE IS XX.
    01 Account-Record.
       02 AccountName PICTURE IS X(16) VALUE SPACES.
       02 AccountPennies PICTURE IS S9(8) VALUE 0.
    01 AccountData OCCURS 100 TIMES.
       02 AccountName PICTURE IS X(16) VALUE SPACES.
       02 AccountPennies PICTURE IS S9(8) VALUE 0.
    01 TodaysDate          PIC 9(8).
    
    01 Current-Account.
        02 Transaction-Count PIC 9999.
        02 F-AccountName PIC X(16).
        02 Running-Account-Balance BINARY-LONG.
        
    01 Flags.
        02 F-EOF   PIC X(1).
        02 Account-Head-Flag PIC X(1).
        
    01 One-Const   PIC 9 VALUE 1.
    
    01 Grand-Totals.
        02 Account-Count PIC 99.
        02 Total-Bank-Balance      BINARY-LONG.

    01 LastAccountNumber PIC 99.

  REPORT SECTION.
    RD Transaction-Report
        CONTROLS ARE FINAL F-AccountNumber
        PAGE LIMITS ARE 59 LINES 132 COLUMNS
             HEADING 1
             FIRST DETAIL 5
             LAST  DETAIL 59.
    01 TYPE IS PAGE HEADING.
        05 LINE NUMBER PLUS 1.
            10 COL 1 SOURCE TodaysDate PIC 9999/99/99.
            10 COL 20 VALUE 'Piggy Bank Transaction Report'.
            10 COL 120 VALUE 'Page:'.
            10 COL 125 SOURCE PAGE-COUNTER PIC ZZ9.
      
    01 Head-Account TYPE IS CONTROL HEADING F-AccountNumber.
        05 LINE NUMBER PLUS 1.
           10 COL 5 VALUE 'Account #:'.
           10 COL 15 SOURCE F-AccountNumber PIC 99.
           10 COL 20 VALUE 'Account Name:'.
           10 COL 35 SOURCE F-AccountName PIC X(16).
        05 LINE NUMBER PLUS 1.
           10 COL 1 VALUE 'Transaction Date'.
           10 COL 32 VALUE 'Amount'.
           10 COL 46 VALUE 'Balance'.
        05 LINE NUMBER PLUS 1.
           10 COL 1  VALUE '==========================='.
           10 COL 32 VALUE '============'.
           10 COL 46 VALUE '============'.
           
    01 Detail-Line TYPE IS DETAIL.
        05 LINE NUMBER PLUS 1.
            10 COL 1 SOURCE CDT-Year PIC 9(4).
            10 COL 5 VALUE '-'.
            10 COL 6 SOURCE CDT-Month PIC 9(2).
            10 COL 8 VALUE '-'.
            10 COL 9 SOURCE CDT-Day PIC 9(2).
            10 COL 11 VALUE ' '.
            10 COL 12 SOURCE CDT-Hour PIC 9(2).
            10 COL 14 VALUE ':'.
            10 COL 15 SOURCE CDT-Minutes PIC 9(2).
            10 COL 17 VALUE ':'.
            10 COL 18 SOURCE CDT-Seconds PIC 9(2).
            10 COL 20 VALUE '.'.
            10 COL 21 SOURCE CDT-Hundredths-Of-Secs  PIC 9(2).
            10 COL 23 SOURCE CDT-GMT-Diff-Hours  PIC S9(2)
                                            SIGN LEADING SEPARATE.
            10 COL 26 SOURCE CDT-GMT-Diff-Minutes PIC 9(2).
            10 COL 32 SOURCE F-AmountOfPennies PIC ZZ,ZZZ,ZZ9DB.
            10 COL 46 SOURCE Running-Account-Balance PIC ZZ,ZZZ,ZZ9DB.

    01 End-Account TYPE IS CONTROL FOOTING F-AccountNumber.
        05 LINE NUMBER PLUS 1.
            10 COL 46 VALUE '------------'.
        05 LINE NUMBER PLUS 1.
            10 COL 1 VALUE 'Tranaction Count:'.
            10 COL 18 SOURCE Transaction-Count PIC Z(5)9.
            10 COL 46 SOURCE Running-Account-Balance PIC ZZ,ZZZ,ZZ9DB.

    01 End-Overall TYPE IS CONTROL FOOTING FINAL.
        05 LINE NUMBER PLUS 1.
            10 COL 46 VALUE '------------'.
        05 LINE NUMBER PLUS 1.
            10 COL 1 VALUE 'Account Count:'.
            10 COL 15 SOURCE Account-Count PIC Z9.
            10 COL 46 SOURCE Total-Bank-Balance PIC ZZ,ZZZ,ZZ9DB.

PROCEDURE DIVISION.
  DECLARATIVES.
  000-End-Account SECTION.
  USE BEFORE REPORTING End-Account.
  1. COMPUTE Total-Bank-Balance = Total-Bank-Balance + Running-Account-Balance
     ADD 1 TO Account-Count
     MOVE 'R' TO Account-Head-Flag
  .
  END DECLARATIVES.
  010-Main SECTION.
  1. ACCEPT TodaysDate FROM DATE YYYYMMDD
     SORT SORT-FILE
        ASCENDING KEY F-AccountNumber
        INPUT PROCEDURE 100-Pre-Process-Data
        OUTPUT PROCEDURE 200-Generate-Report
     STOP RUN.

     
  100-Pre-Process-Data SECTION.
  1.  OPEN INPUT Transactions
      PERFORM FOREVER
        READ Transactions
        AT END
            EXIT PERFORM
        END-READ
        MOVE Transaction-Struct TO SORT-REC
        RELEASE SORT-REC
      END-PERFORM
      CLOSE Transactions
      .
    
  200-Generate-Report SECTION.
  1.  PERFORM P100-OpenAccounts
      PERFORM P200-ReadAccounts
      INITIALIZE Flags Current-Account Grand-Totals
      OPEN OUTPUT Report-File
      INITIATE Transaction-Report
      RETURN SORT-FILE
      AT END 
        MOVE 'Y' TO F-EOF
      END-RETURN
      PERFORM UNTIL F-EOF = 'Y'
        IF Account-Head-Flag = 'R'
            MOVE 0 TO Running-Account-Balance
            MOVE ' ' TO Account-Head-Flag
        END-IF
        MOVE AccountName IN AccountData(1+F-AccountNumber) TO F-AccountName
        ADD F-AmountOfPennies TO Running-Account-Balance
        ADD 1 TO Transaction-Count
        GENERATE Detail-Line
        RETURN SORT-FILE
        AT END
            MOVE 'Y' TO F-EOF
        END-RETURN
      END-PERFORM
      TERMINATE Transaction-Report
      CLOSE Report-File
      CALL "SYSTEM" USING "a2ps -q -1 -B --borders=no --underlay=test --landscape --lines-per-page=66 --chars-per-line=132 --prolog=greenbar -o - /tmp/report.txt| ps2pdf - report.pdf"
      .
    
  P100-OpenAccounts.
>>D    DISPLAY "*** P100-OpenAccounts"  UPON STDERR
    OPEN INPUT Accounts
    IF AccountsStatus NOT = '00'
       OPEN OUTPUT Accounts
       IF AccountsStatus NOT = '00'
         DISPLAY 'Could not open or create Accounts file!' UPON STDERR
         STOP RUN
       END-IF
       CLOSE Accounts
       OPEN INPUT Accounts
    END-IF.

  P200-ReadAccounts.

>>D    DISPLAY "*** P200-ReadAccounts" UPON STDERR
    PERFORM WITH TEST BEFORE UNTIL AccountsStatus = '10'
      READ Accounts RECORD INTO Account-Record
      IF AccountsStatus = '10'
        EXIT PERFORM
      END-IF
>>D      DISPLAY "*** Account-Record is " Account-Record UPON STDERR
      MOVE CORRESPONDING Account-Record TO AccountData(1+LastAccountNumber)
>>D      DISPLAY "*** LastAccountNumber is " LastAccountNumber UPON STDERR
>>D      DISPLAY "*** AccountData(1+LastAccountNumber) is " AccountData(1+LastAccountNumber) UPON STDERR
      ADD 1 TO LastAccountNumber
    END-PERFORM
    CLOSE Accounts.
    
END PROGRAM TransReport. 
