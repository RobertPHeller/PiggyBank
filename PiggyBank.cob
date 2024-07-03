IDENTIFICATION DIVISION.
  PROGRAM-ID. PiggyBank.
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
DATA DIVISION.
  FILE SECTION.
    FD Accounts.
    01 Account-Struct.
       02 AccountName PICTURE IS X(16).
       02 AccountPennies PICTURE IS S9(8).
    FD Transactions.
    01 Transaction-Struct.
       02 TransactionID PICTURE IS X(32).
       02 AccountNumber PICTURE IS 9(2).
       02 AmountOfPennies PICTURE IS S9(8).
  WORKING-STORAGE SECTION.
    01 AccountsStatus PICTURE IS XX.
    01 TransactionsStatus PICTURE IS XX.
    01 Account-Record.
       02 AccountName PICTURE IS X(16) VALUE SPACES.
       02 AccountPennies PICTURE IS S9(8) VALUE 0.
    01 Transaction-Record.
       02 TransactionID PICTURE IS X(32) VALUE SPACES.
       02 AccountNumber PICTURE IS 9(2) VALUE 0.
       02 AmountOfPennies PICTURE IS S9(8) VALUE 0.
    01 Now.
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
    01 MainAnswer PICTURE IS X VALUE "C".
    01 TransactionAnswer PICTURE IS X VALUE "C".
    01 CurrentAccountName PICTURE IS X(16) VALUE SPACES.
    01 CurrentAccountNumber PICTURE IS 9(2) VALUE 0.
    01 CurrentBalance PICTURE IS S9(8) VALUE 0.
    01 LastAccountNumber PICTURE IS 9(2) VALUE 0.
    01 Transaction-Entry.
        02 Pennies PICTURE IS 9(5) VALUE 0.
        02 TransType PICTURE IS X VALUE "D".
    01 AccountData OCCURS 100 TIMES.
       02 AccountName PICTURE IS X(16) VALUE SPACES.
       02 AccountPennies PICTURE IS S9(8) VALUE 0.
  SCREEN SECTION.
    01 Account-Login-Screen.
       02 VALUE "Account Login Screen" BLANK SCREEN  LINE 1 COL 35.
       02 VALUE "Account Name: "                     LINE 3 COL 10.
       02   Name-Input                               LINE 3 COL 25
                        PICTURE IS X(16) TO CurrentAccountName.
       02 VALUE "C - TO CONTINUE"                    LINE 11 COL 30.
       02 VALUE "Q - TO QUIT"                        LINE 12 COL 30.
       02 VALUE "ENTER RESPONSE"                     LINE 14 COL 30.
       02 RESPONSE-INPUT                             LINE 14 COL 45
                            PICTURE IS X TO MainAnswer.
    01 Transaction-Screen.       
       02 VALUE "Transaction Screen" BLANK SCREEN  LINE 1 COL 35.
       02 VALUE "Account Name: "                    LINE 3 COL 10.
       02 Account-Name                              LINE 3 COL 25
                    PICTURE IS X(16) FROM CurrentAccountName.    
       02 VALUE "Current Balance: "                 LINE 4 COL 10.
       02 Account-Balance                           LINE 4 COL 27
                    PICTURE IS $ZZ,ZZZ,ZZ9DB FROM CurrentBalance.   
       02 VALUE "Type (D for Deposit, W for Withdrawal): " LINE 7 COL 10.
       02   Type-Input                              LINE 7 COL 50
                            PICTURE IS X TO TransType IN Transaction-Entry.
       02 VALUE "Ammount: "                         LINE 8 COL 10.
       02   Pennies-Input                           LINE 8 COL 19
                            PICTURE IS ZZ TO Pennies IN Transaction-Entry.
       02 VALUE "C - TO CONTINUE"                    LINE 11 COL 30.
       02 VALUE "Q - TO QUIT"                        LINE 12 COL 30.
       02 VALUE "ENTER RESPONSE"                     LINE 14 COL 30.
       02 RESPONSE-INPUT                             LINE 14 COL 45
                            PICTURE IS X TO TransactionAnswer.
PROCEDURE DIVISION.
    PERFORM P100-OpenAccounts
    PERFORM P200-ReadAccounts
    PERFORM P300-OpenTransactions
    PERFORM P400-MainScreen UNTIL FUNCTION UPPER-CASE(MainAnswer) = 'Q'
    STOP Run.
P100-OpenAccounts.

    DISPLAY "*** P100-OpenAccounts"  UPON STDERR
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

    DISPLAY "*** P200-ReadAccounts" UPON STDERR
    PERFORM WITH TEST BEFORE UNTIL AccountsStatus = '10'
      READ Accounts RECORD INTO Account-Record
      IF AccountsStatus = '10'
        EXIT PERFORM
      END-IF
      DISPLAY "*** Account-Record is " Account-Record UPON STDERR
      MOVE CORRESPONDING Account-Record TO AccountData(LastAccountNumber)
      DISPLAY "*** LastAccountNumber is " LastAccountNumber UPON STDERR
      DISPLAY "*** AccountData(LastAccountNumber) is " AccountData(LastAccountNumber) UPON STDERR
      ADD 1 TO LastAccountNumber
    END-PERFORM
    CLOSE Accounts.
    
P300-OpenTransactions.

    DISPLAY "*** P300-OpenTransactions" UPON STDERR
    OPEN EXTEND Transactions
    IF TransactionsStatus NOT = '00'
      OPEN  OUTPUT Transactions
      IF TransactionsStatus NOT = '00'
        DISPLAY 'Could not open or create Transactions file!' UPON STDERR
        STOP RUN
      END-IF
    END-IF.
    
P400-MainScreen.

    DISPLAY "*** P400-MainScreen" UPON STDERR
    MOVE SPACES TO CurrentAccountName
    MOVE " " TO MainAnswer
    DISPLAY Account-Login-Screen
    ACCEPT Account-Login-Screen
    IF FUNCTION UPPER-CASE(MainAnswer) = "C"
      PERFORM WITH TEST BEFORE 
            VARYING CurrentAccountNumber FROM 0 UNTIL CurrentAccountNumber = LastAccountNumber
        DISPLAY "*** CurrentAccountNumber is " CurrentAccountNumber UPON STDERR
        IF AccountData(CurrentAccountNumber) = CurrentAccountName
          MOVE AccountPennies IN AccountData(CurrentAccountNumber) TO CurrentBalance
          PERFORM P500-TransactionScreen UNTIL TransactionAnswer = 'Q'
          EXIT PERFORM
        END-IF
      END-PERFORM
      DISPLAY "*** LastAccountNumber is " LastAccountNumber UPON STDERR
      IF CurrentAccountNumber = LastAccountNumber
        MOVE CurrentAccountName TO AccountName IN AccountData(CurrentAccountNumber)
        MOVE 0 TO CurrentBalance
        ADD 1 TO LastAccountNumber
        PERFORM P500-TransactionScreen UNTIL FUNCTION UPPER-CASE(TransactionAnswer) = 'Q'
      END-IF
    END-IF
    PERFORM P600-ReWriteAccounts.
    
P500-TransactionScreen.

    DISPLAY "*** P500-TransactionScreen" UPON STDERR
    
    MOVE 0 TO Pennies
    MOVE " " TO TransType
    MOVE " " TO TransactionAnswer
    DISPLAY Transaction-Screen
    ACCEPT Transaction-Screen
    IF  FUNCTION UPPER-CASE(TransactionAnswer) = 'C'
      MOVE FUNCTION CURRENT-DATE TO  Now
      MOVE Now TO TransactionID IN Transaction-Record
      MOVE CurrentAccountNumber TO AccountNumber IN Transaction-Record
      IF  FUNCTION UPPER-CASE(TransType) = 'D'
        COMPUTE CurrentBalance = CurrentBalance + Pennies
        MOVE Pennies TO AmountOfPennies IN Transaction-Record
      ELSE
        COMPUTE CurrentBalance = CurrentBalance - Pennies
        COMPUTE AmountOfPennies IN Transaction-Record = -Pennies
      END-IF
      MOVE CurrentBalance TO AccountPennies IN AccountData(CurrentAccountNumber)
      MOVE CORRESPONDING Transaction-Record TO Transaction-Struct
      WRITE Transaction-Struct
    END-IF.
    
P600-ReWriteAccounts.

    DISPLAY "*** P600-ReWriteAccounts" UPON STDERR
    CLOSE Transactions
    OPEN OUTPUT Accounts
    PERFORM VARYING CurrentAccountNumber FROM 0 UNTIL CurrentAccountNumber = LastAccountNumber
        DISPLAY "*** CurrentAccountNumber = " CurrentAccountNumber UPON STDERR
        DISPLAY "*** AccountData(CurrentAccountNumber) is " AccountData(CurrentAccountNumber) UPON STDERR
        MOVE CORRESPONDING AccountData(CurrentAccountNumber) TO Account-Struct
        WRITE Account-Struct
    END-PERFORM.
    CLOSE Accounts.
    
END PROGRAM PiggyBank.
