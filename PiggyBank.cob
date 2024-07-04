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
    COPY "AccountFileData.cbi" REPLACING ACC BY Accounts 
                               Record BY Account-Struct.
    COPY "TransactionFileData.cbi" REPLACING TRANS BY Transactions
                                             Record BY Transaction-Struct.
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
        02 Pennies PICTURE IS S9(2) VALUE 0.
        02 TransType PICTURE IS X VALUE "D".
    01 AccountData OCCURS 100 TIMES.
       02 AccountName PICTURE IS X(16) VALUE SPACES.
       02 AccountPennies PICTURE IS S9(8) VALUE 0.
  SCREEN SECTION.
    01 Account-Login-Screen.
       02 VALUE "PIGGY BANK, Your Bank under your bed!" 
                        BLANK SCREEN                 LINE 1 COL 5.
       02 VALUE "Account Login Screen"               LINE 6 COL 35.
       02 VALUE "Account Name: "                     LINE 8 COL 10.
       02   Name-Input                               LINE 8 COL 25
                        PICTURE IS X(16) TO CurrentAccountName.
       02 VALUE "C - TO CONTINUE"                    LINE 11 COL 30.
       02 VALUE "Q - TO QUIT"                        LINE 12 COL 30.
       02 VALUE "ENTER RESPONSE"                     LINE 14 COL 30.
       02 RESPONSE-INPUT                             LINE 14 COL 45
                            PICTURE IS X TO MainAnswer.
    01 Transaction-Screen.       
       02 VALUE "PIGGY BANK, Your Bank under your bed!" 
                                      BLANK SCREEN  LINE 1 COL 5.
       02 VALUE "Transaction Screen"                LINE 3 COL 35.
       02 VALUE "Account Name: "                    LINE 4 COL 10.
       02 Account-Name                              LINE 4 COL 25
                    PICTURE IS X(16) FROM CurrentAccountName.    
       02 VALUE "Current Balance: "                 LINE 5 COL 10.
       02 Account-Balance                           LINE 5 COL 27
                    PICTURE IS ZZ,ZZZ,ZZ9DB FROM CurrentBalance.
       02 VALUE "Pennies"                           LINE 5 COL 40.   
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
    
P300-OpenTransactions.

>>D    DISPLAY "*** P300-OpenTransactions" UPON STDERR
    OPEN EXTEND Transactions
    IF TransactionsStatus NOT = '00'
      OPEN  OUTPUT Transactions
      IF TransactionsStatus NOT = '00'
        DISPLAY 'Could not open or create Transactions file!' UPON STDERR
        STOP RUN
      END-IF
    END-IF.
    
P400-MainScreen.

>>D    DISPLAY "*** P400-MainScreen" UPON STDERR
    MOVE SPACES TO CurrentAccountName
    MOVE " " TO MainAnswer
>>D    DISPLAY "*** CurrentAccountName: '" CurrentAccountName "'" UPON STDERR 
    MOVE SPACES TO Name-Input IN Account-Login-Screen
    MOVE " " TO RESPONSE-INPUT  IN Account-Login-Screen
    DISPLAY Account-Login-Screen
    ACCEPT Account-Login-Screen
>>D    DISPLAY "*** CurrentAccountName: '" CurrentAccountName "'" UPON STDERR 
    IF FUNCTION UPPER-CASE(MainAnswer) = "C"
      PERFORM WITH TEST BEFORE 
            VARYING CurrentAccountNumber FROM 0 UNTIL CurrentAccountNumber = LastAccountNumber
>>D        DISPLAY "*** CurrentAccountNumber is " CurrentAccountNumber UPON STDERR
        IF AccountName IN AccountData(1+CurrentAccountNumber) = CurrentAccountName
>>D          DISPLAY "*** Using existing account (#" CurrentAccountNumber ") for " CurrentAccountName UPON STDERR
          MOVE AccountPennies IN AccountData(1+CurrentAccountNumber) TO CurrentBalance
          PERFORM P500-TransactionScreen UNTIL FUNCTION UPPER-CASE(TransactionAnswer) = 'Q'
          PERFORM P600-ReWriteAccounts
          EXIT PARAGRAPH
        END-IF
      END-PERFORM
>>D      DISPLAY "*** LastAccountNumber is " LastAccountNumber UPON STDERR
      IF CurrentAccountNumber = LastAccountNumber
>>D        DISPLAY "*** Creating new account for " CurrentAccountName UPON STDERR
        MOVE SPACES TO AccountName IN AccountData(1+CurrentAccountNumber)
        MOVE 0 TO AccountPennies IN AccountData(1+CurrentAccountNumber)
>>D        DISPLAY "*** [Before] AccountData(1+" CurrentAccountNumber ") is '" AccountData(1+CurrentAccountNumber) "'" UPON STDERR
        MOVE CurrentAccountName TO AccountName IN AccountData(1+CurrentAccountNumber)
        MOVE 0 TO CurrentBalance
        ADD 1 TO LastAccountNumber
>>D        DISPLAY "*** CurrentAccountName is '" CurrentAccountName "'" UPON STDERR
>>D        DISPLAY "*** CurrentBalance is " CurrentBalance UPON STDERR
>>D        DISPLAY "*** [After] AccountData(1+" CurrentAccountNumber ") is '" AccountData(1+CurrentAccountNumber) "'" UPON STDERR
        PERFORM P500-TransactionScreen UNTIL FUNCTION UPPER-CASE(TransactionAnswer) = 'Q'
      END-IF
    END-IF
    PERFORM P600-ReWriteAccounts.
    
P500-TransactionScreen.

>>D    DISPLAY "*** P500-TransactionScreen" UPON STDERR
    
    MOVE 0 TO Pennies
    MOVE " " TO TransType
    MOVE " " TO TransactionAnswer
    MOVE " " TO Type-Input IN Transaction-Screen
    MOVE 0 TO Pennies-Input IN Transaction-Screen
    MOVE " " TO RESPONSE-INPUT IN Transaction-Screen
    DISPLAY Transaction-Screen
    ACCEPT Transaction-Screen
    IF  FUNCTION UPPER-CASE(TransactionAnswer) = 'C'
      MOVE FUNCTION CURRENT-DATE TO  Now
      MOVE Now TO TransactionID IN Transaction-Record
      MOVE CurrentAccountNumber TO AccountNumber IN Transaction-Record
      IF  FUNCTION UPPER-CASE(TransType) = 'D'
        ADD Pennies TO CurrentBalance
        MOVE Pennies TO AmountOfPennies IN Transaction-Record
      ELSE
        SUBTRACT Pennies FROM CurrentBalance
        MOVE 0 TO AmountOfPennies IN Transaction-Record
        SUBTRACT Pennies FROM AmountOfPennies IN Transaction-Record
      END-IF
>>D      DISPLAY "*** Transaction-Record is " Transaction-Record UPON STDERR
      MOVE CurrentBalance TO AccountPennies IN AccountData(1+CurrentAccountNumber)
      MOVE CORRESPONDING Transaction-Record TO Transaction-Struct
      WRITE Transaction-Struct
    END-IF.
    
P600-ReWriteAccounts.

>>D    DISPLAY "*** P600-ReWriteAccounts" UPON STDERR
    CLOSE Transactions
    OPEN OUTPUT Accounts
    PERFORM VARYING CurrentAccountNumber FROM 0 UNTIL CurrentAccountNumber = LastAccountNumber
>>D        DISPLAY "*** CurrentAccountNumber = " CurrentAccountNumber UPON STDERR
>>D        DISPLAY "*** AccountData(1+CurrentAccountNumber) is " AccountData(1+CurrentAccountNumber) UPON STDERR
        MOVE CORRESPONDING AccountData(1+CurrentAccountNumber) TO Account-Struct
>>D        DISPLAY "*** Account-Struct '" Account-Struct "'" UPON STDERR
        WRITE Account-Struct
    END-PERFORM.
    CLOSE Accounts.
.    
END PROGRAM PiggyBank.
